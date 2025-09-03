(ns eca.features.chat
  (:require
   [cheshire.core :as json]
   [clojure.set :as set]
   [clojure.string :as string]
   [eca.db :as db]
   [eca.features.commands :as f.commands]
   [eca.features.context :as f.context]
   [eca.features.index :as f.index]
   [eca.features.login :as f.login]
   [eca.features.prompt :as f.prompt]
   [eca.features.rules :as f.rules]
   [eca.features.tools :as f.tools]
   [eca.features.tools.mcp :as f.mcp]
   [eca.llm-api :as llm-api]
   [eca.logger :as logger]
   [eca.messenger :as messenger]
   [eca.shared :as shared :refer [assoc-some]]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[CHAT]")

(defn default-model [db config]
  (llm-api/default-model db config))

(defn ^:private send-content! [{:keys [messenger chat-id]} role content]
  (messenger/chat-content-received
   messenger
   {:chat-id chat-id
    :role role
    :content content}))

(defn finish-chat-prompt! [status {:keys [chat-id db* clear-history-after-finished?] :as chat-ctx}]
  (swap! db* assoc-in [:chats chat-id :status] status)
  (send-content! chat-ctx :system
                 {:type :progress
                  :state :finished})
  (if clear-history-after-finished?
    (swap! db* assoc-in [:chats chat-id :messages] [])
    (db/update-workspaces-cache! @db*)))

(defn ^:private assert-chat-not-stopped! [{:keys [chat-id db*] :as chat-ctx}]
  (when (identical? :stopping (get-in @db* [:chats chat-id :status]))
    (finish-chat-prompt! :idle chat-ctx)
    (logger/info logger-tag "Chat prompt stopped:" chat-id)
    (throw (ex-info "Chat prompt stopped" {:silent? true
                                           :chat-id chat-id}))))

;;; Helper functions for tool call state management

(defn ^:private get-tool-call-state
  "Get the complete state map for a specific tool call."
  [db chat-id tool-call-id]
  (get-in db [:chats chat-id :tool-calls tool-call-id]))

(defn ^:private get-active-tool-calls
  "Returns a map of tool calls that are still active.

  Active tool calls are those not in the following (terminal) states: :completed, :rejected, :stopped."
  [db chat-id]
  (->> (get-in db [:chats chat-id :tool-calls] {})
       (remove (fn [[_ state]]
                 (#{:completed :rejected :stopped} (:status state))))
       (into {})))

;;; Event-driven state machine for tool calls

(def ^:private tool-call-state-machine
  "State machine for tool call lifecycle management.

   Maps [current-status event] -> {:status new-status :actions [action-list]}

   Events:
   - :tool-prepare     - LLM preparing tool call (can happen multiple times)
   - :tool-run         - LLM ready to run tool call
   - :user-approve     - User approves tool call
   - :user-reject      - User rejects tool call
   - :send-reject      - A made-up event to cause a toolCallReject.  Used in a context where are the data is available.
   - :execution-start  - Tool call execution begins
   - :execution-end    - Tool call completes successfully
   - :stop-requested   - An event to request that active tool calls be stopped

   Actions:
   - send-* notifications
   - promise init & delivery
   - logging/metrics"
  {;; Note: transition-tool-call! treats no existing state as :initial state
   [:initial :tool-prepare]
   {:status :preparing
    :actions [:send-toolCallPrepare]}

   [:preparing :tool-prepare]
   {:status :preparing
    :actions [:send-toolCallPrepare]} ; Multiple prepares allowed

   [:preparing :tool-run]
   {:status :check-approval
    :actions [:init-approval-promise :send-toolCallRun]}

   [:check-approval :manual-approve]
   {:status :waiting-approval
    :actions [:send-progress]}

   [:check-approval :auto-approve]
   {:status :execution-approved
    :actions [:deliver-default-approval]}

   [:waiting-approval :user-approve]
   {:status :execution-approved
    :actions [:deliver-approval-true]}

   [:waiting-approval :user-reject]
   {:status :rejected
    :actions [:deliver-approval-false :log-rejection]}

   [:rejected :send-reject]
   {:status :rejected
    :actions [:send-toolCallRejected]}

   [:execution-approved :execution-start]
   {:status :executing
    :actions []}

   [:executing :execution-end]
   {:status :completed
    :actions [:send-toolCalled :record-metrics]}

   ;; And now all the :stop-requested transitions

   ;; TODO: In the future, when calls can be interrupted,
   ;; more states and actions will be required.

   [:execution-approved :stop-requested]
   {:status :stopped
    :actions [:send-toolCallRejected]}

   [:waiting-approval :stop-requested]
   {:status :stopped
    :actions [:deliver-approval-false]}

   [:check-approval :stop-requested]
   {:status :stopped
    :actions [:send-toolCallRejected]}

   [:preparing :stop-requested]
   {:status :stopped
    :actions [:send-toolCallRejected]}

   [:initial :stop-requested] ; Nothing sent yet, just mark as stopped
   {:status :stopped
    :actions []}})

(defn ^:private execute-action!
  "Execute a single action during state transition"
  [action db* chat-ctx tool-call-id event-data]
  (case action
    ;; Notification actions
    :send-progress
    (send-content! chat-ctx :system
                   {:type :progress
                    :state (:state event-data)
                    :text (:text event-data)})

    :send-toolCallPrepare
    (send-content! chat-ctx :assistant
                   (assoc-some
                    {:type :toolCallPrepare
                     :id tool-call-id
                     :name (:name event-data)
                     :origin (:origin event-data)
                     :arguments-text (:arguments-text event-data)}
                    :summary (:summary event-data)))

    :send-toolCallRun
    (send-content! chat-ctx :assistant
                   (assoc-some
                    {:type :toolCallRun
                     :id tool-call-id
                     :name (:name event-data)
                     :origin (:origin event-data)
                     :arguments (:arguments event-data)
                     :manual-approval (:manual-approval event-data)}
                    :details (:details event-data)
                    :summary (:summary event-data)))

    :send-toolCalled
    (send-content! chat-ctx :assistant
                   (assoc-some
                    {:type :toolCalled
                     :id tool-call-id
                     :origin (:origin event-data)
                     :name (:name event-data)
                     :arguments (:arguments event-data)
                     :error (:error event-data)
                     :outputs (:outputs event-data)}
                    :details (:details event-data)
                    :summary (:summary event-data)))

    :send-toolCallRejected
    (send-content! chat-ctx :assistant
                   (assoc-some
                    {:type :toolCallRejected
                     :id tool-call-id
                     :origin (:origin event-data)
                     :name (:name event-data)
                     :arguments (:arguments event-data)
                     :reason (:reason event-data :user)}
                    :details (:details event-data)
                    :summary (:summary event-data)))

    ;; State management actions
    :init-approval-promise
    (swap! db* assoc-in [:chats (:chat-id chat-ctx) :tool-calls tool-call-id :approved?*]
           (:approved?* event-data))

    :deliver-approval-false
    (deliver (get-in @db* [:chats (:chat-id chat-ctx) :tool-calls tool-call-id :approved?*])
             false)

    :deliver-approval-true
    (deliver (get-in @db* [:chats (:chat-id chat-ctx) :tool-calls tool-call-id :approved?*])
             true)

    :deliver-default-approval
    (deliver (get-in @db* [:chats (:chat-id chat-ctx) :tool-calls tool-call-id :approved?*])
             (:default-approval event-data))

    ;; Logging/metrics actions
    :log-rejection
    (logger/info logger-tag "Tool call rejected"
                 {:tool-call-id tool-call-id :reason (:reason event-data)})

    :record-metrics
    (logger/debug logger-tag "Tool call completed"
                  {:tool-call-id tool-call-id :duration (:duration event-data)})

    ;; Default case for unknown actions
    (logger/warn logger-tag "Unknown action" {:action action :tool-call-id tool-call-id})))

(defn ^:private transition-tool-call!
  "Execute an event-driven state transition for a tool call.

   Args:
   - db*: Database atom
   - chat-ctx: Chat context map with :chat-id, :request-id, :messenger
   - tool-call-id: Tool call identifier
   - event: Event keyword (e.g., :tool-prepare, :tool-run, :user-approve)
   - event-data: Optional map with event-specific data

   Returns: {:status new-status :actions actions-executed}

   Throws: ex-info if the transition is invalid for the current state"
  [db* chat-ctx tool-call-id event & [event-data]]
  (let [current-state (get-tool-call-state @db* (:chat-id chat-ctx) tool-call-id)
        current-status (:status current-state :initial) ; Default to :initial if no state
        transition-key [current-status event]
        {:keys [status actions]} (get tool-call-state-machine transition-key)]

    (when-not status
      (let [valid-events (map second (filter #(= current-status (first %))
                                             (keys tool-call-state-machine)))]
        (throw (ex-info "Invalid state transition"
                        {:current-status current-status
                         :event event
                         :tool-call-id tool-call-id
                         :valid-events valid-events}))))

    ;; Atomic status update
    (swap! db* assoc-in [:chats (:chat-id chat-ctx) :tool-calls tool-call-id :status] status)

    ;; Execute all actions sequentially
    (doseq [action actions]
      (execute-action! action db* chat-ctx tool-call-id event-data))

    {:status status :actions actions}))

(defn ^:private tool-name->origin [name all-tools]
  (:origin (first (filter #(= name (:name %)) all-tools))))

(defn ^:private usage-msg->usage
  "How this works:
    - tokens: the last message from API already contain the total
              tokens considered, but we save them for cost calculation
    - cost: we count the tokens in past requests done + current one"
  [{:keys [input-tokens output-tokens
           input-cache-creation-tokens input-cache-read-tokens]}
   full-model
   {:keys [chat-id db*]}]
  (when (and output-tokens input-tokens)
    (swap! db* update-in [:chats chat-id :total-input-tokens] (fnil + 0) input-tokens)
    (swap! db* update-in [:chats chat-id :total-output-tokens] (fnil + 0) output-tokens)
    (when input-cache-creation-tokens
      (swap! db* update-in [:chats chat-id :total-input-cache-creation-tokens] (fnil + 0) input-cache-creation-tokens))
    (when input-cache-read-tokens
      (swap! db* update-in [:chats chat-id :total-input-cache-read-tokens] (fnil + 0) input-cache-read-tokens))
    (let [db @db*
          total-input-tokens (get-in db [:chats chat-id :total-input-tokens] 0)
          total-input-cache-creation-tokens (get-in db [:chats chat-id :total-input-cache-creation-tokens] nil)
          total-input-cache-read-tokens (get-in db [:chats chat-id :total-input-cache-read-tokens] nil)
          total-output-tokens (get-in db [:chats chat-id :total-output-tokens] 0)
          model-capabilities (get-in db [:models full-model])]
      (assoc-some {:session-tokens (+ input-tokens
                                      (or input-cache-read-tokens 0)
                                      (or input-cache-creation-tokens 0)
                                      output-tokens)}
                  :limit (:limit model-capabilities)
                  :last-message-cost (shared/tokens->cost input-tokens input-cache-creation-tokens input-cache-read-tokens output-tokens model-capabilities)
                  :session-cost (shared/tokens->cost total-input-tokens total-input-cache-creation-tokens total-input-cache-read-tokens total-output-tokens model-capabilities)))))

(defn ^:private tokenize-args [^String s]
  (if (string/blank? s)
    []
    (->> (re-seq #"\s*\"([^\"]*)\"|\s*([^\s]+)" s)
         (map (fn [[_ quoted unquoted]] (or quoted unquoted)))
         (vec))))

(defn ^:private message->decision [message]
  (let [slash? (string/starts-with? message "/")]
    (if slash?
      (let [command (subs message 1)
            tokens (let [toks (tokenize-args command)] (if (seq toks) toks [""]))
            first-token (first tokens)
            args (vec (rest tokens))]
        (if (and first-token (string/includes? first-token ":"))
          (let [[server prompt] (string/split first-token #":" 2)]
            {:type :mcp-prompt
             :server server
             :prompt prompt
             :args args})
          {:type :eca-command
           :command first-token
           :args args}))
      {:type :prompt-message
       :message message})))

(defn ^:private prompt-messages!
  [user-messages
   clear-history-after-finished?
   {:keys [db* config chat-id contexts behavior full-model instructions messenger] :as chat-ctx}]
  (when (seq contexts)
    (send-content! chat-ctx :system {:type :progress
                                     :state :running
                                     :text "Parsing given context"}))
  (let [db @db*
        chat-ctx (assoc chat-ctx :clear-history-after-finished? clear-history-after-finished?)
        [provider model] (string/split full-model #"/" 2)
        past-messages (get-in db [:chats chat-id :messages] [])
        all-tools (f.tools/all-tools behavior @db* config)
        received-msgs* (atom "")
        received-thinking* (atom "")
        add-to-history! (fn [msg]
                          (swap! db* update-in [:chats chat-id :messages] (fnil conj []) msg))]
    (when-let [expires-at (get-in db [:auth provider :expires-at])]
      (when (<= (long expires-at) (quot (System/currentTimeMillis) 1000))
        (send-content! chat-ctx :system {:type :progress
                                         :state :running
                                         :text "Renewing auth token"})
        (f.login/renew-auth! provider chat-ctx
                             {:on-error (fn [error-msg]
                                          (send-content! chat-ctx :system {:type :text
                                                                           :text error-msg})
                                          (finish-chat-prompt! :idle chat-ctx)
                                          (throw (ex-info "Auth token renew failed" {})))})))

    (send-content! chat-ctx :system {:type :progress
                                     :state :running
                                     :text "Waiting model"})
    (llm-api/complete!
     {:model model
      :provider provider
      :model-capabilities (get-in db [:models full-model])
      :user-messages user-messages
      :instructions instructions
      :past-messages past-messages
      :config config
      :tools all-tools
      :provider-auth (get-in @db* [:auth provider])
      :on-first-response-received (fn [& _]
                                    (assert-chat-not-stopped! chat-ctx)
                                    (doseq [message user-messages]
                                      (add-to-history! message))
                                    (send-content! chat-ctx :system {:type :progress
                                                                     :state :running
                                                                     :text "Generating"}))
      :on-usage-updated (fn [usage]
                          (when-let [usage (usage-msg->usage usage full-model chat-ctx)]
                            (send-content! chat-ctx :system
                                           (merge {:type :usage}
                                                  usage))))
      :on-message-received (fn [{:keys [type] :as msg}]
                             (assert-chat-not-stopped! chat-ctx)
                             (case type
                               :text (do
                                       (swap! received-msgs* str (:text msg))
                                       (send-content! chat-ctx :assistant {:type :text
                                                                           :text (:text msg)}))
                               :url (send-content! chat-ctx :assistant {:type :url
                                                                        :title (:title msg)
                                                                        :url (:url msg)})
                               :limit-reached (do
                                                (send-content! chat-ctx :system
                                                               {:type :text
                                                                :text (str "API limit reached. Tokens: " (json/generate-string (:tokens msg)))})

                                                (finish-chat-prompt! :idle chat-ctx))
                               :finish (do
                                         (add-to-history! {:role "assistant" :content [{:type :text :text @received-msgs*}]})
                                         (finish-chat-prompt! :idle chat-ctx))))
      :on-prepare-tool-call (fn [{:keys [id name arguments-text]}]
                              (assert-chat-not-stopped! chat-ctx)
                              (transition-tool-call! db* chat-ctx id :tool-prepare
                                                     {:name name
                                                      :origin (tool-name->origin name all-tools)
                                                      :arguments-text arguments-text
                                                      :summary (f.tools/tool-call-summary all-tools name nil)}))
      ;; TODO: Should this be :on-tool-called instead for consistency?
      :on-tools-called (fn [tool-calls]
                         (assert-chat-not-stopped! chat-ctx)
                         ;; Flush any pending assistant text once before processing multiple tool calls
                         (when-not (string/blank? @received-msgs*)
                           (add-to-history! {:role "assistant" :content [{:type :text :text @received-msgs*}]})
                           (reset! received-msgs* ""))
                         (let [calls (doall
                                      (for [{:keys [id name arguments] :as tool-call} tool-calls]
                                        (let [approved?* (promise)
                                              details (f.tools/tool-call-details-before-invocation name arguments)
                                              summary (f.tools/tool-call-summary all-tools name arguments)
                                              origin (tool-name->origin name all-tools)
                                              approval (f.tools/approval all-tools name arguments db config)
                                              ask? (= :ask approval)]
                                          ;; Inform client the tool is about to run and store approval promise
                                          (transition-tool-call! db* chat-ctx id :tool-run
                                                                 {:approved?* approved?*
                                                                  :name name
                                                                  :origin (tool-name->origin name all-tools)
                                                                  :arguments arguments
                                                                  :manual-approval ask?
                                                                  :details details
                                                                  :summary summary})
                                          (if ask?
                                            (transition-tool-call! db* chat-ctx id :manual-approve
                                                                   {:state :running
                                                                    :text "Waiting for tool call approval"})
                                            (transition-tool-call! db* chat-ctx id :auto-approve
                                                                   {:default-approval (= :allow approval)}))
                                          ;; Execute each tool call concurrently
                                          (future
                                            (if @approved?* ;TODO: Should there be a timeout here?  If so, what would be the state transitions?
                                              ;; assert: In :execution-approved state
                                              (do
                                                (assert-chat-not-stopped! chat-ctx)
                                                (transition-tool-call! db* chat-ctx id :execution-start)
                                                (let [result (f.tools/call-tool! name arguments @db* config messenger behavior)
                                                      details (f.tools/tool-call-details-after-invocation name arguments details result)]
                                                  (add-to-history! {:role "tool_call" :content (assoc tool-call
                                                                                                      :details details
                                                                                                      :summary summary
                                                                                                      :origin origin)})
                                                  (add-to-history! {:role "tool_call_output" :content (assoc tool-call
                                                                                                             :error (:error result)
                                                                                                             :output result
                                                                                                             :details details
                                                                                                             :summary summary
                                                                                                             :origin origin)})
                                                  (transition-tool-call! db* chat-ctx id :execution-end
                                                                         {:origin origin
                                                                          :name name
                                                                          :arguments arguments
                                                                          :error (:error result)
                                                                          :outputs (:contents result)
                                                                          :details details
                                                                          :summary summary})))
                                              ;; assert: In :rejected state
                                              (let [[reject-reason reject-text] (if (= :deny approval)
                                                                                  [:user-config "Tool call denied by user config"]
                                                                                  [:user-choice "Tool call rejected by user choice"])]
                                                (add-to-history! {:role "tool_call" :content tool-call})
                                                (add-to-history! {:role "tool_call_output"
                                                                  :content (assoc tool-call :output {:error true
                                                                                                     :contents [{:text reject-text
                                                                                                                 :type :text}]})})
                                                (transition-tool-call! db* chat-ctx id :send-reject
                                                                       {:origin origin
                                                                        :name name
                                                                        :arguments arguments
                                                                        :reason reject-reason
                                                                        :details details
                                                                        :summary summary})))))))]
                           (assert-chat-not-stopped! chat-ctx)
                           ;; Wait for all tool calls to complete before returning
                           (run! deref calls)
                           (send-content! chat-ctx :system {:type :progress :state :running :text "Generating"})
                           {:new-messages (get-in @db* [:chats chat-id :messages])}))
      :on-reason (fn [{:keys [status id text external-id]}]
                   (assert-chat-not-stopped! chat-ctx)
                   (case status
                     :started (send-content! chat-ctx :assistant
                                             {:type :reasonStarted
                                              :id id})
                     :thinking (do
                                 (swap! received-thinking* str text)
                                 (send-content! chat-ctx :assistant
                                                {:type :reasonText
                                                 :id id
                                                 :text text}))
                     :finished (do
                                 (add-to-history! {:role "reason" :content {:id id
                                                                            :external-id external-id
                                                                            :text @received-thinking*}})
                                 (send-content! chat-ctx :assistant
                                                {:type :reasonFinished
                                                 :id id}))
                     nil))
      :on-error (fn [{:keys [message exception]}]
                  (send-content! chat-ctx :system
                                 {:type :text
                                  :text (or message (str "Error: " (ex-message exception)))})
                  (finish-chat-prompt! :idle chat-ctx))})))

(defn ^:private send-mcp-prompt!
  [{:keys [prompt args]}
   {:keys [db*] :as chat-ctx}]
  (let [{:keys [arguments]} (first (filter #(= prompt (:name %)) (f.mcp/all-prompts @db*)))
        args-vals (zipmap (map :name arguments) args)
        {:keys [messages error-message]} (f.prompt/get-prompt! prompt args-vals @db*)]
    (if error-message
      (send-content! chat-ctx :system
                     {:type :text
                      :text error-message})
      (prompt-messages! messages false chat-ctx))))

(defn ^:private message-content->chat-content [role message-content]
  (case role
    ("user"
     "system"
     "assistant") (reduce
                   (fn [m content]
                     (case (:type content)
                       :text (assoc m
                                    :type :text
                                    :text (str (:text m) "\n" (:text content)))
                       m))
                   {}
                   message-content)
    "tool_call" {:type :toolCallPrepare
                 :origin (:origin message-content)
                 :name (:name message-content)
                 :arguments-text ""
                 :id (:id message-content)}
    "tool_call_output" {:type :toolCalled
                        :origin (:origin message-content)
                        :name (:name message-content)
                        :arguments (:arguments message-content)
                        :error (:error message-content)
                        :id (:id message-content)
                        :outputs (:contents (:output message-content))}
    "reason" {:id (:id message-content)
              :external-id (:external-id message-content)
              :text (:text message-content)}))

(defn ^:private handle-command! [{:keys [command args]} chat-ctx]
  (let [{:keys [type] :as result} (f.commands/handle-command! command args chat-ctx)]
    (case type
      :chat-messages (do
                       (doseq [[chat-id messages] (:chats result)]
                         (doseq [message messages]
                           (send-content! (assoc chat-ctx :chat-id chat-id)
                                          (:role message)
                                          (message-content->chat-content (:role message) (:content message)))))
                       (finish-chat-prompt! :idle chat-ctx))
      :new-chat-status (finish-chat-prompt! (:status result) chat-ctx)
      :send-prompt (prompt-messages! [{:role "user" :content (:prompt result)}] (:clear-history-after-finished? result) chat-ctx)
      nil)))

(defn prompt
  [{:keys [message model behavior contexts chat-id]}
   db*
   messenger
   config]
  (let [message (string/trim message)
        chat-id (or chat-id
                    (let [new-id (str (random-uuid))]
                      (swap! db* assoc-in [:chats new-id] {:id new-id})
                      new-id))
        db @db*
        full-model (or model (default-model db config))
        rules (f.rules/all config (:workspace-folders db))
        refined-contexts (f.context/raw-contexts->refined contexts db config)
        repo-map* (delay (f.index/repo-map db config {:as-string? true}))
        instructions (f.prompt/build-instructions refined-contexts
                                                  rules
                                                  repo-map*
                                                  (or behavior
                                                      (-> config :chat :defaultBehavior) ;; legacy
                                                      (-> config :defaultBehavior))
                                                  config)
        chat-ctx {:chat-id chat-id
                  :contexts contexts
                  :behavior behavior
                  :instructions instructions
                  :full-model full-model
                  :db* db*
                  :config config
                  :messenger messenger}
        decision (message->decision message)]
    (swap! db* assoc-in [:chats chat-id :status] :running)
    (send-content! chat-ctx :user {:type :text
                                   :text (str message "\n")})
    (case (:type decision)
      :mcp-prompt (send-mcp-prompt! decision chat-ctx)
      :eca-command (handle-command! decision chat-ctx)
      :prompt-message (prompt-messages! [{:role "user" :content [{:type :text :text message}]}] false chat-ctx))
    {:chat-id chat-id
     :model full-model
     :status :prompting}))

(defn tool-call-approve [{:keys [chat-id tool-call-id request-id]} db* messenger]
  (let [chat-ctx {;; What else is needed?
                  :chat-id chat-id
                  :db* db*
                  :request-id request-id
                  :messenger messenger}]
    (transition-tool-call! db* chat-ctx tool-call-id :user-approve)))

(defn tool-call-reject [{:keys [chat-id tool-call-id request-id]} db* messenger]
  (let [chat-ctx {;; What else is needed?
                  :chat-id chat-id
                  :db* db*
                  :request-id request-id
                  :messenger messenger}]
    (transition-tool-call! db* chat-ctx tool-call-id :user-reject)))

(defn query-context
  [{:keys [query contexts chat-id]}
   db*
   config]
  {:chat-id chat-id
   :contexts (set/difference (set (f.context/all-contexts query db* config))
                             (set contexts))})

(defn query-commands
  [{:keys [query chat-id]}
   db*
   config]
  (let [query (string/lower-case query)
        commands (f.commands/all-commands @db* config)
        commands (if (string/blank? query)
                   commands
                   (filter #(or (string/includes? (string/lower-case (:name %)) query)
                                (string/includes? (string/lower-case (:description %)) query))
                           commands))]
    {:chat-id chat-id
     :commands commands}))

(defn prompt-stop
  [{:keys [chat-id]} db* messenger]
  (when (identical? :running (get-in @db* [:chats chat-id :status]))
    (let [chat-ctx {:chat-id chat-id
                    :db* db*
                    :messenger messenger}]
      (send-content! chat-ctx :system {:type :text
                                       :text "\nPrompt stopped"})

      ;; Handle each active tool call
      (doseq [[tool-call-id _] (get-active-tool-calls @db* chat-id)]
        (transition-tool-call! db* chat-ctx tool-call-id :stop-requested))
      (finish-chat-prompt! :stopping chat-ctx))))

(defn delete-chat
  [{:keys [chat-id]} db*]
  (swap! db* update :chats dissoc chat-id)
  (db/update-workspaces-cache! @db*))
