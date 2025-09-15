(ns eca.llm-providers.openai
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [eca.config :as config]
   [eca.features.login :as f.login]
   [eca.llm-util :as llm-util]
   [eca.logger :as logger]
   [hato.client :as http]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[OPENAI]")

(def ^:private responses-path "/v1/responses")

(defn ^:private base-completion-request! [{:keys [rid body api-url url-relative-path api-key on-error on-response]}]
  (let [url (str api-url (or url-relative-path responses-path))]
    (llm-util/log-request logger-tag rid url body)
    (http/post
     url
     {:headers {"Authorization" (str "Bearer " api-key)
                "Content-Type" "application/json"}
      :body (json/generate-string body)
      :throw-exceptions? false
      :async? true
      :as :stream}
     (fn [{:keys [status body]}]
       (try
         (if (not= 200 status)
           (let [body-str (slurp body)]
             (logger/warn logger-tag "Unexpected response status: %s body: %s" status body-str)
             (on-error {:message (format "OpenAI response status: %s body: %s" status body-str)}))
           (with-open [rdr (io/reader body)]
             (doseq [[event data] (llm-util/event-data-seq rdr)]
               (llm-util/log-response logger-tag rid event data)
               (on-response event data))))
         (catch Exception e
           (on-error {:exception e}))))
     (fn [e]
       (on-error {:exception e})))))

(defn ^:private normalize-messages [messages supports-image?]
  (keep (fn [{:keys [role content] :as msg}]
          (case role
            "tool_call" {:type "function_call"
                         :name (:name content)
                         :call_id (:id content)
                         :arguments (json/generate-string (:arguments content))}
            "tool_call_output"
            {:type "function_call_output"
             :call_id (:id content)
             :output (llm-util/stringfy-tool-result content)}
            "reason" {:type "reasoning"
                      :id (:id content)
                      :summary (if (string/blank? (:text content))
                                 []
                                 [{:type "summary_text"
                                   :text (:text content)}])
                      :encrypted_content (:external-id content)}
            (update msg :content (fn [c]
                                   (if (string? c)
                                     c
                                     (keep #(case (name (:type %))

                                              "text"
                                              (assoc % :type (if (= "user" role)
                                                               "input_text"
                                                               "output_text"))

                                              "image"
                                              (when supports-image?
                                                {:type "input_image"
                                                 :image_url (format "data:%s;base64,%s"
                                                                    (:media-type %)
                                                                    (:base64 %))})

                                              %)
                                           c))))))
        messages))

(defn completion! [{:keys [model user-messages instructions reason? supports-image? api-key api-url url-relative-path
                           max-output-tokens past-messages tools web-search extra-payload]}
                   {:keys [on-message-received on-error on-prepare-tool-call on-tools-called on-reason on-usage-updated]}]
  (let [input (concat (normalize-messages past-messages supports-image?)
                      (normalize-messages user-messages supports-image?))
        tools (cond-> tools
                web-search (conj {:type "web_search_preview"}))
        body (merge {:model model
                     :input input
                     :prompt_cache_key (str (System/getProperty "user.name") "@ECA")
                     :parallel_tool_calls true
                     :instructions instructions
                     :tools tools
                     :include (when reason?
                                ["reasoning.encrypted_content"])
                     :store false
                     :reasoning (when reason?
                                  {:effort "medium"
                                   :summary "detailed"})
                     :stream true
                     ;; :verbosity "medium"
                     :max_output_tokens max-output-tokens}
                    extra-payload)
        tool-call-by-item-id* (atom {})
        on-response-fn
        (fn handle-response [event data]
          (case event
            ;; text
            "response.output_text.delta"
            (on-message-received {:type :text
                                  :text (:delta data)})
            ;; tools
            "response.function_call_arguments.delta" (let [call (get @tool-call-by-item-id* (:item_id data))]
                                                       (on-prepare-tool-call {:id (:id call)
                                                                              :name (:name call)
                                                                              :arguments-text (:delta data)}))

            "response.output_item.done"
            (case (:type (:item data))
              "reasoning" (on-reason {:status :finished
                                      :id (-> data :item :id)
                                      :external-id (-> data :item :encrypted_content)})
              nil)

            ;; URL mentioned
            "response.output_text.annotation.added"
            (case (-> data :annotation :type)
              "url_citation" (on-message-received
                              {:type :url
                               :title (-> data :annotation :title)
                               :url (-> data :annotation :url)})
              nil)

            ;; reasoning / tools
            "response.reasoning_summary_text.delta"
            (on-reason {:status :thinking
                        :id (:item_id data)
                        :text (:delta data)})

            "response.reasoning_summary_text.done"
            (on-reason {:status :thinking
                        :id (:item_id data)
                        :text "\n"})

            "response.output_item.added"
            (case (-> data :item :type)
              "reasoning" (on-reason {:status :started
                                      :id (-> data :item :id)})
              "function_call" (let [call-id (-> data :item :call_id)
                                    item-id (-> data :item :id)
                                    function-name (-> data :item :name)
                                    function-args (-> data :item :arguments)]
                                (swap! tool-call-by-item-id* assoc item-id {:name function-name :id call-id})
                                (on-prepare-tool-call {:id call-id
                                                       :name function-name
                                                       :arguments-text function-args}))
              nil)

            ;; done
            "response.completed"
            (let [response (:response data)
                  tool-calls (keep (fn [{:keys [id call_id name arguments] :as output}]
                                     (when (= "function_call" (:type output))
                                       ;; Fallback case when the tool call was not prepared before when
                                       ;; some models/apis respond only with response.completed (skipping streaming).
                                       (when-not (get @tool-call-by-item-id* id)
                                         (swap! tool-call-by-item-id* assoc id {:name name :id call_id})
                                         (on-prepare-tool-call {:id call_id
                                                                :name name
                                                                :arguments-text arguments}))
                                       {:id call_id
                                        :item-id id
                                        :name name
                                        :arguments (json/parse-string arguments)}))
                                   (:output response))]
              (on-usage-updated (let [input-cache-read-tokens (-> response :usage :input_tokens_details :cached_tokens)]
                                  {:input-tokens (if input-cache-read-tokens
                                                   (- (-> response :usage :input_tokens) input-cache-read-tokens)
                                                   (-> response :usage :input_tokens))
                                   :output-tokens (-> response :usage :output_tokens)
                                   :input-cache-read-tokens input-cache-read-tokens}))
              (if (seq tool-calls)
                (let [{:keys [new-messages]} (on-tools-called tool-calls)
                      input (normalize-messages new-messages supports-image?)]
                  (base-completion-request!
                   {:rid (llm-util/gen-rid)
                    :body (assoc body :input input)
                    :api-url api-url
                    :url-relative-path url-relative-path
                    :api-key api-key
                    :on-error on-error
                    :on-response handle-response})
                  (doseq [tool-call tool-calls]
                    (swap! tool-call-by-item-id* dissoc (:item-id tool-call))))
                (on-message-received {:type :finish
                                      :finish-reason (-> data :response :status)})))

            "response.failed" (do
                                (when-let [error (-> data :response :error)]
                                  (on-error {:message (:message error)}))
                                (on-message-received {:type :finish
                                                      :finish-reason (-> data :response :status)}))
            nil))]
    (base-completion-request!
     {:rid (llm-util/gen-rid)
      :body body
      :api-url api-url
      :url-relative-path url-relative-path
      :api-key api-key
      :on-error on-error
      :on-response on-response-fn})))

(defmethod f.login/login-step ["openai" :login/start] [{:keys [db* chat-id provider send-msg!]}]
  (swap! db* assoc-in [:chats chat-id :login-provider] provider)
  (swap! db* assoc-in [:auth provider] {:step :login/waiting-api-key})
  (send-msg! "Paste your API Key"))

(defmethod f.login/login-step ["openai" :login/waiting-api-key] [{:keys [input db* provider send-msg!] :as ctx}]
  (if (string/starts-with? input "sk-")
    (do (config/update-global-config! {:providers {"openai" {:key input}}})
        (swap! db* dissoc :auth provider)
        (send-msg! (str "API key saved in " (.getCanonicalPath (config/global-config-file))))

        (f.login/login-done! ctx :update-cache? false))
    (send-msg! (format "Invalid API key '%s'" input))))
