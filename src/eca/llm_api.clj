(ns eca.llm-api
  (:require
   [babashka.fs :as fs]
   [clojure.string :as string]
   [eca.config :as config]
   [eca.llm-providers.anthropic :as llm-providers.anthropic]
   [eca.llm-providers.azure]
   [eca.llm-providers.copilot]
   [eca.llm-providers.deepseek]
   [eca.llm-providers.ollama :as llm-providers.ollama]
   [eca.llm-providers.openai :as llm-providers.openai]
   [eca.llm-providers.openai-chat :as llm-providers.openai-chat]
   [eca.llm-providers.openrouter]
   [eca.llm-providers.z-ai]
   [eca.llm-util :as llm-util]
   [eca.logger :as logger]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[LLM-API]")

;; TODO ask LLM for the most relevant parts of the path
(defn refine-file-context [path lines-range]
  (cond
    (not (fs/exists? path))
    "File not found"

    (not (fs/readable? path))
    "File not readable"

    :else
    (let [content (slurp path)]
      (if lines-range
        (let [lines (string/split-lines content)
              start (dec (:start lines-range))
              end (min (count lines) (:end lines-range))]
          (string/join "\n" (subvec lines start end)))
        content))))

(defn default-model
  "Returns the default LLM model checking this waterfall:
  - defaultModel set
  - Anthropic api key set
  - Openai api key set
  - Github copilot login done
  - Ollama first model if running
  - Anthropic default model."
  [db config]
  (let [[decision model]
        (or (when-let [config-default-model (:defaultModel config)]
              [:config-default-model config-default-model])
            (when (llm-util/provider-api-key "anthropic" (get-in db [:auth "anthropic"]) config)
              [:api-key-found "anthropic/claude-sonnet-4-20250514"])
            (when (llm-util/provider-api-key "openai" (get-in db [:auth "openai"]) config)
              [:api-key-found "openai/gpt-5"])
            (when (get-in db [:auth "github-copilot" :api-key])
              [:api-key-found "github-copilot/gpt-4.1"])
            (when-let [ollama-model (first (filter #(string/starts-with? % config/ollama-model-prefix) (keys (:models db))))]
              [:ollama-running ollama-model])
            [:default "anthropic/claude-sonnet-4-20250514"])]
    (logger/info logger-tag (format "Default LLM model '%s' decision '%s'" model decision))
    model))

(defn ^:private tool->llm-tool [tool]
  (assoc (select-keys tool [:name :description :parameters])
         :type "function"))

(defn complete!
  [{:keys [provider model model-capabilities instructions user-messages config on-first-response-received
           on-message-received on-error on-prepare-tool-call on-tools-called on-reason on-usage-updated
           past-messages tools provider-auth]}]
  (let [first-response-received* (atom false)
        emit-first-message-fn (fn [& args]
                                (when-not @first-response-received*
                                  (reset! first-response-received* true)
                                  (apply on-first-response-received args)))
        on-message-received-wrapper (fn [& args]
                                      (apply emit-first-message-fn args)
                                      (apply on-message-received args))
        on-reason-wrapper (fn [& args]
                            (apply emit-first-message-fn args)
                            (apply on-reason args))
        on-prepare-tool-call-wrapper (fn [& args]
                                       (apply emit-first-message-fn args)
                                       (apply on-prepare-tool-call args))
        on-error-wrapper (fn [{:keys [exception] :as args}]
                           (when-not (:silent? (ex-data exception))
                             (logger/error args)
                             (on-error args)))
        tools (when (:tools model-capabilities)
                (mapv tool->llm-tool tools))
        reason? (:reason? model-capabilities)
        supports-image? (:image-input? model-capabilities)
        web-search (:web-search model-capabilities)
        max-output-tokens (:max-output-tokens model-capabilities)
        provider-config (get-in config [:providers provider])
        model-config (get-in provider-config [:models model])
        extra-payload (:extraPayload model-config)
        api-key (llm-util/provider-api-key provider provider-auth config)
        api-url (llm-util/provider-api-url provider config)
        provider-auth-type (:type provider-auth)
        callbacks {:on-message-received on-message-received-wrapper
                   :on-error on-error-wrapper
                   :on-prepare-tool-call on-prepare-tool-call-wrapper
                   :on-tools-called on-tools-called
                   :on-reason on-reason-wrapper
                   :on-usage-updated on-usage-updated}]
    (try
      (when-not api-url (throw (ex-info (format "API url not found.\nMake sure you have provider '%s' configured properly." provider) {})))
      (cond
        (= "openai" provider)
        (llm-providers.openai/completion!
         {:model model
          :instructions instructions
          :user-messages user-messages
          :max-output-tokens max-output-tokens
          :reason? reason?
          :supports-image? supports-image?
          :past-messages past-messages
          :tools tools
          :web-search web-search
          :extra-payload extra-payload
          :api-url api-url
          :api-key api-key
          :auth-type provider-auth-type}
         callbacks)

        (= "anthropic" provider)
        (llm-providers.anthropic/completion!
         {:model model
          :instructions instructions
          :user-messages user-messages
          :max-output-tokens max-output-tokens
          :reason? reason?
          :supports-image? supports-image?
          :past-messages past-messages
          :tools tools
          :web-search web-search
          :extra-payload extra-payload
          :api-url api-url
          :api-key api-key
          :auth-type provider-auth-type}
         callbacks)

        (= "github-copilot" provider)
        (llm-providers.openai-chat/completion!
         {:model model
          :instructions instructions
          :user-messages user-messages
          :max-output-tokens max-output-tokens
          :reason? reason?
          :supports-image? supports-image?
          :past-messages past-messages
          :tools tools
          :extra-payload extra-payload
          :api-url api-url
          :api-key api-key
          :extra-headers {"openai-intent" "conversation-panel"
                          "x-request-id" (str (random-uuid))
                          "vscode-sessionid" ""
                          "vscode-machineid" ""
                          "Copilot-Vision-Request" "true"
                          "copilot-integration-id" "vscode-chat"}}
         callbacks)

        (= "ollama" provider)
        (llm-providers.ollama/completion!
         {:api-url api-url
          :reason? (:reason? model-capabilities)
          :supports-image? supports-image?
          :model model
          :instructions instructions
          :user-messages user-messages
          :past-messages past-messages
          :tools tools
          :extra-payload extra-payload}
         callbacks)

        model-config
        (let [provider-fn (case (:api provider-config)
                            ("openai-responses"
                             "openai") llm-providers.openai/completion!
                            "anthropic" llm-providers.anthropic/completion!
                            "openai-chat" llm-providers.openai-chat/completion!
                            (on-error-wrapper {:message (format "Unknown model %s for provider %s" (:api provider-config) provider)}))
              url-relative-path (:completionUrlRelativePath provider-config)]
          (provider-fn
           {:model model
            :instructions instructions
            :user-messages user-messages
            :max-output-tokens max-output-tokens
            :reason? reason?
            :supports-image? supports-image?
            :past-messages past-messages
            :tools tools
            :extra-payload extra-payload
            :url-relative-path url-relative-path
            :api-url api-url
            :api-key api-key}
           callbacks))

        :else
        (on-error-wrapper {:message (format "ECA Unsupported model %s for provider %s" model provider)}))
      (catch Exception e
        (on-error-wrapper {:exception e})))))
