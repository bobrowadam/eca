(ns eca.handlers-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [eca.config :as config]
   [eca.db :as db]
   [eca.features.tools :as f.tools]
   [eca.handlers :as handlers]
   [eca.models :as models]
   [eca.test-helper :as h]
   [matcher-combinators.test :refer [match?]]))

(h/reset-components-before-test)

(deftest initialize-test
  (testing "initializationOptions config is merged properly with default init config"
    (let [db* (atom {})]
      (with-redefs [models/sync-models! (constantly nil)
                    db/load-db-from-cache! (constantly nil)]
        (is (match?
             {}
             (handlers/initialize (h/components)
                                  {:initialization-options
                                   {:pureConfig true
                                    :providers {"github-copilot" {:key "123"
                                                                  :models {"gpt-5" {:a 1}}}}}})))
        (is (match?
             {:providers {"github-copilot" {:key "123"
                                            :models {"gpt-5" {:a 1}
                                                     "gpt-5-mini" {}}
                                            :url string?}}}
             (config/all @db*)))))))

(deftest chat-selected-behavior-changed-test
  (testing "Switching to behavior with defaultModel updates model"
    (h/reset-components!)
    (h/config! {:behavior {"custom" {:defaultModel "gpt-4.1"}}})

    (handlers/chat-selected-behavior-changed (h/components)
                                             {:behavior "custom"})
    (is (match? {:config-updated [{:chat {:select-model "gpt-4.1"}}]
                 :tool-server-update [{}]}
                (h/messages))))

  (testing "Switching to behavior without defaultModel uses global default"
    (h/reset-components!)
    (h/config! {:defaultModel "claude-opus-4"
                :behavior {"plan" {}}})
    (handlers/chat-selected-behavior-changed (h/components)
                                             {:behavior "plan"})
    (is (match? {:config-updated [{:chat {:select-model "claude-opus-4"}}]
                 :tool-server-update [{}]}
                (h/messages))))

  (testing "Switching behavior updates tool status"
    (h/reset-components!)
    (h/config! {:behavior {"plan" {:disabledTools ["eca_edit_file" "eca_write_file"]}}})
    (with-redefs [f.tools/native-tools (constantly [{:name "eca_edit_file"}
                                                    {:name "eca_read_file"}])]
      (handlers/chat-selected-behavior-changed (h/components)
                                               {:behavior "plan"})
      (is (match? {:tool-server-update [{:tools [{:name "eca_edit_file"
                                                  :disabled true}
                                                 {:name "eca_read_file"
                                                  :disabled false}]}]}
                  (h/messages)))))

  (testing "Switching to undefined behavior uses defaults"
    (h/reset-components!)
    (h/config! {:defaultModel "fallback-model"})
    (handlers/chat-selected-behavior-changed (h/components)
                                             {:behavior "nonexistent"})
    (is (match? {:config-updated [{:chat {:select-model "fallback-model"}}]
                 :tool-server-update [{}]}
                (h/messages)))))
