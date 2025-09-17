(ns eca.test-helper
  (:require
   [clojure.string :as string]
   [clojure.test :refer [use-fixtures]]
   [eca.config :as config]
   [eca.db :as db]
   [eca.messenger :as messenger]
   [eca.metrics :as metrics]
   [eca.shared :as shared]))

(def windows? (string/starts-with? (System/getProperty "os.name") "Windows"))

(defn file-path [path]
  (cond-> path windows?
          (-> (string/replace-first #"^/" "C:\\\\")
              (->> (re-matches #"(.+?)(\.jar:.*)?"))
              (update 1 string/replace "/" "\\")
              rest
              (->> (apply str)))))

(defn file-uri [uri]
  (cond-> uri windows?
          (string/replace #"^(file):///(?!\w:/)" "$1:///C:/")))

(defrecord TestMessenger [messages* diagnostics*]
  messenger/IMessenger
  (chat-content-received [_ data] (swap! messages* update :chat-content-received (fnil conj []) data))
  (config-updated [_ data] (swap! messages* update :config-updated (fnil conj []) data))
  (tool-server-updated [_ data] (swap! messages* update :tool-server-update (fnil conj []) data))
  (showMessage [_ data] (swap! messages* update :show-message (fnil conj []) data))
  (editor-diagnostics [_ _uri] (future {:diagnostics @diagnostics*})))

(defn ^:private make-components []
  (let [db* (atom db/initial-db)]
    {:db* (atom db/initial-db)
     :messenger (->TestMessenger (atom {}) (atom []))
     :metrics (metrics/->NoopMetrics db*)
     :config config/initial-config}))

(def components* (atom (make-components)))
(defn components [] @components*)

(defn db* [] (:db* (components)))
(defn db [] (deref (db*)))

(defn messages [] @(:messages* (:messenger (components))))
(defn messenger [] (:messenger (components)))

(defn config [] (:config (components)))

(defn metrics [] (:metrics (components)))

(defn config! [config]
  (swap! components* update :config shared/deep-merge config))

(defn reset-components! []
  (reset! config/initialization-config* {})
  (reset! components* (make-components)))
(defn reset-components-before-test []
  (use-fixtures :each (fn [f] (reset-components!) (f))))
(defn reset-messenger! [] (swap! components* assoc :messenger (:messenger (make-components))))
