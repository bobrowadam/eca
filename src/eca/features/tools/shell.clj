(ns eca.features.tools.shell
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [clojure.string :as string]
   [eca.config :as config]
   [eca.features.tools.util :as tools.util]
   [eca.logger :as logger]
   [eca.shared :as shared]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[TOOLS-SHELL]")

(defn ^:private shell-command [arguments {:keys [db]}]
  (let [command-args (get arguments "command")
        user-work-dir (get arguments "working_directory")]
    (or (tools.util/invalid-arguments arguments [["working_directory" #(or (nil? %)
                                                                           (fs/exists? %)) "working directory $working_directory does not exist"]])
        (let [work-dir (or (some-> user-work-dir fs/canonicalize str)
                           (some-> (:workspace-folders db)
                                   first
                                   :uri
                                   shared/uri->filename)
                           (config/get-property "user.home"))
              _ (logger/debug logger-tag "Running command:" command-args)
              result (try
                       (p/shell {:dir work-dir
                                 :out :string
                                 :err :string
                                 :continue true} "bash -c" command-args)
                       (catch Exception e
                         {:exit 1 :err (.getMessage e)}))
              err (some-> (:err result) string/trim)
              out (some-> (:out result) string/trim)]
          (logger/debug logger-tag "Command executed:" result)
          (if (zero? (:exit result))
            (tools.util/single-text-content (:out result))
            {:error true
             :contents (remove nil?
                               (concat [{:type :text
                                         :text (str "Exit code " (:exit result))}]
                                       (when-not (string/blank? err)
                                         [{:type :text
                                           :text (str "Stderr:\n" err)}])
                                       (when-not (string/blank? out)
                                         [{:type :text
                                           :text (str "Stdout:\n" out)}])))})))))

(defn shell-command-summary [args]
  (if-let [command (get args "command")]
    (if (> (count command) 20)
      (format "Running '%s...'" (subs command 0 20))
      (format "Running '%s'" command))
    "Running shell command"))

(def definitions
  {"eca_shell_command"
   {:description (tools.util/read-tool-description "eca_shell_command")
    :parameters {:type "object"
                 :properties {"command" {:type "string"
                                         :description "The shell command to execute."}
                              "working_directory" {:type "string"
                                                   :description "The directory to run the command in. Default to the first workspace root."}}
                 :required ["command"]}
    :handler #'shell-command
    :require-approval-fn (fn [args {:keys [db]}]
                           (when-let [wd (and args (get args "working_directory"))]
                             (when-let [wd (and (fs/exists? wd) (str (fs/canonicalize wd)))]
                               (let [workspace-roots (mapv (comp shared/uri->filename :uri) (:workspace-folders db))]
                                 (not-any? #(fs/starts-with? wd %) workspace-roots)))))
    :summary-fn #'shell-command-summary}})
