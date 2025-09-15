(ns eca.features.prompt
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [eca.features.tools.mcp :as f.mcp]
   [eca.logger :as logger]
   [eca.shared :refer [multi-str] :as shared])
  (:import
   [java.util Map]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[PROMPT]")

;; Built-in behavior prompts are now complete files, not templates
(defn ^:private load-builtin-prompt* [filename]
  (slurp (io/resource (str "prompts/" filename))))

(def ^:private load-builtin-prompt (memoize load-builtin-prompt*))

(defn ^:private init-prompt-template* [] (slurp (io/resource "prompts/init.md")))
(def ^:private init-prompt-template (memoize init-prompt-template*))

(defn ^:private title-prompt-template* [] (slurp (io/resource "prompts/title.md")))
(def ^:private title-prompt-template (memoize title-prompt-template*))

(defn ^:private compact-prompt-template* [file-path]
  (if (fs/relative? file-path)
    (slurp (io/resource file-path))
    (slurp (io/file file-path))))

(def ^:private compact-prompt-template (memoize compact-prompt-template*))

(defn ^:private replace-vars [s vars]
  (reduce
   (fn [p [k v]]
     (string/replace p (str "{" (name k) "}") v))
   s
   vars))

(defn ^:private eca-prompt [behavior config]
  (let [behavior-config (get-in config [:behavior behavior])
        ;; Use systemPromptFile from behavior config, or fall back to built-in
        prompt-file (or (:systemPromptFile behavior-config)
                        ;; For built-in behaviors without explicit config
                        (when (#{"agent" "plan"} behavior)
                          (str "prompts/" behavior "_behavior.md")))]
    (cond
      ;; Custom behavior with absolute path
      (and prompt-file (string/starts-with? prompt-file "/"))
      (slurp prompt-file)

      ;; Built-in or resource path
      prompt-file
      (load-builtin-prompt (some-> prompt-file (string/replace-first #"prompts/" "")))

      ;; Fallback for unknown behavior
      :else
      (load-builtin-prompt "agent_behavior.md"))))

(defn ^:private format-surrounding-lines [start-line surrounding-lines]
  (str "**Surrounding code:**\n```\n"
       (string/join "\n"
                    (map-indexed (fn [idx line]
                                   (let [cursor-line-0-based (dec start-line)
                                         start-idx (max 0 (- cursor-line-0-based 3))
                                         line-num (inc (+ start-idx idx))
                                         is-cursor-line (= line-num start-line)]
                                     (str (format "%3d  " line-num)
                                          (if is-cursor-line ">>> " "    ")
                                          line)))
                                 surrounding-lines))
       "\n```\n\n"))

(defn ^:private format-cursor-context [{:keys [path position surrounding-lines]}]
  (when (and path position)
    (let [start-line-raw (:line (:start position))
          start-char (:character (:start position))
          end-line-raw (:line (:end position))
          end-char (:character (:end position))]
      ;; Validate that all position components are numbers, not nil
      (when (and (number? start-line-raw) (number? start-char)
                 (number? end-line-raw) (number? end-char))
        (let [start-line (inc start-line-raw) ; Convert to 1-based
              end-line (inc end-line-raw) ; Convert to 1-based
              coordinates (if (= start-line end-line)
                            (if (= start-char end-char)
                              (format "line %d, column %d" start-line start-char)
                              (format "line %d, columns %d-%d" start-line start-char end-char))
                            (format "lines %d:%d to %d:%d" start-line start-char end-line end-char))]
          (str "\n## CURRENT USER CURSOR POSITION\n\n"
               (format "**File:** `%s`  \n" path)
               (format "**Position:** %s\n\n" coordinates)
               (when (seq surrounding-lines)
                 (format-surrounding-lines start-line surrounding-lines))))))))

(defn build-instructions [refined-contexts rules repo-map* behavior config]
  (let [cursor-contexts (filter #(= :cursor (:type %)) refined-contexts)
        non-cursor-contexts (filter #(not= :cursor (:type %)) refined-contexts)]
    (multi-str
     (eca-prompt behavior config)
     (when (seq rules)
       ["<rules description=\"Rules defined by user\">\n"
        (reduce
         (fn [rule-str {:keys [name content]}]
           (str rule-str (format "<rule name=\"%s\">%s</rule>\n" name content)))
         ""
         rules)
        "</rules>"])

     ;; Add cursor contexts with enhanced formatting right after rules
     (when (seq cursor-contexts)
       (reduce str ""
               (map format-cursor-context cursor-contexts)))
     ""
     (when (seq non-cursor-contexts)
       ["<contexts description=\"Manually provided by user, usually when provided user knows that your task is related to those files, so consider relying on it, if not enough, use tools to read/gather any extra files/contexts.\">"
        (reduce
         (fn [context-str {:keys [type path content partial uri]}]
           (str context-str (case type
                              :file (if partial
                                      (format "<file partial=true path=\"%s\">...\n%s\n...</file>\n" path content)
                                      (format "<file path=\"%s\">%s</file>\n" path content))
                              :repoMap (format "<repoMap description=\"Workspaces structure in a tree view, spaces represent file hierarchy\" >%s</repoMap>\n" @repo-map*)
                              :mcpResource (format "<resource uri=\"%s\">%s</resource>\n" uri content)
                              "")))
         ""
         non-cursor-contexts)
        "</contexts>"]))))

(defn init-prompt [db]
  (replace-vars
   (init-prompt-template)
   {:workspaceFolders (string/join ", " (map (comp shared/uri->filename :uri) (:workspace-folders db)))}))

(defn title-prompt []
  (title-prompt-template))

(defn compact-prompt [additional-input config]
  (replace-vars
   (compact-prompt-template (:compactPromptFile config))
   {:addionalUserInput (if additional-input
                         (format "You MUST respect this user input in the summarization: %s." additional-input)
                         "")}))

(defn get-prompt! [^String name ^Map arguments db]
  (logger/info logger-tag (format "Calling prompt '%s' with args '%s'" name arguments))
  (try
    (let [result (f.mcp/get-prompt! name arguments db)]
      (logger/debug logger-tag "Prompt result: " result)
      result)
    (catch Exception e
      (logger/warn logger-tag (format "Error calling prompt %s: %s" name e))
      {:error-message (str "Error calling prompt: " (.getMessage e))})))
