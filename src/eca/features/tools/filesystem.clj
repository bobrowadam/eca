(ns eca.features.tools.filesystem
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [eca.logger :as logger]
   [clojure.string :as string]
   [eca.diff :as diff]
   [eca.features.tools.text-match :as text-match]
   [eca.features.tools.util :as tools.util]
   [eca.shared :as shared]))

(set! *warn-on-reflection* true)

(defn ^:private allowed-path? [db path]
  (some #(fs/starts-with? path (shared/uri->filename (:uri %)))
        (:workspace-folders db)))

(defn ^:private path-validations [db]
  [["path" fs/exists? "$path is not a valid path"]
   ["path" (partial allowed-path? db) (str "Access denied - path $path outside allowed directories: " (tools.util/workspace-roots-strs db))]])

(defn ^:private tree-walk
  "Recursively walks a directory tree, building a visual tree representation with counts."
  ([directory stats]
   (tree-walk directory "" stats 1 0))
  ([directory stats max-depth]
   (tree-walk directory "" stats max-depth 0))
  ([directory prefix stats max-depth current-depth]
   (let [filepaths (->> (fs/list-dir directory)
                        (map fs/file-name)
                        (remove #(string/starts-with? % "."))
                        sort
                        vec)]
     (loop [index 0
            output ""]
       (if (>= index (count filepaths))
         output
         (let [filename (nth filepaths index)
               absolute (fs/path directory filename)
               is-last? (= index (dec (count filepaths)))
               current-prefix (if is-last? "└── " "├── ")
               next-prefix (if is-last? "    " "│   ")]

           ;; Register file/directory in stats
           (if (fs/directory? absolute)
             (swap! (:dir-count stats) inc)
             (swap! (:file-count stats) inc))

           (let [current-line (str prefix current-prefix filename "\n")
                 ;; Only recurse if we haven't reached max depth
                 subtree-output (if (and (fs/directory? absolute)
                                         (< current-depth max-depth))
                                  (tree-walk absolute
                                             (str prefix next-prefix)
                                             stats
                                             max-depth
                                             (inc current-depth))
                                  "")]
             (recur (inc index)
                    (str output current-line subtree-output)))))))))

(defn ^:private directory-tree [arguments {:keys [db]}]
  (let [path (delay (fs/canonicalize (get arguments "path")))]
    (or (tools.util/invalid-arguments arguments (path-validations db))
        (let [max-depth (or (get arguments "max_depth") Integer/MAX_VALUE)
              stats {:dir-count (atom 0)
                     :file-count (atom 0)}
              tree-output (tree-walk @path stats max-depth)
              summary (format "%d directories, %d files"
                              @(:dir-count stats)
                              @(:file-count stats))]
          (tools.util/single-text-content
           (str tree-output "\n" summary))))))

(def ^:private read-file-max-lines 2000)

(defn ^:private read-file [arguments {:keys [db]}]
  (or (tools.util/invalid-arguments arguments (concat (path-validations db)
                                                      [["path" fs/readable? "File $path is not readable"]
                                                       ["path" (complement fs/directory?) "$path is a directory, not a file"]]))
      (let [line-offset (or (get arguments "line_offset") 0)
            limit (or (get arguments "limit") read-file-max-lines)
            full-content-lines (string/split-lines (slurp (fs/file (fs/canonicalize (get arguments "path")))))
            maybe-truncated-content-lines (cond-> full-content-lines
                                            line-offset (->> (drop line-offset))
                                            limit (->> (take limit)))
            was-truncated? (not= (- (count full-content-lines) line-offset)
                                 (count maybe-truncated-content-lines))
            content (string/join "\n" maybe-truncated-content-lines)]
        (tools.util/single-text-content (if was-truncated?
                                          (str content "\n\n"
                                               "[CONTENT TRUNCATED] Showing lines " (if line-offset (inc line-offset) 1) 
                                               " to " (+ (or line-offset 0) limit)
                                               " of " (count full-content-lines) " total lines. "
                                               "Use line_offset=" (+ (or line-offset 0) limit)
                                               " parameter to read more content.")
                                          content)))))

(defn ^:private read-file-summary [args]
  (if-let [path (get args "path")]
    (str "Reading file " (fs/file-name (fs/file path)))
    "Reading file"))

(defn ^:private write-file [arguments {:keys [db]}]
  (or (tools.util/invalid-arguments arguments [["path" (partial allowed-path? db) (str "Access denied - path $path outside allowed directories: " (tools.util/workspace-roots-strs db))]])
      (let [path (get arguments "path")
            content (get arguments "content")]
        (fs/create-dirs (fs/parent (fs/path path)))
        (spit path content)
        (tools.util/single-text-content (format "Successfully wrote to %s" path)))))

(defn ^:private write-file-summary [args]
  (if-let [path (get args "path")]
    (str "Creating file " (fs/file-name (fs/file path)))
    "Creating file"))

(defn ^:private run-ripgrep [path pattern include]
  (let [cmd (cond-> ["rg" "--files-with-matches" "--no-heading"]
              include (concat ["--glob" include])
              :always (concat ["-e" pattern path]))]
    (->> (apply shell/sh cmd)
         :out
         (string/split-lines)
         (filterv #(not (string/blank? %))))))

(defn ^:private run-grep [path pattern ^String include]
  (let [include-patterns (if (and include (.contains include "{"))
                           (let [pattern-match (re-find #"\*\.\{(.+)\}" include)]
                             (when pattern-match
                               (map #(str "*." %) (clojure.string/split (second pattern-match) #","))))
                           [include])
        cmd (cond-> ["grep" "-E" "-l" "-r" "--exclude-dir=.*"]
              (and include (> (count include-patterns) 1)) (concat (mapv #(str "--include=" %) include-patterns))
              include (concat [(str "--include=" include)])
              :always (concat [pattern path]))]
    (->> (apply shell/sh cmd)
         :out
         (string/split-lines)
         (filterv #(not (string/blank? %))))))

(defn ^:private run-java-grep [path pattern include]
  (let [include-pattern (when include
                          (re-pattern (str ".*\\.("
                                           (-> include
                                               (string/replace #"^\*\." "")
                                               (string/replace #"\*\.\{(.+)\}" "$1")
                                               (string/replace #"," "|"))
                                           ")$")))
        pattern-regex (re-pattern pattern)]
    (letfn [(search [dir]
              (keep
               (fn [file]
                 (cond
                   (and (fs/directory? file) (not (fs/hidden? file)))
                   (search file)

                   (and (not (fs/directory? file))
                        (or (nil? include-pattern)
                            (re-matches include-pattern (fs/file-name file))))
                   (try
                     (with-open [rdr (io/reader (fs/file file))]
                       (loop [lines (line-seq rdr)]
                         (when (seq lines)
                           (if (re-find pattern-regex (first lines))
                             (str (fs/canonicalize file))
                             (recur (rest lines))))))
                     (catch Exception _ nil))))
               (fs/list-dir dir)))]
      (when (fs/exists? path)
        (flatten (search path))))))

(defn ^:private grep
  "Searches for files containing patterns using regular expressions.

   This function provides a fast content search across files using three different
   backends depending on what's available:
   1. ripgrep (rg) - fastest, preferred when available
   2. grep - standard Unix tool fallback
   3. Pure Java implementation - slow, but cross-platform fallback

   Returns matching file paths, prioritizing by modification time when possible.
   Validates that the search path is within allowed workspace directories."
  [arguments {:keys [db]}]
  (or (tools.util/invalid-arguments arguments (concat (path-validations db)
                                                      [["path" fs/readable? "File $path is not readable"]
                                                       ["pattern" #(and % (not (string/blank? %))) "Invalid content regex pattern '$pattern'"]
                                                       ["include" #(or (nil? %) (not (string/blank? %))) "Invalid file pattern '$include'"]
                                                       ["max_results" #(or (nil? %) number?) "Invalid number '$max_results'"]]))
      (let [path (get arguments "path")
            pattern (get arguments "pattern")
            include (get arguments "include")
            max-results (or (get arguments "max_results") 1000)
            paths
            (->> (cond
                   (tools.util/command-available? "rg" "--version")
                   (run-ripgrep path pattern include)

                   (tools.util/command-available? "grep" "--version")
                   (run-grep path pattern include)

                   :else
                   (run-java-grep path pattern include))
                 (take max-results))]
        ;; TODO sort by modification time.
        (if (seq paths)
          (tools.util/single-text-content (string/join "\n" paths))
          (tools.util/single-text-content "No files found for given pattern" :error)))))

(defn grep-summary [args]
  (if-let [pattern (get args "pattern")]
    (if (> (count pattern) 22)
      (format "Searching for '%s...'" (subs pattern 0 22))
      (format "Searching for '%s'" pattern))
    "Searching for files"))

(defn ^:private handle-file-change-result
  "Convert file-change-full-content result to appropriate tool response"
  [result path success-message]
  (cond
    (:new-full-content result)
    (tools.util/single-text-content success-message)

    (= (:error result) :not-found)
    (tools.util/single-text-content (format "Original content not found in %s" path) :error)

    (= (:error result) :ambiguous)
    (tools.util/single-text-content
      (format "Ambiguous match - content appears %d times in %s. Provide more specific context to identify the exact location."
              (:match-count result) path) :error)

    :else
    (tools.util/single-text-content (format "Failed to process %s" path) :error)))

(defn ^:private change-file [arguments {:keys [db]}]
  (or (tools.util/invalid-arguments arguments (concat (path-validations db)
                                                      [["path" fs/readable? "File $path is not readable"]]))
      (let [path (get arguments "path")
            original-content (get arguments "original_content")
            new-content (get arguments "new_content")
            all? (boolean (get arguments "all_occurrences"))
            result (text-match/apply-content-change-to-file path original-content new-content all?)]
        (if (:new-full-content result)
          (do
            (spit path (:new-full-content result))
            (handle-file-change-result result path (format "Successfully replaced content in %s." path)))
          (handle-file-change-result result path nil)))))

(defn ^:private edit-file [arguments components]
  (change-file arguments components))

(defn ^:private preview-file-change [arguments {:keys [db]}]
  (or (tools.util/invalid-arguments arguments [["path" (partial allowed-path? db) (str "Access denied - path $path outside allowed directories: " (tools.util/workspace-roots-strs db))]])
      (let [path (get arguments "path")
            original-content (get arguments "original_content")
            new-content (get arguments "new_content")
            all? (boolean (get arguments "all_occurrences"))
            file-exists? (fs/exists? path)]
        (cond
          file-exists?
          (let [result (text-match/apply-content-change-to-file path original-content new-content all?)]
            (handle-file-change-result result path
                                       (format "Change simulation completed for %s. Original file unchanged - preview only." path)))

          (and (not file-exists?) (= "" original-content))
          (tools.util/single-text-content (format "New file creation simulation completed for %s. File will be created - preview only." path))

          :else
          (tools.util/single-text-content
           (format "Preview error for %s: For new files, original_content must be empty string (\"\"). Use markdown blocks during exploration, then eca_preview_file_change for final implementation only."
                   path)
           :error)))))

(defn ^:private move-file [arguments {:keys [db]}]
  (let [workspace-dirs (tools.util/workspace-roots-strs db)]
    (or (tools.util/invalid-arguments arguments [["source" fs/exists? "$source is not a valid path"]
                                                 ["source" (partial allowed-path? db) (str "Access denied - path $source outside allowed directories: " workspace-dirs)]
                                                 ["destination" (partial allowed-path? db) (str "Access denied - path $destination outside allowed directories: " workspace-dirs)]
                                                 ["destination" (complement fs/exists?) "Path $destination already exists"]])
        (let [source (get arguments "source")
              destination (get arguments "destination")]
          (fs/move source destination {:replace-existing false})
          (tools.util/single-text-content (format "Successfully moved %s to %s" source destination))))))

(def definitions
  {"eca_directory_tree"
   {:description (tools.util/read-tool-description "eca_directory_tree")
    :parameters {:type "object"
                 :properties {"path" {:type "string"
                                      :description "The absolute path to the directory."}
                              "max_depth" {:type "integer"
                                           :description "Maximum depth to traverse (optional)"}
                              "limit" {:type "integer"
                                       :description "Maxium number of entries to show (default: 100)"}}
                 :required ["path"]}
    :handler #'directory-tree
    :summary-fn (constantly "Listing file tree")}
   "eca_read_file"
   {:description (tools.util/read-tool-description "eca_read_file")
    :parameters {:type "object"
                 :properties {"path" {:type "string"
                                      :description "The absolute path to the file to read."}
                              "line_offset" {:type "integer"
                                             :description "Line to start reading from (default: 0)"}
                              "limit" {:type "integer"
                                       :description (str "Maximum lines to read (default: " read-file-max-lines ")")}}
                 :required ["path"]}
    :handler #'read-file
    :summary-fn #'read-file-summary}
   "eca_write_file"
   {:description (tools.util/read-tool-description "eca_write_file")
    :parameters {:type "object"
                 :properties {"path" {:type "string"
                                      :description "The absolute path to the file to create or overwrite"}
                              "content" {:type "string"
                                         :description "The complete content to write to the file"}}
                 :required ["path" "content"]}
    :handler #'write-file
    :summary-fn #'write-file-summary}
   "eca_edit_file"
   {:description (tools.util/read-tool-description "eca_edit_file")
    :parameters  {:type "object"
                  :properties {"path" {:type "string"
                                       :description "The absolute file path to do the replace."}
                               "original_content" {:type "string"
                                                   :description "The exact content to find and replace"}
                               "new_content" {:type "string"
                                              :description "The new content to replace the original content with"}
                               "all_occurrences" {:type "boolean"
                                                  :description "Whether to replace all occurences of the file or just the first one (default)"}}
                  :required ["path" "original_content" "new_content"]}
    :handler #'edit-file
    :summary-fn (constantly "Editing file")}
   "eca_preview_file_change"
   {:description (tools.util/read-tool-description "eca_preview_file_change")
    :parameters {:type "object"
                 :properties {"path" {:type "string"
                                      :description "The absolute file path to preview changes for."}
                              "original_content" {:type "string"
                                                  :description "The exact content to find in the file"}
                              "new_content" {:type "string"
                                             :description "The content to show as replacement in the preview"}
                              "all_occurrences" {:type "boolean"
                                                 :description "Whether to preview replacing all occurrences or just the first one (default)"}}
                 :required ["path" "original_content" "new_content"]}
    :handler #'preview-file-change
    :summary-fn (constantly "Previewing change")}
   "eca_move_file"
   {:description (tools.util/read-tool-description "eca_move_file")
    :parameters  {:type "object"
                  :properties {"source" {:type "string"
                                         :description "The absolute origin file path to move."}
                               "destination" {:type "string"
                                              :description "The new absolute file path to move to."}}
                  :required ["source" "destination"]}
    :handler #'move-file
    :summary-fn (constantly "Moving file")}
   "eca_grep"
   {:description (tools.util/read-tool-description "eca_grep")
    :parameters  {:type "object"
                  :properties {"path" {:type "string"
                                       :description "The absolute path to search in."}
                               "pattern" {:type "string"
                                          :description "The regular expression pattern to search for in file contents"}
                               "include" {:type "string"
                                          :description "File pattern to include in the search (e.g. \"*.clj\", \"*.{clj,cljs}\")"}
                               "max_results" {:type "integer"
                                              :description "Maximum number of results to return (default: 1000)"}}
                  :required ["path" "pattern"]}
    :handler #'grep
    :summary-fn #'grep-summary}})

(defmethod tools.util/tool-call-details-before-invocation :eca_edit_file [_name arguments]
  (let [path (get arguments "path")
        original-content (get arguments "original_content")
        new-content (get arguments "new_content")
        all? (get arguments "all_occurrences")
        file-exists? (and path (fs/exists? path))]
    (cond
      (and file-exists? original-content new-content)
      (let [result (text-match/apply-content-change-to-file path original-content new-content all?)
            original-full-content (:original-full-content result)]
        (when original-full-content
          (if-let [new-full-content (:new-full-content result)]
            (let [{:keys [added removed diff]} (diff/diff original-full-content new-full-content path)]
              {:type :fileChange
               :path path
               :linesAdded added
               :linesRemoved removed
               :diff diff})
            (logger/warn "tool-call-details-before-invocation - NO DIFF GENERATED because match failed for path:" path))))

      (and (not file-exists?) (= original-content "") new-content path)
      (let [{:keys [added removed diff]} (diff/diff "" new-content path)]
        {:type :fileChange
         :path path
         :linesAdded added
         :linesRemoved removed
         :diff diff})

      :else nil)))

(defmethod tools.util/tool-call-details-before-invocation :eca_preview_file_change [_name arguments]
  (tools.util/tool-call-details-before-invocation :eca_edit_file arguments))

(defmethod tools.util/tool-call-details-before-invocation :eca_write_file [_name arguments]
  (let [path (get arguments "path")
        content (get arguments "content")]
    (when (and path content)
      (let [{:keys [added removed diff]} (diff/diff "" content path)]
        {:type :fileChange
         :path path
         :linesAdded added
         :linesRemoved removed
         :diff diff}))))
