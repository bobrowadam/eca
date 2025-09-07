(ns eca.features.tools.shell-test
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [clojure.test :refer [deftest is testing are]]
   [eca.config :as config]
   [eca.features.tools :as f.tools]
   [eca.features.tools.shell :as f.tools.shell]
   [eca.test-helper :as h]
   [matcher-combinators.test :refer [match?]]))

(deftest shell-command-test
  (testing "inexistent working_directory"
    (is (match?
         {:error true
          :contents [{:type :text
                      :text (format "working directory %s does not exist" (h/file-path "/baz"))}]}
         (with-redefs [fs/exists? (constantly false)]
           ((get-in f.tools.shell/definitions ["eca_shell_command" :handler])
            {"command" "ls -lh"
             "working_directory" (h/file-path "/baz")}
            {:db {:workspace-folders [{:uri (h/file-uri "file:///project/foo") :name "foo"}]}})))))
  (testing "command exited with non-zero exit code"
    (is (match?
         {:error true
          :contents [{:type :text
                      :text "Exit code 1"}
                     {:type :text
                      :text "Stderr:\nSome error"}]}
         (with-redefs [fs/exists? (constantly true)
                       p/shell (constantly {:exit 1 :err "Some error"})]
           ((get-in f.tools.shell/definitions ["eca_shell_command" :handler])
            {"command" "ls -lh"}
            {:db {:workspace-folders [{:uri (h/file-uri "file:///project/foo") :name "foo"}]}})))))
  (testing "command succeeds"
    (is (match?
         {:error false
          :contents [{:type :text
                      :text "Some text"}]}
         (with-redefs [fs/exists? (constantly true)
                       p/shell (constantly {:exit 0 :out "Some text" :err "Other text"})]
           ((get-in f.tools.shell/definitions ["eca_shell_command" :handler])
            {"command" "ls -lh"}
            {:db {:workspace-folders [{:uri (h/file-uri "file:///project/foo") :name "foo"}]}})))))
  (testing "command succeeds with different working directory"
    (is (match?
         {:error false
          :contents [{:type :text
                      :text "Some text"}]}
         (with-redefs [fs/exists? (constantly true)
                       p/shell (constantly {:exit 0 :out "Some text" :err "Other text"})]
           ((get-in f.tools.shell/definitions ["eca_shell_command" :handler])
            {"command" "ls -lh"
             "working_directory" (h/file-path "/project/foo/src")}
            {:db {:workspace-folders [{:uri (h/file-uri "file:///project/foo") :name "foo"}]}})))))
)

(deftest shell-require-approval-fn-test
  (let [approval-fn (get-in f.tools.shell/definitions ["eca_shell_command" :require-approval-fn])
        db {:workspace-folders [{:uri (h/file-uri "file:///project/foo") :name "foo"}]}]
    (testing "returns nil when working_directory is not provided"
      (is (nil? (approval-fn nil {:db db})))
      (is (nil? (approval-fn {} {:db db}))))
    (testing "returns nil when working_directory does not exist"
      (with-redefs [fs/exists? (constantly false)]
        (is (nil? (approval-fn {"working_directory" (h/file-path "/project/foo/src")} {:db db})))))
    (testing "returns false when working_directory equals a workspace root"
      (with-redefs [fs/exists? (constantly true)
                    fs/canonicalize identity]
        (is (false? (approval-fn {"working_directory" (h/file-path "/project/foo")} {:db db})))))
    (testing "returns false when working_directory is a subdirectory of a workspace root"
      (with-redefs [fs/exists? (constantly true)
                    fs/canonicalize identity]
        (is (false? (approval-fn {"working_directory" (h/file-path "/project/foo/src")} {:db db})))))
    (testing "returns true when working_directory is outside any workspace root"
      (with-redefs [fs/exists? (constantly true)
                    fs/canonicalize identity]
        (is (true? (approval-fn {"working_directory" (h/file-path "/other/place")} {:db db})))))))

(deftest plan-mode-restrictions-test
  (testing "safe commands allowed in plan mode"
    (are [command] (match?
                    {:error false
                     :contents [{:type :text
                                 :text "Some output"}]}
                    (with-redefs [fs/exists? (constantly true)
                                  p/shell (constantly {:exit 0 :out "Some output"})]
                      ((get-in f.tools.shell/definitions ["eca_shell_command" :handler])
                       {"command" command}
                       {:db {:workspace-folders [{:uri (h/file-uri "file:///project/foo") :name "foo"}]}
                        :behavior "plan"})))
      "git status"
      "ls -la"
      "find . -name '*.clj'"
      "grep 'test' file.txt"
      "cat file.txt"
      "head -10 file.txt"
      "pwd"
      "date"
      "env")))

(deftest plan-mode-restrictions-test
  (let [all-tools [{:name "eca_shell_command" :server "eca"}]
        config config/initial-config]
    
    (testing "dangerous commands blocked in plan mode via approval"
      (are [command] (= :deny 
                       (f.tools/approval all-tools "eca_shell_command" 
                                       {"command" command} {} config "plan"))
        "echo 'test' > file.txt"
        "cat file.txt > output.txt"
        "ls >> log.txt"
        "rm file.txt"
        "mv old.txt new.txt"
        "cp file1.txt file2.txt"
        "touch newfile.txt"
        "mkdir newdir"
        "sed -i 's/old/new/' file.txt"
        "git add ."
        "git commit -m 'test'"
        "npm install package"
        "python -c \"open('file.txt','w').write('test')\""
        "bash -c 'echo test > file.txt'"))
    
    (testing "non-dangerous commands default to ask in plan mode"
      (are [command] (= :ask
                       (f.tools/approval all-tools "eca_shell_command"
                                       {"command" command} {} config "plan"))
        "python --version"  ; not matching dangerous patterns, defaults to ask
        "node script.js"     ; not matching dangerous patterns, defaults to ask
        "clojure -M:test"))  ; not matching dangerous patterns, defaults to ask
    
    (testing "safe commands not denied in plan mode"
      (are [command] (not= :deny
                          (f.tools/approval all-tools "eca_shell_command"
                                          {"command" command} {} config "plan"))
        "git status"
        "ls -la"
        "find . -name '*.clj'"
        "grep 'test' file.txt"
        "cat file.txt"
        "head -10 file.txt"
        "pwd"
        "date"
        "env"))
    
    (testing "same commands work fine in agent mode (not denied)"
      (are [command] (not= :deny
                          (f.tools/approval all-tools "eca_shell_command"
                                          {"command" command} {} config "agent"))
        "echo 'test' > file.txt"
        "rm file.txt"
        "git add ."
        "python --version"))))
