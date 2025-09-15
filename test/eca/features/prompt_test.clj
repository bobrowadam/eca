(ns eca.features.prompt-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [eca.features.prompt :as prompt]))

(deftest build-instructions-test
  (testing "Should create instructions with rules, contexts, and behavior"
    (let [refined-contexts [{:type :file :path "foo.clj" :content "(ns foo)"}
                            {:type :file :path "bar.clj" :content "(def a 1)" :partial true}
                            {:type :repoMap}
                            {:type :mcpResource :uri "custom://my-resource" :content "some-cool-content"}]
          rules [{:name "rule1" :content "First rule"}
                 {:name "rule2" :content "Second rule"}]
          fake-repo-map (delay "TREE")
          behavior "agent"
          result (prompt/build-instructions refined-contexts rules fake-repo-map behavior {})]
      (is (string/includes? result "You are ECA"))
      (is (string/includes? result "<rules description=\"Rules defined by user\">"))
      (is (string/includes? result "<rule name=\"rule1\">First rule</rule>"))
      (is (string/includes? result "<rule name=\"rule2\">Second rule</rule>"))
      (is (string/includes? result "<contexts description=\"Manually provided by user, usually when provided user knows that your task is related to those files, so consider relying on it, if not enough, use tools to read/gather any extra files/contexts.\">"))
      (is (string/includes? result "<file path=\"foo.clj\">(ns foo)</file>"))
      (is (string/includes? result "<file partial=true path=\"bar.clj\">...\n(def a 1)\n...</file>"))
      (is (string/includes? result "<repoMap description=\"Workspaces structure in a tree view, spaces represent file hierarchy\" >TREE</repoMap>"))
      (is (string/includes? result "<resource uri=\"custom://my-resource\">some-cool-content</resource>"))
      (is (string/includes? result "</contexts>"))
      (is (string? result))))
  (testing "Should create instructions with rules, contexts, and plan behavior"
    (let [refined-contexts [{:type :file :path "foo.clj" :content "(ns foo)"}
                            {:type :file :path "bar.clj" :content "(def a 1)" :partial true}
                            {:type :repoMap}
                            {:type :mcpResource :uri "custom://my-resource" :content "some-cool-content"}]
          rules [{:name "rule1" :content "First rule"}
                 {:name "rule2" :content "Second rule"}]
          fake-repo-map (delay "TREE")
          behavior "plan"
          result (prompt/build-instructions refined-contexts rules fake-repo-map behavior {})]
      (is (string/includes? result "You are ECA"))
      (is (string/includes? result "<rules description=\"Rules defined by user\">"))
      (is (string/includes? result "<rule name=\"rule1\">First rule</rule>"))
      (is (string/includes? result "<rule name=\"rule2\">Second rule</rule>"))
      (is (string/includes? result "<contexts description=\"Manually provided by user, usually when provided user knows that your task is related to those files, so consider relying on it, if not enough, use tools to read/gather any extra files/contexts.\">"))
      (is (string/includes? result "<file path=\"foo.clj\">(ns foo)</file>"))
      (is (string/includes? result "<file partial=true path=\"bar.clj\">...\n(def a 1)\n...</file>"))
      (is (string/includes? result "<repoMap description=\"Workspaces structure in a tree view, spaces represent file hierarchy\" >TREE</repoMap>"))
      (is (string/includes? result "<resource uri=\"custom://my-resource\">some-cool-content</resource>"))
      (is (string/includes? result "</contexts>"))
      (is (string? result)))))

(deftest cursor-context-formatting-test
  (testing "Should format  cursor context prominently with surrounding code"
    (let [cursor-context {:type :cursor
                          :path "/path/to/file.clj"
                          :position {:start {:line 5 :character 10}
                                     :end {:line 5 :character 15}}
                          :surrounding-lines ["(defn bar" "  [x]" "  (* x 2))" "" "(defn cursor-fn" "  [a b]" "  (+ a b))"]}
          other-contexts [{:type :file :path "foo.clj" :content "(ns foo)"}]
          result (prompt/build-instructions [cursor-context (first other-contexts)] [] (delay "") "agent" {})]
      (is (string/includes? result "## CURRENT USER CURSOR POSITION"))
      (is (string/includes? result "**File:** `/path/to/file.clj`"))
      (is (string/includes? result "**Position:** line 6, columns 10-15"))
      (is (string/includes? result "**Surrounding code:**"))
      (is (string/includes? result "  6  >>> "))
      (is (string/includes? result "  3      (defn bar"))
      (is (string/includes? result "<contexts description="))
      (is (string/includes? result "<file path=\"foo.clj\">"))))

  (testing "Should handle cursor context without surrounding lines"
    (let [cursor-context {:type :cursor
                          :path "/path/to/file.clj"
                          :position {:start {:line 0 :character 0}
                                     :end {:line 0 :character 0}}
                          :surrounding-lines []}
          result (prompt/build-instructions [cursor-context] [] (delay "") "agent" {})]
      (is (string/includes? result "## CURRENT USER CURSOR POSITION"))
      (is (string/includes? result "**File:** `/path/to/file.clj`"))
      (is (string/includes? result "**Position:** line 1, column 0"))
      (is (not (string/includes? result "**Surrounding code:**")))))

  (testing "Should handle cursor context with missing path"
    (let [cursor-context {:type :cursor
                          :position {:start {:line 5 :character 10}
                                     :end {:line 5 :character 15}}
                          :surrounding-lines ["line 1" "line 2"]}
          other-contexts [{:type :file :path "foo.clj" :content "(ns foo)"}]
          result (prompt/build-instructions [cursor-context (first other-contexts)] [] (delay "") "agent" {})]
      ;; Should not include cursor context section when path is missing
      (is (not (string/includes? result "## CURRENT USER CURSOR POSITION")))
      ;; Should still include other contexts
      (is (string/includes? result "<contexts description="))
      (is (string/includes? result "<file path=\"foo.clj\">"))))

  (testing "Should handle cursor context with missing position"
    (let [cursor-context {:type :cursor
                          :path "/path/to/file.clj"
                          :surrounding-lines ["line 1" "line 2"]}
          other-contexts [{:type :file :path "foo.clj" :content "(ns foo)"}]
          result (prompt/build-instructions [cursor-context (first other-contexts)] [] (delay "") "agent" {})]
      ;; Should not include cursor context section when position is missing
      (is (not (string/includes? result "## CURRENT USER CURSOR POSITION")))
      ;; Should still include other contexts
      (is (string/includes? result "<contexts description="))
      (is (string/includes? result "<file path=\"foo.clj\">"))))

  (testing "Should handle empty refined-contexts"
    (let [result (prompt/build-instructions [] [] (delay "TREE") "agent" {})]
      (is (string/includes? result "You are ECA"))
      (is (not (string/includes? result "## CURRENT USER CURSOR POSITION")))
      (is (not (string/includes? result "<contexts description=")))))

  (testing "Should handle only cursor contexts in refined-contexts"
    (let [cursor-context {:type :cursor
                          :path "/path/to/file.clj"
                          :position {:start {:line 5 :character 10}
                                     :end {:line 5 :character 15}}
                          :surrounding-lines ["line 1" "line 2"]}
          result (prompt/build-instructions [cursor-context] [] (delay "TREE") "agent" {})]
      (is (string/includes? result "You are ECA"))
      (is (string/includes? result "## CURRENT USER CURSOR POSITION"))
      (is (string/includes? result "**File:** `/path/to/file.clj`"))
      ;; Should not include empty contexts section
      (is (not (string/includes? result "<contexts description=")))))

  (testing "Should handle cursor context with malformed position data"
    (let [cursor-context {:type :cursor
                          :path "/path/to/file.clj"
                          :position {:start {:line nil :character 10}
                                     :end {:line 5 :character nil}}
                          :surrounding-lines ["line 1" "line 2"]}
          other-contexts [{:type :file :path "foo.clj" :content "(ns foo)"}]
          result (prompt/build-instructions [cursor-context (first other-contexts)] [] (delay "") "agent" {})]
      ;; Should gracefully handle malformed position and not crash
      (is (string? result))
      (is (string/includes? result "You are ECA"))
      ;; Should not include cursor context section when position data is malformed
      (is (not (string/includes? result "## CURRENT USER CURSOR POSITION")))
      ;; Should include other contexts even if cursor context fails
      (is (string/includes? result "<contexts description="))
      (is (string/includes? result "<file path=\"foo.clj\">"))))

  (testing "Should handle cursor context with incomplete position structure"
    (let [cursor-context {:type :cursor
                          :path "/path/to/file.clj"
                          :position {:start {:line 5}} ; missing :character
                          :surrounding-lines ["line 1" "line 2"]}
          other-contexts [{:type :file :path "foo.clj" :content "(ns foo)"}]
          result (prompt/build-instructions [cursor-context (first other-contexts)] [] (delay "") "agent" {})]
      ;; Should gracefully handle incomplete position structure
      (is (string? result))
      (is (string/includes? result "You are ECA"))
      ;; Should not include cursor context section when position structure is incomplete
      (is (not (string/includes? result "## CURRENT USER CURSOR POSITION")))
      ;; Should include other contexts
      (is (string/includes? result "<contexts description="))
      (is (string/includes? result "<file path=\"foo.clj\">")))))
