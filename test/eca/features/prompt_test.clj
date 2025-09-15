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
      (is (string/includes? result "<contexts description=\"Manually provided by user, usually when provided user knows that your task is related to those files, so consider reliying on it, if not enough, use tools to read/gather any extra files/contexts.\">"))
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
      (is (string/includes? result "<contexts description=\"Manually provided by user, usually when provided user knows that your task is related to those files, so consider reliying on it, if not enough, use tools to read/gather any extra files/contexts.\">"))
      (is (string/includes? result "<file path=\"foo.clj\">(ns foo)</file>"))
      (is (string/includes? result "<file partial=true path=\"bar.clj\">...\n(def a 1)\n...</file>"))
      (is (string/includes? result "<repoMap description=\"Workspaces structure in a tree view, spaces represent file hierarchy\" >TREE</repoMap>"))
      (is (string/includes? result "<resource uri=\"custom://my-resource\">some-cool-content</resource>"))
      (is (string/includes? result "</contexts>"))
      (is (string? result)))))

(deftest cursor-context-formatting-test
  (testing "Should format cursor context prominently with surrounding code"
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
      (is (not (string/includes? result "**Surrounding code:**"))))))
