(ns token-matcher.core-test
  (:require [clojure.test :refer :all]
            [token-matcher.core :refer :all]))

(deftest parse-template-test
  ;; We need to refer our used public symbols---in contexts where they
  ;; are quoted---because most testers run in the `user` namespace.
  (testing "parse-template"
    (refer 'token-matcher.core :only '[digits-alone n-digits-alone])
    (is (= '([*int {:max-tokens 1,
                    :finally? (token-matcher.core/digit-string? *int)}])
           (parse-template '([*int (digits-alone {} *int)]))))
    (is (= '([*int
              {:finally?
               (and
                (token-matcher.core/digit-string? *int)
                (> (read-string *int) 28)),
               :max-tokens 1}])
           (parse-template '([*int (digits-alone {:finally? (> (read-string *int) 28)} *int)]))))
    (is (= '([*int
              {:finally? (and
                          (token-matcher.core/digit-string? *int)
                          (clojure.core/= (clojure.core/count *int) 3)
                          (> (read-string *int) 99)),
               :max-tokens 1}])
           (parse-template '([*int (n-digits-alone {:finally? (> (read-string *int) 99)}
                                                                      *int
                                                                      3)]))))
    ))

(deftest template-local-test
  (testing "Locals in template"
    ;; Local scope:
    (let [max 3
          max-limited-digits (fn [self]
                               {:max-tokens 1
                                :finally? `(and (digit-string? ~self)
                                                (<= (count ~self) ~max))})]
      (is (= '{*foo "23", *bar "321"}
             (match `([~'*foo ~(max-limited-digits '*foo)]
                      [~'*bar ~(max-limited-digits '*bar)])
                    "23 321")))
      (is (= nil
             (match `([~'*foo ~(max-limited-digits '*foo)]
                      [~'*bar ~(max-limited-digits '*bar)])
                    "23 4321")))
      (is (= '([*foo
	        {:max-tokens 1,
	         :finally?
	         (clojure.core/and
	          (token-matcher.core/digit-string? *foo)
	          (clojure.core/<= (clojure.core/count *foo) 3))}]
	       [*bar
	        {:max-tokens 1,
	         :finally?
	         (clojure.core/and
	          (token-matcher.core/digit-string? *bar)
	          (clojure.core/<= (clojure.core/count *bar) 3))}])
             (parse-template `([~'*foo ~(max-limited-digits '*foo)]
                               [~'*bar ~(max-limited-digits '*bar)]))))
      )))

(def ^:dynamic *test-atom* (atom nil))

;;; TODO: Explicit/alternative bindings for dynamic Clojure vars.
(deftest match-test
  ;; We need to refer our used public symbols---in contexts where they
  ;; are quoted---because most testers run in the `user` namespace.
  (refer 'token-matcher.core :only '[count-tokens
                                     digits-alone n-digits-alone digits-along
                                     different-when-bound same-when-bound])
  (testing "match"
    ;; No vars.
    ;; Not handled: (is (= {} (match "" "")))
    (is (= {} (match "foo" "foo")))
    (is (= {} (match "foo bar" "foo bar")))
    (is (= nil (match "foo" "bar")))
    ;; Only *vars.
    (is (= '{*foo "bar"} (match "*foo" "bar")))
    (is (= '{*foo "bar bell"} (match "*foo" "bar bell")))
    (is (= '{*foo "bar"} (match "*foo bell" "bar bell")))
    (is (= {} (match '("#to") "#to")))
    (binding [*chars-to-strip* #{}]
      (match '("'twas") "'twas"))
    (is (= '{*foo "true false"} (match "*foo" "true false")))
    (is (= '{*foo "false"} (match "\"nil\" *foo" "nil false")))
    (is (= nil (match "nil *foo" "nil false")))
    (is (= nil (match "*foo bell" "bar")))
    (is (= '{*foo "bar"} (match "bell *foo" "bell bar")))
    (is (= '{*foo "bar none"} (match "bell *foo" "bell bar none")))
    (is (= nil (match "*foo bar" "bar bell")))
    (is (= '{*foo "bar" *ring "bell"} (match "*foo *ring" "bar bell")))
    (is (= '{*first "first", *third "third"}
           (match "*first second *third" "first second third")))
    (is (= '{*first "first first", *third "third"}
           (match "*first second *third" "first first second third")))
    (is (= '{*first "first first", *fourth "fourth"}
           (match "*first second third *fourth" "first first second third fourth")))
    (is (= '{*front "x y", *middle "m n", *back "r s"}
           (match "*front aaa *middle bbb *back"
                  "x y aaa m n bbb r s")))
    ;; Consecutive *vars: Prefer longer bindings for earlier vars.
    ;; Replace the with a.
    (is (= '{*foo "bar that" *ring "bell"}
           (match "*foo *ring" "bar that bell")))
    ;; Multi-occurrence vars:
    (is (= '{*foo "bar"} (match "*foo *foo" "bar bar")))
    (is (= nil (match "*foo *foo" "bar bell")))
    ;; Addressing +vars:
    (do (install-kind-instance-map '{kind   #{"really" "really big show"
                                              "John" "John Smith"}
                                     kinder #{"kinder" "kinder gentler"
                                              "something" "something really big"}
                                     kindle #{"Book 1" "Book 2"}})
      ;; Only +vars:
      (is (= '{+kind "really"}
             (match "+kind" "really")))
      (is (= '{+kind "really big show"}
             (match "+kind" "really big show")))
      (is (= '{+kinder "something" +kind "really big show"}
             (match "+kinder +kind" "something really big show")))
      ;; Mixed vars:
      (is (= '{*silly "any old" +kind "really big show"}
             (match "*silly +kind" "any old really big show")))
      (is (= '{*silly "reading" +kindle "Book 2"}
             (match "*silly +kindle" "reading Book 2")))
      ;; Multi-occurrence vars:
      (is (= '{+kindle "Book 2"} (match "+kindle foo +kindle" "Book 2 foo Book 2")))
      (is (= nil (match "+kindle foo +kindle" "Book 2 foo Book 1")))
      (is (= '{+kinder "something" +kind "really big show"}
             (match "+kinder +kind +kinder" "something really big show something")))
      ;; Addressing subkinds:
      (add-subkind 'kind 'kinder)
      (is (= '{+kind "something"}
             (match "+kind" "something")))
;;; Annotated +vars:
      (is (= '{+kind "really", +how "something really big"}
             (match "+kind (:optional [+how {:kind kinder}])" "really something really big")))
      (is (= nil
             (match "+kind (:optional [+how {:kind kinder}])" "really not at all")))
      (is (= '{+kind_0 "really"}
             (match "[+kind_0 {:kind kind}]" "really")))
      (is (= '{+kind_0 "really big show"}
             (match "[+kind_0 {:kind kind}]" "really big show")))
      (is (= '{+kinder_a "something" +kind_0 "really big show"}
             (match "[+kinder_a {:kind kinder}] [+kind_0 {:kind kind}]"
                    "something really big show")))
      ;; Mixed vars:
      (is (= '{*silly "any old" +kind_0 "really big show"}
             (match "*silly [+kind_0 {:kind kind}]" "any old really big show")))
      (is (= '{*silly "reading" +kindle_bar "Book 2"}
             (match "*silly [+kindle_bar {:kind kindle}]" "reading Book 2")))
      ;; Multi-occurrence vars:
      (is (= '{+kindle_bar "Book 2"} (match "[+kindle_bar {:kind kindle}] foo +kindle_bar"
                                            "Book 2 foo Book 2")))
      (is (= nil (match "[+kindle_bar {:kind kindle}] foo +kindle_bar"
                        "Book 2 foo Book 1")))
      (is (= '{+kinder_a "something" +kind_0 "really big show"}
             (match "[+kinder_a {:kind kinder}] [+kind_0 {:kind kind}] +kinder_a"
                    "something really big show something")))
      ;; Multiple +vars of a kind.
      (is (= '{+kind_0 "really", +kind_1 "John"}
             (match "[+kind_0 {:kind kind}] [+kind_1 {:kind kind}] " "really John"))))
    ;; Daynamically created *vars:
    (is (= '{*happy "really"} (match "[*happy {:kind kind}]" "really")))
    (do (install-kind-instance-map '{})
      (is (= '{*found "Register", +finding "register"}
             (match "[*found {:kind finding}] this +finding" "Register this register."))))
    ;; Token cardinality specs:
    (is (= '{*foo "this", *bar "little test"}
           (match "[*foo {:max-tokens 1}] *bar" "this little test")))
    (is (= '{*foo "this little", *bar "test"}
           (match "[*foo {:max-tokens 2}] *bar" "this little test")))
    (is (= '{*foo "this little", *bar "test"}
           (match "*foo [*bar {:min-tokens 1}]" "this little test")))
    (is (= '{*foo "this", *bar "little test"}
           (match "*foo [*bar {:min-tokens 2}]" "this little test")))
    (is (= nil (match "*foo [*bar {:min-tokens 3}]" "this little test")))
    (do (install-kind-instance-map '{})
      (is (= '{*found "Register", +finding "register"}
             (match "[*found {:kind finding :max-tokens 1}] this +finding"
                    "Register this register."))))
    ;; Optional vars:
    (is (= {} (match "(:optional *bar)" "")))
    (is (= '{*foo "Hello Dolly"}
           (match "*foo (:optional *bar)" "Hello, Dolly!")))
    (is (= '{*foo "Hello"}
           (match "*foo (:optional *bar) (:optional *baz)"
                  "Hello!")))
    (is (= '{*bar "Hello", *foo "Dolly"}
           (match "(:optional *bar) *foo" "Hello, Dolly!")))
    (is (= '{*bar "Hello", *foo "Dolly"}
           (match "(:optional *bar) (:optional *baz) *foo"
                  "Hello, Dolly!")))
    (do (install-kind-instance-map '{kind   #{"really" "really big show"
                                              "John" "John Smith"}
                                     kinder #{"kinder" "kinder gentler"
                                              "something" "something really big"}
                                     kindle #{"Book 1" "Book 2"}})
      (is (= '{*rest "Register this item"}
             (match "(:optional +kind) *rest"
                    "Register this item."))))
    ;; Inline kind:
    (is (= '{+fruits "apples"}
           (match "[+fruits {:kind #{\"apples\" \"pumpkins\" \"pears\"}}] to +fruits"
                  "apples to apples")))
    (is (= '{+fruits "apples", +nuts "filberts"}
           (match "[+fruits {:kind #{\"apples\" \"pumpkins\" \"pears\"}}] to
                            [+nuts {:kind #{\"Brazils\" \"almonds\" \"filberts\"}}]"
                  "apples to filberts")))
    ;; List template:
    (is (= '{+fruits "apples", +nuts "filberts"}
           (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}] to
                    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
                  "apples to filberts")))
    (is (= '{+fruits "apples", *to "to", +nuts "filberts"}
           (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}] *to
                    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
                  "apples to filberts")))
    (is (= '{+fruits "apples", +nuts "filberts"}
           (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}] to the
                    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
                  "apples to the filberts")))
    (is (= nil
           (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}] "*to" the
                    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
                  "apples to the filberts")))
    (is (= '{+fruits "apples", +nuts "filberts"}
           (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}] "*to" the
                    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
                  "apples *to the filberts")))
    (is (= '{+fruits "apples", +nuts "filberts"}
           (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}] "#to" the
                    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
                  "apples #to the filberts")))
    (is (= '{+fruits "apples", +nuts "filberts"}
           (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}] "nil"
                    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
                  "apples nil filberts")))
    (is (= nil (match '(nil *foo) "nil false")))
    (is (= '{*foo "false"} (match '("nil" *foo) "nil false")))
    ;; Restrictions:
    (is (= nil (match '(*foo [*bar {:finally? (not= *bar *foo)}])
                      "ho ho")))
    (is (= '{*foo "ho", *bar "ho"}
           (match '(*foo [*bar {:finally? (= *bar *foo)}])
                  "ho ho")))
    (is (= '{*foo "ho", *bar "ho"}
           ;; It's bad style (inefficient) for a restriction not to
           ;; refer to the current var, of course.
           (match '(*foo [*bar {:always? (< (count-tokens *foo) 2)}])
                  "ho ho")))
    (is (= '{*foo "ho", *bar "ho ho"}
           ;; It's bad style (inefficient) for a restriction not to
           ;; refer to the current var.
           (match '(*foo [*bar {:always? (< (count-tokens *foo) 2)}])
                  "ho ho ho")))
    ;; Shorthand restrictions:
    (is (= '{*int "29"}
           (match '([*int (digits-alone {} *int)]) "29")))
    (is (= nil
           (match '([*int (digits-alone {} *int)]) "no")))
    (is (= nil
           (match '(*foo [*bar (different-when-bound {} *foo *bar)])
                  "ho ho")))
    (is (= '{*foo "ho", *bar "ho"}
           (match '(*foo [*bar (same-when-bound {} *foo *bar)])
                  "ho ho")))
    (is (= '{*ints "29 30"}
           (match '([*ints (digits-along {} *ints)]) "29 30")))
    (is (= nil
           (match '([*ints (digits-along {} *ints)]) "29 no")))
    (is (= nil
           (match '([*int (n-digits-alone {:finally? (> (read-string *int) 99)}
                                          *int
                                          3)])
                  "1001")))
    (is (= nil
           (match '([*int (n-digits-alone {:finally? (> (read-string *int) 99)}
                                          *int
                                          3)])
                  "001")))
    (is (= '{*int "101"}
           (match '([*int (n-digits-alone {:finally? (> (read-string *int) 99)}
                                          *int
                                          3)])
                  "101")))
    ;; Actions:
    (binding [token-matcher.core-test/*test-atom* (atom nil)]
      (match '([*foo {:finally. (reset! token-matcher.core-test/*test-atom*
                                        *foo)}])
             "bar")
      (is (= "bar" @token-matcher.core-test/*test-atom*)))
    (binding [token-matcher.core-test/*test-atom* (atom nil)]
      (match '(no [*foo {:finally. (reset! token-matcher.core-test/*test-atom*
                                           *foo)}])
             "bar")
      (is (= nil @token-matcher.core-test/*test-atom*)))
    ;; Case sensitivity.
    (binding [*case-sensitive* true]
      (is (= nil (match "Foo" "foo")))
      (is (= {} (match "(:-case Foo)" "foo")))
      (is (= nil (match "(:+case Foo)" "foo")))
      (is (= nil (match "(:-case Foo) Bar" "foo bar")))
      (is (= {} (match "(:-case Foo) (:-case Bar)" "foo bar")))
      (do (install-kind-instance-map '{kind #{"really" "really big show"
                                              "John" "John Smith"}})
        (is (= nil (match "+kind" "john")))))
    (binding [*case-sensitive* false]
      (is (= {} (match "Foo" "foo")))
      (is (= nil (match "(:+case Foo)" "foo")))
      (is (= {} (match "(:-case Foo)" "foo")))
      (is (= nil (match "(:+case Foo) Bar" "foo bar")))
      (is (= nil (match "(:+case Foo) (:+case Bar)" "foo bar"))))
    ;; Optional content:
    (is (= {} (match '((:optional foo)) "")))
    (is (= {} (match '((:optional *foo)) "")))
    (is (= {} (match '((:optional foo) bar) "bar")))
    (is (= {} (match '(foo (:optional bar)) "foo")))
    (is (= {} (match '(foo (:optional *bar)) "foo")))
    (is (= '{*bar "bar"} (match '(foo (:optional *bar)) "foo bar")))
    (is (= '{*bar "bar"} (match '(foo (:optional *bar stool)) "foo bar stool")))
    (is (= nil (match '(foo (:optional *bar stool)) "foo bar")))
    (is (= {} (match '(foo (:optional *bar stool)) "foo")))
    (is (= '{*quack "foo"}
           (match '((:optional *bar stool)
                    [*quack (different-when-bound {} *quack *bar)])
                  "foo")))
    (is (= '{*bar "bar", *quack "quack"}
           (match '((:optional *bar stool)
                    [*quack (different-when-bound {} *quack *bar)])
                  "bar stool quack")))
    (is (= '{*quack "bar stool bar"}
           (match '((:optional *bar stool)
                    [*quack (different-when-bound {} *quack *bar)])
                  "bar stool bar")))
    ;; Choices:
    (is (= {} (match '((:choice foo bar)) "bar")))
    (is (= '{*foo "bar"} (match '((:choice *foo bar)) "bar")))
    (is (= '{*foo "bar"} (match '(some (:choice *foo bar)) "some bar")))
    ;; Series:
    (is (= '{*foo "bar"} (match '(:series *foo) "bar")))
    (is (= '{*foo "bar"} (match '((:series *foo)) "bar")))
    ;; Control combos:
    (is (= '{*foo "bar"} (match '(:series (:optional *foo)) "bar")))
    (is (= '{*foo "foo bar"} (match '(:series (:optional *foo)) "foo bar")))
    (is (= '{*foo "foo bar"}
           (match '(:series (:optional (:choice *foo))) "foo bar")))
    (is (= nil (match '(:series (:optional (:choice foo))) "Foo bar")))
    (binding [*case-sensitive* false]
      (is (= nil (match '(:series (:optional (:choice (:+case Foo)))) "foo"))))
    (binding [*case-sensitive* true]
      (is (= {} (match '(:series (:optional (:choice (:-case foo)))) "Foo"))))
    (do (initialize-matcher)
        (add-kind-instance "person" "Alex")
        (add-kind-instance "person" "Bob")
        (add-kind-instance "restricted resource" "Repo 1"))
    (is (= #{"Repo 1" "Alex" "Bob"}
           (get-kind-instances 'thing)))
    (is (= '{*subject "Alex", *predicate "permissioned to", +thing "Repo 1"}
           (match "*subject is *predicate +thing" "\"Alex\" is permissioned to Repo 1")))
    ))

(deftest matches-test
  (testing "matches"
    (initialize-matcher)
    (is (= '#{[{*foo "one", *bar "two three"} #{}]
	      [{*foo "one two", *bar "three"} #{}]}
           (matches "*foo *bar" "one two three")))
    ))

(deftest defn-templaters-test
  ;; We need to refer our used public symbols---in contexts where they
  ;; are quoted---because most testers run in the `user` namespace.
  (refer 'token-matcher.core :only '[defn-templating-symbols defn-templating-strings])
  (testing "defn-templating forms"
    (is (= '(defn list-outer-symbols [phrase]
              (let [bindings-hashmap (token-matcher.core/match '"*front stuff *back" phrase)]
                (when bindings-hashmap
                  (let [*front (token-matcher.core/instance->symbol (get bindings-hashmap '*front))
                        *back (token-matcher.core/instance->symbol (get bindings-hashmap '*back))]
                    (list *front *back)))))
           (macroexpand-1
            '(defn-templating-symbols list-outer-symbols ["*front stuff *back"
                                                          [phrase]]
               (list *front *back)))))
    (comment ; Not suitable here.
      (defn-templating-symbols list-outer-symbols ["*front stuff *back"
                                                   [phrase]]
        (list *front *back))
      (is (= '(make up)
             (list-outer-symbols "make stuff up"))))
    (is (= '(defn list-outer-strings [phrase]
              (let [bindings-hashmap (token-matcher.core/match '"*front stuff *back" phrase)]
                (when bindings-hashmap
                  (let [*front (get bindings-hashmap '*front)
                        *back (get bindings-hashmap '*back)]
                    (list *front *back)))))
           (macroexpand-1 '(defn-templating-strings list-outer-strings ["*front stuff *back"
                                                                        [phrase]]
                             (list *front *back)))))
    (comment ; Not suitable.
      (defn-templating-symbols list-outer-symbols ["*front stuff *back"
                                                   [phrase]]
        (list *front *back))
      (is (= '(make up)
             (list-outer-symbols "make stuff up"))))
    ;; With optional var, list template:
    (is (= '(defn list-elided [phrase]
              (let [bindings-hashmap (token-matcher.core/match '((:optional *foo)) phrase)]
                (when bindings-hashmap
                  (let [*foo (get bindings-hashmap '*foo)]
                    (list *foo)))))
           (macroexpand-1 '(defn-templating-strings list-elided [((:optional *foo))
                                                                 [phrase]]
                             (list *foo)))))
    ))

(deftest with-matching-macros-test
  ;; We need to refer our used public symbols---in contexts where they
  ;; are quoted---because most testers run in the `user` namespace.
  (refer 'token-matcher.core :only '[with-matching-template-symbols])
  (testing "with-matching-macros"
    ;; With optional var, list template:
    (is (= '(clojure.core/let [*foo 'nil] true)
           (macroexpand-1 '(with-matching-template-symbols [((:optional *foo)) ""] true))))
    (is (= true
           (with-matching-template-symbols [((:optional *foo)) ""] true)))
    ))

(comment ; Make the tesetd functions public, to run these tests.
  (deftest isolate-declared-chars-test
    (testing "isolate-chars"
      (is '["(" "foo" "(" "bar" "(" "baz" "like" ")" ")" ")"]
          (isolate-declared-chars "(foo (bar (baz like)))"))
      (is ["(" "foo" "(" "ba" "[" "r" "(" "baz" "li" "]" "ke" ")" ")" ")"]
          (isolate-declared-chars "(foo (ba[r (baz li]ke)))"))
      ))

  (deftest strip-leading-chars-test
    (testing "strip-leading-chars"
      (is (= "" (strip-leading-chars "")))
      (is (= "Foo" (strip-leading-chars "Foo")))
      (is (= "Foo" (strip-leading-chars ",Foo")))
      (is (= "" (strip-leading-chars ".")))
      (is (= "" (strip-leading-chars "?")))))

  (deftest strip-trailing-chars-test
    (testing "strip-trailing-chars"
      (is (= "" (strip-trailing-chars "")))
      (is (= "Foo" (strip-trailing-chars "Foo.")))
      (is (= "Foo" (strip-trailing-chars "Foo")))
      (binding [*allow-trailing-apostrophe* true]
        (is (= "Foo'" (strip-trailing-chars "Foo'")))
        (is (= "Foo'" (strip-trailing-chars "Foo'."))))
      (binding [*allow-trailing-apostrophe* false]
        (is (= "Foo" (strip-trailing-chars "Foo'")))
        (is (= "Foo" (strip-trailing-chars "Foo'."))))))

  (deftest normalize-token-test
    (testing "normalize-token"
      (binding [*token-substitutions* {"happy" "glad"}]
        (is (= "foo" (normalize-token "foo")))
        (is (= "Bob" (normalize-token "Bob")))
        (is (= "glad" (normalize-token "happy")))
        (is (= "glad" (normalize-token "Happy"))))))

  (deftest checking-vars
    (testing "var? functions"
      (is (= true (*var? '*foo)))
      (is (= nil (*var? 'foo)))
      (is (= true (+var? '+foo)))
      (is (= nil (+var? 'foo)))))

  (deftest updated-var-val-test
    (testing "updated-var-val"
      (is "bar bell" (updated-var-val "bar" 'bell))))

  (deftest pruned-by-prefix-test
    (testing "pruned-by-prefix"
      (is (= #{} (pruned-by-prefix #{"f" "ff" "gf"} "x")))
      (is (= #{} (pruned-by-prefix #{"22"} "1"))) ; Handle a too-long prefix.
      (is (= #{"f" "ff"} (pruned-by-prefix #{"f" "ff" "gf"} "f")))
      (is (= #{"f" "ff" "gf"} (pruned-by-prefix #{"f" "ff" "gf"} "")))))

  (deftest guard-template-vars-test
    (testing "guard-template-vars"
      (is (= '{*bar "bar", *a nil, *b nil}
             (guard-template-vars '{*bar "bar"} '(*a *b))))))
  )

