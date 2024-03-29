(ns token-matcher.core
  (:require [clolog.core :as pl :refer :all]
            [clojure.pprint :refer [pprint cl-format]]
            [clojure.string :as str]
            [clojure.set]
            [riddley.walk :refer [walk-exprs]]
            ))

;;;;; Preliminaries:
;;;
;;; Source code variables, functions, and macros are defined before
;;; they are mentioned.  For top-down comprehension, perhaps read this
;;; file starting at the bottom?  Section title comments run in both
;;; directions: ":" for reading down, " ^^" for reading up---around a
;;; long line of dashes.  (See the first such a few lines below.)

;;;;; Preliminaries ^^
;;;;;----------------------------------------------------------------
;;;;; Token handling:

(defn parse-string-template
  "Parse string `template`, returning a list template."
  [template]
  (let [r (java.io.PushbackReader. (java.io.StringReader. template))]
    (take-while (complement #{::eof})
                (repeatedly #(read {:eof ::eof} r)))))

(defn- standard-split
  "Split `s` at whitespace chars, returning a vec of (unnormalized)
  tokens."
  [s]
  (str/split (str/triml s) #"\s+"))

(def ^:dynamic *chars-to-isolate*
  "The set of chars to be isolated as single-char tokens."
  ;; Don't remove any of these chars:
  #{\( \) \[ \] \{ \}})

(defn- isolate-declared-chars
  "Perform a standard split, after isolating chars in
  `*chars-to-isolate*` to single-char tokens."
  [s]
  (standard-split (apply str (mapcat (fn [char]
                                       (if (contains? *chars-to-isolate* char)
                                         (list \space char \space)
                                         (list char)))
                                     (seq s)))))

;;; Dynamic, so that a using application can override with
;;; (binding [*chars-to-isolate* ...] ...).
(def ^:dynamic *chars-to-strip*
  "The set of chars normalization will strip when they lead or trail an
  input token."
  #{\, \. \; \: \? \! \" \'})

(defn- strippable? [char]
  (contains? *chars-to-strip* char))

(defn- strip-leading-chars
  "Strip strippable leading chars from token."
  [s]
  (if-not (seq s)
    s
    (if (strippable? (first s))
      ;; Strip off leading character.
      (strip-leading-chars (subs s 1))
      ;; Nothing to strip.
      s)))

(def ^:dynamic *allow-trailing-apostrophe* false)

(defn- strip-trailing-chars
  "Strip strippable trailing chars from token."
  [s]
  (if-not (seq s)
    s
    ;; Is the last character strippable?
    (let [trailing-char (last s)]
      (if (and (strippable? trailing-char)
               (if (and (contains? *chars-to-strip* \')
                        ;; Not going so far for now as to allow only
                        ;; one...
                        *allow-trailing-apostrophe*)
                 (not= trailing-char \')
                 true))
        ;; Strip off trailing character.
        (strip-trailing-chars (subs s 0
                                    (- (count s)
                                       1)))
        ;; Nothing to strip.
        s))))

;;; Use a string-key/value hashmap, e.g.,
;;; {"can't" "cannot", "big" "large"}.  Keys must be lower-case.
;;; TODO: Accommodate upper-case keys.
(def ^:dynamic *token-substitutions* {})

(defn- normalize-token
  "Strip token, apply single-token substitutions."
  [token]
  (let [token (strip-leading-chars token)
        token (strip-trailing-chars token)
        token (or (get *token-substitutions* (str/lower-case token))
                  ;; Leave anything else alone.
                  token)]
    token))

(defn- parse-input-string
  "Return a vector of the non-empty, normalized tokens of `s`."
  [s]
  (let [tokens (isolate-declared-chars s)
        tokens (map normalize-token tokens)
        tokens (filter #(not= % "") tokens)]
    (vec tokens)))

;;;;; Token handling ^^
;;;;; ----------------------------------------------------------------
;;;;; Kind instance registration:

;;; We use clolog to register a kind's instance strings that a
;;; template's +var will match against.

(def ^:dynamic *kind-assertions*
  "The knowledge base of assertions for token-matcher kind reasoning.
  Initialized (in `initialize-kind-assertions`) to the (empty)
  initialized Prolog knowledge base."
  ;; An atom with a placeholder value.
  (atom {}))

(defn initialize-kind-assertions []
  "Clears the kind knowledge base and adds assertions to support
  subkind reasoning."
  (binding [*assertions* *kind-assertions*]
    (initialize-prolog)
    ;; Sample application knoweldge (facts):
    (comment
      (binding [*assertions* *kind-assertions*] 
        (<- (has-subkind "vertebrate" "mammal"))
        (<- (has-subkind "mammal" "primate"))
        (<- (has-subkind "primate" "human"))
        ;; Idea is to handle instance mentions at any level.
        (<- (has-kind "Bob" "vertebrate"))
        (<- (has-kind "Sally" "mammal"))
        (<- (has-kind "Joe" "primate"))
        (<- (has-kind "Freida" "human"))))
    ;; Rules.
    (<- (has-kind* ?instance ?kind)
        (has-kind ?instance ?kind))
    (<- (has-kind* ?instance ?kind)
        (has-subkind* ?kind ?subkind)
        (has-kind ?instance ?subkind))
    ;; `has-subkind` is non-transitive (to avoid infinite recursion) .
    (<- (has-subkind* ?kind ?subkind)
        (has-subkind ?kind ?subkind))
    ;; This assertion of `has-subkind*` is transitive.
    (<- (has-subkind* ?kind ?subsubkind)
        (has-subkind ?kind ?subkind)
        (has-subkind* ?subkind ?subsubkind))))

(defn initialize-matcher []
  "Currently, just initializes the kind knoweldge base."
  (initialize-kind-assertions))

(declare symbol->instance)

(defn get-kind-instances [kind]
  "Returns the registered instances of `kind` (including instances of
  subkinds of `kind`)."
  (binding [*assertions* *kind-assertions*]
    (set (query '?instance
                ;; Introduce `thing`, the universal kind.
                (if (= kind 'thing)
                  '((has-kind ?instance ?_kind))
                  `((~'evals-from? ~'?kind (quote ~kind)) ; TODO: `->?`
                    (~'has-kind* ~'?instance ~'?kind)))))))

(defn add-kind-instance [kind instance]
  "Adds `instance` to `kind`."
  (binding [*assertions* *kind-assertions*]
    (let [assertion `((~'has-kind ~instance ~kind))]
      ;; We want to avoid duplicates.
      (assert<-_ assertion)
      assertion)))

(defn add-subkind [kind subkind]
  "Makes `subkind` a subkind of `kind`."
  (binding [*assertions* *kind-assertions*]
    (let [assertion `((~'has-subkind ~kind ~subkind))]
      ;; Avoid duplicates.
      (assert<-_ assertion)
      assertion)))

(defn install-kind-instance-map [kind-instance-map]
  "Clears the kind knowledge base and asserts instances from a map
  with kind keys and instance vector values.  See
  `test/clolog/core_test.clj`."
  (initialize-kind-assertions)
  (doseq [kind-instances (vec kind-instance-map)]
    (let [[kind instances] kind-instances]
      (doseq [instance instances]
        (add-kind-instance kind instance)))))

;;;;; Kind instance registration ^^
;;;;; ----------------------------------------------------------------
;;;;; Variables:

(defn- plain-*var? [construct]
  (if (and (symbol? construct)
           ;; 'name' here strips down to the simple symbol.
           (= (nth (name construct) 0) \*))
    true
    ;; Harmonize our match functions' results to use nil, rather than
    ;; false.
    nil))

(defn- annotated-*var? [construct]
  (if (and (vector? construct)
           (plain-*var? (first construct)))
    true
    nil))

(defn- *var? [construct]
  (if (or (plain-*var? construct)
          (annotated-*var? construct))
    true
    nil))

(defn- plain-+var? [construct]
  (if (and (symbol? construct)
           (= (nth (name construct) 0) \+))
    true
    nil))

(defn- annotated-+var? [construct]
  (if (and (vector? construct)
           (plain-+var? (first construct)))
    true
    nil))

(defn- +var? [construct]
  (if (or (plain-+var? construct)
          (annotated-+var? construct))
    true
    nil))

(defn- annotated-var? [construct]
  (or (annotated-*var? construct)
      (annotated-+var? construct)))

(defn- plain-var? [construct]
  (or (plain-*var? construct)
      (plain-+var? construct)))

;;; Not "var?", so that we don't shadow clojure.core/var?.
(defn- tm-var? [construct]
  (or (*var? construct) (+var? construct)))

(defn- plain-var
  "Return the plain var symbol of a potentially annotated var."
  [var]
  (if (annotated-var? var)
    (first var)
    var))

;;; For var, e.g., '+foo', return symbol 'foo'.
(defn- var->root-symbol
  "Return the symbol resulting from stripping the leading `*` or `+`
  from a var symbol's name."
  [var] ; *var or +var
  (clojure.edn/read-string (subs (str var) 1)))

(defn- +var-kind [var]
  (if (annotated-+var? var)
    (:kind (second var))
    (var->root-symbol var)))

(defn- +var-kind-instances [var]
  (let [kind (+var-kind var)]
    (if (set? kind)
      ;; Inline kind instances (no registered kind name).
      kind
      ;; Registered kind usage.
      (when kind (get-kind-instances kind)))))

(defn- *var-kind [var]
  (if (annotated-*var? var)
    (:kind (second var))
    nil))

(defn- var-kind [var]
  (if (+var? var)
    (+var-kind var)
    ;; Else.
    (*var-kind var)))

(defn- var-attributes [var]
  (when (annotated-var? var)
    (or (second var) {})))

;;;;; Variables ^^
;;;;; ----------------------------------------------------------------
;;;;; Attribute short-hand functions:

;;; [*foo (num-tokens-range {} 1 3)]
(defn num-tokens-range [attrs min max]
  (assoc (assoc attrs :min-tokens min) :max-tokens max))

;;; 'earlier' to be evaluated before 'later'.
(defn conjoin-restrictions [earlier later]
  (if (nil? later)
    earlier
    (if (nil? earlier)
      later
      ;; Else neither is nil.
      (let [later-conjuncts (if (= (first later) 'and)
                              (rest later)
                              (list later))
            earlier-conjuncts (if (= (first earlier) 'and)
                                (rest earlier)
                                (list earlier))]
        `(~'and ~@earlier-conjuncts ~@later-conjuncts)))))

(defn adjoin-actions [later earlier]
  (if (nil? later)
    earlier
    (if (nil? earlier)
      later
      ;; Else neither is nil.
      (let [later-statements (if (= (first later) 'do)
                               (rest later)
                               (list later))
            earlier-statements (if (= (first earlier) 'do)
                                 (rest earlier)
                                 (list earlier))]
        `(~'do ~@earlier-statements ~@later-statements)))))

(defn digit-string? [s]
  ;; Encapsulate the regex inside this function, because expressions
  ;; involving regexes are hard to test (because regexes have only
  ;; identity equality).
  (re-matches #"\d*" s))

;;; Restrict 'this' to a single, all-digit token.
(defn digits-alone [attrs this]
  (assoc (assoc attrs :max-tokens 1)
         :finally? (conjoin-restrictions `(digit-string? ~this)
                                         (:finally? attrs))))

;;; Restrict 'this' to a single token of exactly n digits.
(defn n-digits-alone [attrs this n]
  (assoc (assoc attrs :max-tokens 1)
         :finally? (conjoin-restrictions
                    `(~'and ; Standardize 'and'.
                      (digit-string? ~this)
                      (= (count ~this) ~n))
                    (:finally? attrs))))

(defn same-when-bound [attrs this other]
  (assoc attrs
         :finally? (conjoin-restrictions `(if (and ~other ~this)
                                            (= ~this ~other)
                                            true)
                                         (:finally? attrs))))

(defn different-when-bound [attrs this other]
  (assoc attrs
         :finally? (conjoin-restrictions `(if (and ~other ~this)
                                            (not= ~this ~other)
                                            true)
                                         (:finally? attrs))))

;;; To qualify a series of (comma-free, natural) integers.
(defn digits-along [attrs this]
  (assoc attrs
         :each? (conjoin-restrictions `(digit-string? ~this)
                                      (:each? attrs))))
;;; The ':each?' definition above is more efficient than the
;;; ':finally?'  version commented out below.
(comment
  (defn digits-along [attrs this]
    (assoc attrs
           :finally? (conjoin-restrictions `(every? (digit-string? ~this)
                                                    (instance->tokens ~this))
                                           (:finally? attrs)))))

;;;;; Attribute short-hand functions ^^
;;;;; ----------------------------------------------------------------
;;;;; Template matcher (core):

(defn- expand-short-hand-attributes [annotated-var]
  (let [var-sym (first annotated-var)
        attrs (var-attributes annotated-var)]
    (if (seq? attrs)
      (let [fn-sym (first attrs)
            args (rest attrs)]
        ;; Quote the shorthand function's var args.
        [var-sym (eval `(~fn-sym
                         ~@(map #(list 'quote %)
                                args)))])
      annotated-var)))

(defn parse-template [template]
  (let [template (if (string? template)
                   (parse-string-template template)
                   template)
        template (if (= (first template) :series)
                   (list template)
                   template)]
    (walk-exprs annotated-var?
                expand-short-hand-attributes
                template)))

;;; We have an already-in-process var.
(defn- updated-var-val [current-val with-token]
  (if (= current-val "")
    with-token
    (str current-val " " with-token)))

(def ^:dynamic *case-sensitive* false)

(defn- -case-instance= [template-construct input-token]
  (= (str/lower-case
      ;; Can be called on empty lists.
      (or template-construct ""))
     (str/lower-case (or input-token ""))))

(defn- +case-instance= [template-construct input-token]
  ;; No case deviation tolerance.
  (= template-construct input-token))

(defn- instance= [template-construct input-token]
  (if *case-sensitive*
    (+case-instance= template-construct input-token)
    (-case-instance= template-construct input-token)))

(defn- hasPrefix [s prefix]
  (let [string-size (count s)
        prefix-size (count prefix)]
    (when (>= string-size prefix-size)
      (instance= (subs s 0 prefix-size) prefix))))

;;; Later, search using a trie.  For now, we'll brute-force
;;; search (what's left of) the (selected-down) set.
;;;
;;; Return the subset of strings (also a set) having the prefix.
(defn- pruned-by-prefix [set-of-strings prefix]
  (clojure.set/select (fn [item] (hasPrefix item prefix))
                      set-of-strings))

;;; Given a +var, we'd like to bind some match of a kind instances.
;;;
;;; We could prefer a longest or a shortest going-forward match.  Here
;;; we've gone with longest.
;;;
;;; We must examine one input token at a time, traversing the input
;;; string up to next-template-construct, if that's not a var, or calling
;;; match-current-var, if it is a var.  Either way, we must augment
;;; bindings as we go.
;;;
;;; Compared to handling a *var, handling a +var requires...
;;;
;;; - Initially calling with a freshly looked up current-+set
;;;
;;; - Failing when current-+set is empty
;;;
;;; - Failing rather than recursing when current-var's value on
;;;   bindings is not in current-+set.  This saves us from a partial
;;;   match (e.g., "foo" when only "foo bar" is in the set.
;;;
;;; - Pruning current-+set upon consuming an input token.

;;; This clearly won't scale.  We'd ultimately like an instance
;;; retrieval facility to handle case sensitivity.
(defn- downcase-set [s]
  (into #{} (map str/lower-case s)))

;;; Returns nil, if current-+set should not be active.
(defn- initialize-current-+set [current-var bindings]
  (if-let [current-binding (get bindings (plain-var current-var))]
    #{(str/lower-case current-binding)} ; Have to match later binding.
    (when (+var? current-var)
      (when-let [instances (+var-kind-instances current-var)]
        (if *case-sensitive*
          instances
          (downcase-set instances))))))

(defn- var-attribute [var attribute]
  (when (annotated-var? var)
    (get (second var) attribute)))

(defn- instance->tokens [instance]
  (filter #(= % \space) instance))

(defn count-tokens [instance] ; current-val
  (+ 1 (count (instance->tokens instance))))

(def ^:private ^:dynamic *template-vars* #{})

(defn template-vars [template]
  "Public for application integration."
  (let [vars (atom #{})]
    (walk-exprs tm-var?
                #(swap! vars conj (plain-var %))
                template)
    @vars))

;;; Called with seq of unique elements.  Bind any unbound template
;;; vars to nil---to support (e.g.) different-when-bound (because
;;; Clojure barfs on code with free names).
;;;
;;; Arguably more efficiently done at parse time, but (for now,
;;; anyway) we'd like parsed templates cleaner, for tracing/debugging.
(defn- guard-template-vars [bindings template-vars]
  (let [var (first template-vars)
        template-vars (rest template-vars)
        bindings (if (get bindings var)
                   bindings
                   (assoc bindings var nil))]
    (if (empty? template-vars)
      bindings
      (guard-template-vars bindings template-vars))))

;;; This should handle any expression using bound tm-vars and Clojure vars.
(defn- pass-restriction? [bindings-hashmap restriction]
  (let [bindings-hashmap (guard-template-vars bindings-hashmap (seq *template-vars*))
        bindings-list (vec (flatten (vec bindings-hashmap)))]
    ;; Evaluate the restriction.
    (eval `(let ~bindings-list
             ~restriction))))

(defn- do-action [bindings-hashmap action]
  (let [bindings-hashmap (guard-template-vars bindings-hashmap (seq *template-vars*))
        bindings-list (vec (flatten (vec bindings-hashmap)))]
    ;; Evaluate the action.
    (eval `(let ~bindings-list
             ~action))))

(defn- qualify-attributes-always? [current-var current-val bindings]
  (and (let [max-tokens (var-attribute current-var :max-tokens)]
         (or (nil? max-tokens)
             (<= (count-tokens current-val) max-tokens)))
       (let [restriction (var-attribute current-var :always?)
             bindings (assoc bindings (plain-var current-var) current-val)]
         (or (nil? restriction)
             (pass-restriction? bindings restriction)))
       ))

(defn- qualify-attributes-each? [current-var current-token bindings]
  (let [restriction (var-attribute current-var :each?)
        ;; Not really legit bindings (since not current-val), but it's
        ;; what we'll check.
        bindings (assoc bindings (plain-var current-var)
                        current-token)]
    (or (nil? restriction)
        (pass-restriction? bindings restriction))))

(defn- qualify-always? [current-var current-val current-token bindings
                       current-+set]
  (and (qualify-attributes-always? current-var current-val bindings)
       (qualify-attributes-each? current-var current-token bindings)
       (or (and (*var? current-var)
                ;; *var ok, unless already on bindings.
                (not (get bindings (plain-var current-var))))
           ;; +var / any already-bound var needs surviving instances.
           (not (empty? current-+set)))))

(defn- qualify-attributes-finally? [current-var current-val bindings]
  (and (qualify-attributes-always? current-var current-val bindings)
       (let [min-tokens (var-attribute current-var :min-tokens)]
         (or (nil? min-tokens)
             (>= (count-tokens current-val) min-tokens)))
       (let [restriction (var-attribute current-var :finally?)
             bindings (assoc bindings (plain-var current-var) current-val)]
         (or (nil? restriction)
             (pass-restriction? bindings restriction)))
       ))

(defn- qualify-finally? [current-var current-val bindings current-+set]
  (and (qualify-attributes-finally? current-var current-val bindings)
       ;; Qualify +var instance.
       (or (and (*var? current-var)
                ;; *var ok, unless already on bindings.
                (not (get bindings current-var)))
           ;; +var / any already-bound var: fail when
           ;; current-val is not in current-+set.
           (contains? current-+set (str/lower-case current-val)))))

(defn- assert-kind-var-val [current-var current-val]
  (let [assertion (and (annotated-*var? current-var)
                       (let [kind (*var-kind current-var)]
                         (when kind
                           (add-kind-instance kind current-val))))]
    assertion))

(defn- updated-current-+set [current-+set current-var current-val bindings]
  (when (or (get bindings (plain-var current-var))
            (+var? current-var))
    ;; So, (set? current-+set).
    (pruned-by-prefix current-+set current-val)))

;;; We use this macro to modularize 'match-...'  functions
;;; below, to provide a single point of maintenance and avoid typing
;;; this same preamble stuff more than once.
(defmacro ^:private def-match-fn [fn-name & body]
  ;; Can't abide syntax quote's symbol namespacing here.  So, consing.
  (cons 'defn-
        (cons fn-name
              (concat '([template-constructs
                         input-tokens
                         & {:keys [bindings
                                   assertions ; Made during this call to `match`.
                                   current-var
                                   current-val
                                   ;; For +vars, only.  A bit cheesy, but okay.
                                   current-+prefix
                                   current-+set
                                   ;; For future sequence context vars.  See README.md.
                                   ;; context-var
                                   ;; context-val
                                   ]
                            ;; The keyword arguments' default values.
                            :or {bindings {} ; Excepting current-val.
                                 assertions #{} ; What we'll say we've added.
                                 current-var nil
                                 current-val ""
                                 ;; The growing instance (maybe-multi-token string).
                                 current-+prefix ""
                                 current-+set nil
                                 ;; For future.
                                 ;; context-var
                                 ;; context-val
                                 }}])
                      body))))

(declare match-constructs)
(declare match-var)

(def-match-fn match-var-always
  (when-not (empty? input-tokens)
    (let [current-token (first input-tokens)]
      (when (qualify-always? current-var current-val current-token bindings current-+set)
        (let [current-val (updated-var-val current-val current-token)
              current-+set (updated-current-+set current-+set current-var current-val bindings)]
          (match-var template-constructs (rest input-tokens)
                     :bindings bindings
                     :assertions assertions
                     :current-var current-var
                     :current-val current-val
                     :current-+set current-+set))))))

(def-match-fn match-var-finally
  (when (qualify-finally? current-var current-val bindings current-+set)
    ;; Say we're done (recording current var/val on
    ;; bindings) and try the next var.
    ;;
    ;; Arrange for our updates to *kind-assertions* to be unwound upon
    ;; backtracking.
    (binding [*kind-assertions* (atom @*kind-assertions*)]
      (let [bindings (assoc bindings (plain-var current-var) current-val)
            assertion (assert-kind-var-val current-var current-val)
            assertions (if assertion
                         (conj assertions assertion)
                         assertions)
            action (var-attribute current-var :finally.)]
        ;; Perform any specified action.
        (or (nil? action)
            (do-action bindings action))
        ;; Keep matching.
        (match-constructs template-constructs input-tokens
                          :bindings bindings
                          :assertions assertions)))))

;;; We have an already-in-process var.
(def-match-fn match-var
  (or
   ;; Prefer a longer binding for the current var---update the
   ;; current value and chug along.
   (match-var-always template-constructs input-tokens
                     :bindings bindings
                     :assertions assertions
                     :current-var current-var
                     :current-val current-val
                     :current-+set current-+set)
   ;; Reverse the surrounding disjuncts (at both levels) to bind a
   ;; var to the shortest possible remaining token sequence.
   (match-var-finally template-constructs input-tokens
                      :bindings bindings
                      :assertions assertions
                      :current-var current-var
                      :current-val current-val
                      :current-+set current-+set)))

(def-match-fn match-+case
  (let [within-+case-scope (rest (first template-constructs))
        beyond-+case-scope (rest template-constructs)]
    (if *case-sensitive*
      ;; :+case is a no-op.
      (match-constructs `(~@within-+case-scope ~@beyond-+case-scope)
                        input-tokens
                        :bindings bindings
                        :assertions assertions)
      (binding [*case-sensitive* true]
        (let [-case-form (when-not (empty? beyond-+case-scope)
                           ;; Elide if empty, else wrap.
                           `((:-case ~@beyond-+case-scope)))]
          (match-constructs `(~@within-+case-scope ~@-case-form)
                            input-tokens
                            :bindings bindings
                            :assertions assertions))))))

(def-match-fn match--case
  (let [within--case-scope (rest (first template-constructs))
        beyond--case-scope (rest template-constructs)]
    (if-not *case-sensitive*
      ;; :-case is a no-op.
      (match-constructs `(~@within--case-scope ~@beyond--case-scope)
                        input-tokens
                        :bindings bindings
                        :assertions assertions)
      (binding [*case-sensitive* false]
        (let [+case-form (when-not (empty? beyond--case-scope)
                           ;; Elide if empty, else wrap.
                           `((:+case ~@beyond--case-scope)))]
          (match-constructs `(~@within--case-scope ~@+case-form)
                            input-tokens
                            :bindings bindings
                            :assertions assertions))))))

(def-match-fn match-optional
  (let [within-optional-scope (rest (first template-constructs))
        beyond-optional-scope (rest template-constructs)]
    (or
     ;; First try for (prefer) non-empty content.
     (match-constructs `(~@within-optional-scope ~@beyond-optional-scope)
                       input-tokens
                       :bindings bindings
                       :assertions assertions)
     ;; Next try empty content.
     (match-constructs beyond-optional-scope input-tokens
                       :bindings bindings
                       :assertions assertions))))

(def-match-fn match-series
  (let [within-series-scope (rest (first template-constructs))
        beyond-series-scope (rest template-constructs)]
    (match-constructs `(~@within-series-scope ~@beyond-series-scope)
                      input-tokens
                      :bindings bindings
                      :assertions assertions)))

(def-match-fn match-choice
  (let [within-choice-scope (rest (first template-constructs))
        beyond-choice-scope (rest template-constructs)]
    (when-not (empty? within-choice-scope) ; Fail if we've exhausted choices.
      (or (match-constructs `(~(first within-choice-scope) ~@beyond-choice-scope)
                            input-tokens
                            :bindings bindings
                            :assertions assertions)
          ;; Skip the call to match-constructs, since we know what's coming.
          (match-choice `((:choice ~@(rest within-choice-scope))
                          ~@beyond-choice-scope)
                        input-tokens
                        :bindings bindings
                        :assertions assertions)))))

(def-match-fn match-control
  (let [controlled (first template-constructs)
        control (first controlled)]
    (apply (case control
             :+case match-+case
             :-case match--case
             :optional match-optional
             :series match-series
             :choice match-choice)
           [template-constructs input-tokens
            :bindings bindings
            :assertions assertions])))

(def ^:private ^:dynamic *matches* (atom #{}))
(def ^:private ^:dynamic *matcher-assertions* (atom #{}))
(def ^:private ^:dynamic *matches-countdown* nil)
(def ^:private ^:dynamic *assert-unique-match* false)

;;; Match input-string against template, creating (generally,
;;; multi-token) "instance" bindings for template wildcard symbols.
;;; Returns nil if the does template not match, else a binding
;;; hashmap, perhaps empty.
(def-match-fn match-constructs
  (let [template-item (first template-constructs)]
    (cond
      (seq? template-item)
      (match-control template-constructs input-tokens
                     :bindings bindings
                     :assertions assertions)

      (and (empty? template-constructs)
           (empty? input-tokens))
      ;; Succeed (record match and backtrack).
      (let [answer [bindings assertions]] 
        (if (contains? @*matches* answer)
          nil ; Fail
          (do (swap! *matches* conj [bindings assertions])
              (when *matches-countdown*
                (swap! *matches-countdown* dec))
              (if (and *matches-countdown*
                       (= @*matches-countdown* 0))
                (do (when *assert-unique-match* ; *matches-countdown* started at 2.
                      (assert false (cl-format nil "Multiple matches found: ~s" @*matches*)))
                    ;; Return something truthy, unwinding all matching calls.
                    [bindings assertions])
                ;; Else fail, for backtracking.
                nil))))

      ;; Else at least one of template, input still has tokens.
      ;; Optional vars handled at bottom.
      (or (empty? template-constructs)
          (empty? input-tokens))
      ;; Fail if they don't both still have tokens.
      nil

      ;; Else both have tokens.  Make sure they match.
      (and (not (tm-var? template-item))
           (instance= (str template-item) ; Accommodate an easy symbol.
                      (first input-tokens)))
      ;; The front tokens match outright, so check the rest.
      (match-constructs (rest template-constructs) (rest input-tokens)
                        :bindings bindings
                        :assertions assertions)

      :else
      ;; They don't match outright.  Can we find a matching variable binding?
      (when (tm-var? template-item)
        (let [current-var template-item
              current-val (first input-tokens)
              current-+set (initialize-current-+set current-var bindings)]
	  (match-var (rest template-constructs) (rest input-tokens)
                     :bindings bindings
                     :assertions assertions
                     :current-var current-var
                     :current-val current-val
                     :current-+set current-+set))))))

;;; The application-level interface to match-constructs:

;;; Single-match interface.
(defn match-details [template input-string]
  (let [parsed-template (parse-template template)]
    (binding [*matches* (atom #{})
              *matches-countdown* (atom 1)
              *template-vars* (template-vars parsed-template)]
      ;; Returns the last match found.
      (match-constructs parsed-template
                        (parse-input-string input-string))
      ;; Not needed: (nth @*matches* 0)
      )))

;;; Used in most tests.
(defn match [template input-string]
  (let [[bindings _assertions] (match-details template input-string)]
    bindings))

(defn match-uniquely [template input-string]
  (let [parsed-template (parse-template template)]
    (binding [*matches* (atom #{})
              *matches-countdown* (atom 2)
              *assert-unique-match* true
              *template-vars* (template-vars parsed-template)]
      (match-constructs parsed-template
                        (parse-input-string input-string))
      (first @*matches*))))

;;; Multi-match interface.
(defn matches
  ([template input-string]
   (matches template input-string nil))
  ;; 'limit' should be a positive integer.
  ([template input-string limit]
   (let [parsed-template (parse-template template)]
     (binding [*matches* (atom #{})
               *matches-countdown* (when limit (atom limit))
               *template-vars* (template-vars parsed-template)]
       (match-constructs parsed-template
                         (parse-input-string input-string))
       ;; A using application should decide what to do with these, when
       ;; *matches* has more than one entry.
       @*matches*))))

;;; Versions for pre-parsed templates (so that they may be applied
;;; many times, without re-parsing):

(defn match-pre-parsed [parsed-template input-string]
  (binding [*matches* (atom #{})
            *matches-countdown* (atom 1)
            *template-vars* (template-vars parsed-template)]
    ;; Returns the last match found.
    (match-constructs parsed-template
                      (parse-input-string input-string))
    ;; Not needed: (nth @*matches* 0)
    ))

(defn matches-pre-parsed
  ([parsed-template input-string]
   (matches-pre-parsed parsed-template input-string nil))
  ;; 'limit' should be a positive integer.
  ([parsed-template input-string limit]
   (binding [*matches* (atom #{})
             *matches-countdown* (when limit (atom limit))
             *template-vars* (template-vars parsed-template)]
     (match-constructs parsed-template
                       (parse-input-string input-string))
     @*matches*)))

;;;;; Template matcher (core) ^^
;;;;; ----------------------------------------------------------------
;;;;; Template-matching defining forms:

;;; A matcher-returned binding is a string of tokens, e.g., "foo
;;; bar" (or "foo").  When our application used standard
;;; Prolog---whose predicates must be symbols---we needed the
;;; following two functions.
(defn instance->symbol [s]
  ;; Return a corresponding symbol, e.g., `foo_bar` (or `foo`).
  (when s ; Handle elided optional/chioce-scope vars.
    (clojure.edn/read-string (str/replace s " " "_"))))

;;; Invert the above.
(defn symbol->instance [symbol]
  ;; This assumes \_ never occurs in an (e.g., predicate) token.
  (str/replace (name symbol) "_" " "))

;;; For template matcher users/applications, we provide two macros to
;;; define functions automatially binding the variables of a fixed
;;; template and accepting a string input parameter, such that these
;;; variables' values---when matched against the template for a given
;;; input---can be referenced in the function body.  One macro
;;; provides these values as plain strings, the other as symbols---as
;;; illustrated in the long comment below.
(comment

  ;; Define your function.
  (defn-templating-symbols list-outer-symbols ["*front stuff *back"
                                               [phrase]]
    (list *front *back))

  ;; Ask to see an equivalent plan--defn form.
  (macroexpand-1 '(defn-templating-symbols list-outer-symbols ["*front stuff *back"
                                                               [phrase]]
                    (list *front *back)))

  ;; Aren't we getting away with a lot less typing (and code
  ;; duplication presenting maintenance issues)?
  (defn list-outer-symbols [phrase]
    (let [bindings-hashmap (match "*front stuff *back" phrase)]
      (when bindings-hashmap
        (let [*front (instance->symbol (get bindings-hashmap '*front))
              *back (instance->symbol (get bindings-hashmap '*back))]
          (list *front *back)))))

  ;; Call it on some input.
  (list-outer-symbols "We've built some stuff here...")

  ;; The result---a list of symbols.

  (We've_built_some here)

  ;; Or, with the -strings versions:

  ;; Defining form:
  (defn-templating-strings list-outer-strings ["*front stuff *back"
                                               [phrase]]
    (list *front *back))

  ;; Invocation:
  (list-outer-strings "We've built some stuff here...")

  ;; Result:
  ("We've built some" "here")

  )
;;; Since integrating clolog in v1.1---whose predicates can be
;;; strings---we use only the `defn-templating-strings` (and not
;;; `defn-templating-symbols`) in our application.  

;;; The macros (defined below) have some helper functions.  We've
;;; fully qualified some symbols to make these forms exportable.  (It
;;; turns out that our clolog integration has required copying this
;;; section of code into our application and modifying
;;; `defn-templating-core` there so that the call to `match` can
;;; access the application's knowledge base.  Our copy wraps that call
;;; with a macro, `with-model`.)

(defn get-let-binding-form-strings [vars]
  "Public for application integration."
  (into [] cat (map (fn [var]
                      [var (list 'get 'bindings-hashmap
                                 (list 'quote var))])
                    vars)))

(defn defn-templating-core [fn-sym input-sym template let-binding-form body]
  ;; Not using syntax quote (`) here, because of symbol namespacing
  ;; issues.
  (cons 'defn
        (cons fn-sym
              (cons [input-sym]
                    (list
                     (list 'let
                           ['bindings-hashmap (list 'token-matcher.core/match
                                                    (list 'quote template)
                                                    input-sym)]
                           (cons 'when
                                 (list 'bindings-hashmap
                                       (cons 'let
                                             (cons let-binding-form body))))))))))


(defmacro defn-templating-strings [fn-sym [template [input-sym]] & body]
  (let [let-binding-form (get-let-binding-form-strings (seq (template-vars (parse-template template))))]
    (defn-templating-core fn-sym input-sym template let-binding-form body)))

;;; Another helper.
(defn- get-let-binding-form-symbols [vars]
  (into [] cat
        (map (fn [var]
               [var (list 'token-matcher.core/instance->symbol ; Fully qualify.
                          (list 'get 'bindings-hashmap
                                (list 'quote var)))])
             vars)))

(defmacro defn-templating-symbols [fn-sym [template [input-sym]] & body]
  (let [let-binding-form (get-let-binding-form-symbols (seq (template-vars (parse-template template))))]
    (defn-templating-core fn-sym input-sym template let-binding-form body)))

;;;;; Template-matching defining forms ^^
;;;;; ----------------------------------------------------------------
;;;;; REPL-only forms:

;;; The next two forms may be useful at the REPL.  They can't
;;; productively be embedded in a function or a macro.

;;; Unless matching fails, execute body under raw template
;;; bindings (strings).
(defmacro with-matching-template-strings [[template input-form]
                                          & body]
  (let [bindings-hashmap (match template input-form)]
    (when bindings-hashmap ; (not= bindings-hashmap nil)
      (let [bindings-hashmap (guard-template-vars bindings-hashmap
                                                  (seq (template-vars (parse-template template))))
            bindings-list (into [] cat bindings-hashmap)]
        `(let ~bindings-list
           ~@body)
        ;; Syntax quote won't penetrate bindings-list to qualify the
        ;; symbols there.  So we don't need this approach, here.
        ;;        (cons 'let
        ;;              (cons bindings-list
        ;;                    body))
        ))))

;;; Helper function.
(defn- instance->quoted-symbol [string]
  (list 'quote (instance->symbol string)))

;;; Unless matching fails, execute body with template-bound strings
;;; converted to symbols.
(defmacro with-matching-template-symbols [[template input-form]
                                          & body]
  (let [bindings-hashmap (match template input-form)]
    (when bindings-hashmap ; (not= bindings-hashmap nil)
      (let [bindings-hashmap (guard-template-vars bindings-hashmap
                                                  (seq (template-vars (parse-template template))))
            symbols-hashmap (update-vals bindings-hashmap instance->quoted-symbol)
            bindings-list (into [] cat symbols-hashmap)]
        `(let ~bindings-list
          ~@body)))))

;;;;; REPL-only forms ^^
;;;;; ----------------------------------------------------------------
