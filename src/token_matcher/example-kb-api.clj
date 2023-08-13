;;; ----------------------------------------------------------------
;;; Knowledge base:

;;; This section could reasonably be construed as generic for any user
;;; of token-matcher that also uses clolog.  See also
;;; `defn-templating-core` and `defn-templating-strings` in section
;;; 'token-matcher interface'.

;;; `*model*` is our knowledge base of Prolog assertions.
(def ^:dynamic *model*
  ;; An atom with a placeholder value.
  (atom {}))

(defn initialize-model []
  (tm/initialize-matcher)
  ;; We initialize `*model*` to grab "kind" (token-matcher type)
  ;; reasoning assertions in `tm/*kind-assertions*`.
  (reset! *model* @tm/*kind-assertions*))

(defmacro with-model [& body]
  ;; Using `cons`, rather than syntax quote (`), to avoid unnecessary
  ;; resolution (i.e., namespace qualification) of symbols in `body`.
  (cons 'binding
        ;; We bind `pl/*assertions*` so that clolog operations will
        ;; refer to `*model*`.  We bind `tm/*kind-assertions*` so that
        ;; the matcher's KB operations refer to `*model*` also.  So,
        ;; all our layers use the same knowledge base.
        (cons '[pl/*assertions* *model*
                tm/*kind-assertions* *model*]
              body)))

;;; Localize our commonly used KB operations to `*model*`.

(defn inquire [answer-template & goals] ; `query` otherwise used below.
  (with-model
    (pl/query answer-template goals)))

;;; clolog v0.2 doesn't have a variadic `assert<-_` function like this
;;; one, which is handy here.
(defn assert&<-_ [& statements]
  (with-model
    ;; Avoid duplicate assertions.
    (pl/assert<-_ statements)))

(defn get-matching-head-assertions [statement-pattern]
  (with-model
    (pl/get-matching-head-assertions statement-pattern)))

;;; Knowledge base ^^
;;; ----------------------------------------------------------------
;;; token-matcher interface:

;;; The next two forms could reasonably be construed as generic, for
;;; any user of token-matcher that also uses clolog.

;;; Upon integrating clolog, we've needed to copy them from
;;; token-matcher's `core.clj` and modify `defn-templating-core` so
;;; that the call to `match` can access our knowledge base (via
;;; `with-model`).  This simple modification aside, comments with
;;; example expansions in the token-matcher file and (more briefly) in
;;; the next section may be of interest.
(defn defn-templating-core [fn-sym input-sym template let-binding-form body]
  ;; Not using syntax quote (`) here, because of symbol namespacing
  ;; issues.
  (cons 'defn
        (cons fn-sym
              (cons [input-sym]
                    (list
                     (list 'let
                           ['bindings-hashmap (list 'with-model ; Specialization.
                                                    (list 'tm/match
                                                          (list 'quote template)
                                                          input-sym))]
                           (cons 'when
                                 (list 'bindings-hashmap
                                       (cons 'let
                                             (cons let-binding-form body))))))))))


(defmacro defn-templating-strings [fn-sym [template [input-sym]] & body]
  (let [let-binding-form (tm/get-let-binding-form-strings
                          (seq (tm/template-vars (tm/parse-template template))))]
    (defn-templating-core fn-sym input-sym template let-binding-form body)))

;;; token-matcher interface ^^
;;; ----------------------------------------------------------------
