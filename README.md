# token-matcher

We provide token-based pattern matching for Clojure: match a template
containing variables against an input string, creating appropriate
bindings that a using application may process further.

We offer the matcher as one capability towards Clojure-based natural
language processing (NLP) in relatively narrow domains where
hand-crafted solutions may be effective, including for tasks such
as...

- Acquiring knowledge via controlled natural language
- Extracting knowledge from free text
- Standardizing/summarizing conversational input.

In knowledge acquisition and extraction tasks, we have exploited a
sort of bootstrapping process to populate and then refer to named sets
(here called "kinds") of allowed instances (strings of one or more
tokens) for set-linked variables (here called "+vars").  Other
template variables ("*vars") may be bound to instances apart from
reference to any named set, may also be tasked (via annotation) to
extend a named set with their binding---even dynamically, during the
course of processing a single template.

Var annotations also admit raw Clojure expressions used as
"restrictions" that qualify would-be-complete or partial var instances
or as "actions" evaluated for side effects.  We wrap restriction or
action expressions to make template vars' so-far-matched values (using
`nil`, if unmatched) accessible.

We call user- or application-defined "short-hand" functions with var
arguments to help define the attributes of annotated vars.  The
short-hand functions act like macros whose returned 
content our parser splices into a match template before processing.

Template-embedded lists headed by "control" keywords can be nested to
turn on or off token matching case sensitivity and to specify
optional, one-of-choice, or standard token-series content.

Top-level interface macros allow application developers to define a
function of a fixed template and an input string argument, in which
the function body is wrapped similarly to restrictions and actions.

These constructs contribute towards minimizing repetition of
expressions among an application's authored templates, thus enhancing
programmer productivity and product maintainability.

See examples forthcoming next.

We have developed NLP applications using similar (but less versatile)
matching capabilities integrated with logic programming (e.g., Prolog,
Datalog) for knowledge representation and reasoning.  We are working
on a compatible Datalog implementation in Clojure.  We ultimately
expect logical type reasoning to replace the existing kind registry.

We envision other future token matcher work to increase expressivity
for emerging use cases, efficiency, robustness, and scale.

## Example operations

In illustrative examples below, function `match` takes two
arguments---a template and an input string---returning a hashmap of
feasible bindings for template variables (*vars and +vars) where such
exist, else `nil`.  Examples are taken from among tests in
test/token_matcher/core_test.clj.

Sections following these examples present detailed documentation.

### Plain var examples

```clojure
> (match "foo" "foo")
{} ; Empty hashmap.

> (match "foo" "bar")
nil ; No match.

> (match "*foo *foo"
         "bar bar")
{*foo "bar"} ; Consistently bound.

> (match "*foo *bar"
         "ho ho")
{*foo "ho", *bar "ho"}

> (match "do *something with *something-else" 
         "do all the good you can with all you've got")
{*something "all the good you can", 
 *something-else "all you've got"}

> ;;; +Var with (hypothetically) registered kinds:
> (match '(+fruits to +nuts)
         "apples to filberts")
{+fruits "apples", +nuts "filberts"}

> (match '(+fruits to +nuts)
         "apples to apples")
nil ; Apples are not nuts.

```

### Annotated var examples
```clojure
> ;;; +Var with inline kind:
> (match '([+fruits {:kind #{"apples" "pumpkins" "pears"}}]
            to 
	    [+nuts {:kind #{"Brazils" "almonds" "filberts"}}])
         "apples to filberts")
{+fruits "apples", +nuts "filberts"}
```

#### Restriction examples
```clojure
> (match '(*foo [*bar {:finally? (not (= *bar *foo))}])
         "ho ho")
nil ; They are equal.

> (match '(*foo [*bar {:finally? (= *bar *foo)}])
         "ho ho")
{*foo "ho" *bar "ho"}
```

#### Short-hand attribute function call examples
```clojure
;;; Functions:

(defn digit-string? [s]
  (re-matches #"\d*" s))

;;; Restrict 'this' to a single, all-digit token.
(defn digits-alone [attrs this]
  (assoc (assoc attrs :max-tokens 1)
         :finally? (conjoin-restrictions `(digit-string? ~this)
                                         (get attrs :finally?))))

;;; Restrict 'this' to a single token of exactly n digits.
(defn n-digits-alone [attrs this n]
  (assoc (assoc attrs :max-tokens 1)
         :finally? (conjoin-restrictions
                     `(~'and ; Standardize 'and'.
                       (digit-string? ~this)
                       (= (count ~this) ~n))
                     (get attrs :finally?))))

;;; To qualify a series of (comma-free, natural) integers.  
(defn digits-along [attrs this]
  (assoc attrs 
         :each? (conjoin-restrictions `(digit-string? ~this)
                                      (get attrs :each?))))


> ;;; Expansions:

> (parse-template '([*int (n-digits-alone {:finally? (> (read-string *int) 99)} 
                                          *int
                                          3)]))
([*int {:finally? (and (token-matcher.core/digit-string? *int)
                       (clojure.core/= (clojure.core/count *int) 3)
                       (> (read-string *int) 99)),
        :max-tokens 1}])

> (parse-template '([*ints (digits-along {} *ints)]))
([*ints {:each? (token-matcher.core/digit-string? *ints)}])

;;; Vars with short-hand calls:

> (match '([*int (n-digits-alone {:finally? (> (read-string *int) 99)} 
                                 *int
                                 3)])
         "1001")
nil ; Too many digits.
> (match '([*int (n-digits-alone {:finally? (> (read-string *int) 99)} 
                                 *int
                                 3)])
         "001")
nil ; Too small a number.
> (match '([*int (n-digits-alone {:finally? (> (read-string *int) 99)} 
                                 *int
                                 3)])
         "101")
{*int "101"}
> (match '([*ints (digits-along {} *ints)]) "29 no")
nil ; "no" not digits.
> (match '([*ints (digits-along {} *ints)]) "29 010")
{*ints "29 010"}
```

### Template control examples:
```clojure
> (binding [*case-sensitive* true]
      (match "Foo" "foo"))
nil ; Cases don't match.

> (binding [*case-sensitive* true]
      (match "(:-case Foo)" "foo"))
{}

> (match '((:optional *bar stool)
            [*quack (different-when-bound {} *quack *bar)])
            "bar stool like a duck")
{*bar "bar", *quack "like a duck"}

> (match '((:choice foo bar)) "bar")
{}
```

### Defining form examples
```clojure
> (defn-templating-strings list-outer-strings ["*front stuff *back"
                                               [phrase]]
    (list *front *back))
`#'`<namespace>`/list-outer-strings`
> (list-outer-strings "make stuff up")
("make" "up")

> (defn-templating-symbols list-outer-symbols ["*front stuff *back"
                                               [phrase]]
    (list *front *back))
`#'`<namespace>`/list-outer-symbols`
> (list-outer-symbols "make stuff up")
(make up)
```
## Template grammar

In production rules below, ...
- Angle brackets surround a grammar \<element\>.
- \<element\>+ denotes one or more of \<element\>.
- \<element\>* denotes zero or more of \<element\>.
- ":-" separates rules' left- and right-hand sides.
- "|" separates right-hand sides alternatives.

We present first standard, internal template form, then extend this to
input template form accommodating short-hand function calls that
should expand (during parsing) into internal form.

### Internal template form

\<template\> :- \<string-template\> | \<list-template\>

\<string-template\> :- `"`\<term\>*`"`

\<list-template\> :- `(`\<term\>*`)`

\<term\> :- \<token\> | \<var\> | `(`\<control\> \<term\>*`)`

\<token\> :- \<normal-token\> | \<isolated-token\>

\<normal-token\>: A non-empty string free of whitespace, free of
leading and trailing chars in `*chars-to-strip*`, and free of chars in
`*chars-to-isolate*`.  For convenience, alternatively, when feasible,
a symbol whose name is a token.

\<isolated-token\>: A string of a single char included in
`*chars-to-isolate*` (necessarily including `\(`, `\)`, `\[`, `\]`,
`\{`, `\}`).

\<var\> :- \<plain-var\> | \<annotated-var\>

\<plain-var\>: A symbol starting with `*` (*var) or `+` (+var).

\<annotated-var\> :- `[`\<plain-var\> \<attrs\>`]`

\<attrs\> :- `{`\<\<attr-key\> \<attr-val\>\>*`}`

\<\<attr-key\> \<attr-val\>\> :- `:kind` \<kind-spec\> | `:always?`
\<restriction\> | `:each?` \<restriction\> | `:finally?`
\<restriction\> | `:finally.` \<action\>

\<kind-spec\> :- \<symbol\> | `#{`\<instance\>+`}`

\<instance\>: A string of one or more tokens separated by spaces.

\<restriction\>: Clojure expression evaluating truthy/falsey.

\<action\>: Clojure expression to be evaluated for side effects.

\<control\> :- \<mode-control\> | \<sequence-control\>

\<mode-control\> :- `:+case` | `:-case`

\<sequence-control\> :- `:optional` | `:serial` | `:choice`

### Input template form:

Short-hand function calls must expand to expected internal form.
"+:-" below indicates an additional alternative for \<attrs\>, beyond
those specified above using ":-".

\<attrs\> +:- `(`\<short-hand-fn-sym\> \<fn-arg\>*`)`

\<short-hand-fn-sym\>: Symbol naming a short-hand attribute function.

\<fn-arg\> :- \<attrs\> | \<plain-var\> | \<whatever\>

\<whatever\>: Some Clojure datum.

## Token handling

Templates can be either strings or lists.  We use
`clojure.edn/read-string` to parse string templates into list
templates.

We expect templates to be clean.  Tokens should be free of chars in
`*chars-to-strip*`, chars in `*chars-to-isolate*` should already be
isolated, and all expressions should conform to the template language.

To include in a template a token that would cause `read-string`
to error out, stringify it---as in `"#this"` (in a list) or
`\"'this\"` (in a string).  Do the same for a token that the
matcher would recognize as a variable but that you would like
to match explicitly in input---as in `"*that"`.  When `\'` is not
in `*chars-to-strip*` and not in `*chars-to-isolate*`, do the same
where `\'` begins a token---as in `"'twas"`.  Otherwise,
tokens (besides `nil`---see next) that `read-string` will
recognize as symbols may be expressed as such in a list template.

To include the token `"nil"` in a string template, explicitly quote
it: `\"nil\"`.  In a list template, use `"nil"`.

List templates may not include multi-token strings, as in `("#this
that 7.5")`.  Stringify (double-quote) exceptional tokens
individually, as in `("#this" that "7.5")`.

To customize what (e.g., punctuation) characters get stripped
automatically from adjacent tokens, edit or rebind dynamic variable
`*chars-to-strip*`.  Strippable characters that are not adjacent to
alphanumeric tokens are discarded.  A single trailing quote mark (`\'`)
is handled specially, per dynamic variable
`*allow-trailing-apostrophe*`.

An application can also rebind dynamic variables...

- `*token-substitutions*` (hashamp), to standardize selected
  tokens---e.g., to substitute `"a"` for `"an"`

- `*chars-to-isolate*` (set), to isolate (e.g.) `\(` to `"("`

- `*case-sensitive*` (Boolean), re template token to input token
  matching.

We parse the second, string argument to `match` to create a list
of normal tokens (stripping and isolating chars, per spec).

## Template variables

Vars (template variables) either have matching mode input/output
(*vars) or mode input-only (+vars).  A *var may match any series of
tokens.  A +var's permitted matching token series are limited to a
known set of instances.

Only the template contains vars (not the input string).

A var, unless in the scope an `:optional` or `:choice` control, must
match at least one input string token.  It may match a greater number
of tokens.

A var occurring more than once in a template must match consistently.
E.g., template `(*foo *foo)` will match input `"bar bar"` but not
`"bar bell"`.  We take a same-root-named pair of *var and +var (e.g.,
`*part` and `+part`) to be two different vars not requiring such
consistency.  We recommend against later occurrences transcending
scopes of earlier occurrences' containing`:optional` or `:choice`
controls.  Instead, use a restriction calling `same-when-bound`.

## +Vars

We will know all of a +var's possible values before processing a
such a variable in a template.  We provide a
few interfaces to inform the matcher of the values acceptable
for a given +var.  Your application can...

- Use dynamic variable `*kind-instances*` and associated access
  functions (e.g., `(add-kind-instance` \<kind\> \<instance\>`))` to
  register kinds and instances.  A plain var `+foo` then will match
  any registered instance of kind `foo`.

- Add a static kind, (say `weekday-trigram`, for values in `#{Sun
  Mon Tue ...}`) by directly manipulating `*kind-instances*` with
  Clojure primitives; then include `+weekday-trigram`
  in your template where such a trigram were expected.

- Provide a `:kind` key/value pair in an annotated +var's attribute
  map (see next item).

## Annotated vars

A template var can either be "plain"---e.g., a single Clojure symbol
like `*foo` or `+bar`---or "annotated"---a symbol-and-attribute map
Clojure vector like `[*foo {:kind "football"}]`.  Supported
attributes include...

- Instance kind specification, as suggested above

  - We can handle different vars of the same kind in a given
    template this way---as in `"[+subpart {:kind part}] is a part
    of [+superpart {:kind part}]"`.

  - Inline an instance set, for an ephemeral kind (not recorded
    on `*kind-instances*`)---as in `[+foo {:kind #{"any" "one
    of" "these"}}]`.  This can be useful when the requirement for
    a given set is specific to a given template, and there is no
    apparent benefit to giving the set a name on
    `*kind-instances*`.  We support this form only for a +var.

  - When attribute `:kind` is included in a *var's annotation, we add
    its binding to the instance registry for the specified kind.

    - To accommodate backtracking search, the matcher will extend the
      instance registry only locally, until the binding finally is
      included in a recognized match.

    - A template's earlier *var instances discovered thus can be
      available to its later +vars specifying the same kind.

    - An inline set is not an appropriate value for a *var's `:kind`
      attribute.

- Token cardinality restrictions, as in `[*book_2 {:kind book
  :min-tokens` \<min\> `:max-tokens` \<max\>`}]`, where both \<min\>
  and \<max\> are integers \>= 1

- Instance restrictions---Clojure forms that must evaluate
  truthy (i.e., other than `false` or `nil`) to qualify a
  partial or full candidate binding.  Three varieties:
 
  - `{:always?` \<condition\>`}`, to check \<condition\> each time a
    var's candidate binding is extended with a next input token.  So,
    `[*foo {:max-tokens 2}]` could alternatively be specified as `[*foo
    {:always? (<= (count-tokens *foo) 2)}]`.
  
  - `{:each?` \<condition\>`}`, to check \<condition\> applied just to
    the next token that would extend a var's candidate binding.  See
    our definition of `digits-along`, for some motivation.
  
  - `{:finally?` \<condition\>`}`, to check \<condition\> only when a
    var's candidate binding is complete.  So, [*foo {:min-tokens
    2}] could be specified as `[*foo {:finally? (>= (count-tokens
    *foo) 2)}]`.
  
  Such forms may refer to earlier template vars (in doing so,
  will reference their bindings) or to a thread's Clojure vars.
  Example, when `*part_1` occurs earlier in a template:
  `{:finally? (not+ *part_1 *part_2)}`.  In this setting, we
  bind any vars as yet unbound by the matcher to `nil`, so using
  code can branch on that.  See, e.g., our definition for
  `different-when-bound`.  Call functions that are closures to
  access local content in their captured scope.  See our test
  `template-local-test`.

  ```clojure
  (defn different-when-bound [attrs this other]
    (assoc attrs
           :finally? (conjoin-restrictions `(if (and ~other ~this)
                                              (not= ~this ~other)
                                              true)
                                           (get attrs :finally?))))
  ```
  
  We accommodate compound conditions via `and`, `or`, `not`, ...

- Instance actions---Clojure forms to be evaluated for their side
  effects, once a binding value has been qualified finally.

  In this setting, also, we bind any elided optional-scope vars to
  `nil`.  (`different-when-bound` remains a good reference, form-wise.
  Replace the truthy condition with some side effect.  Call
  `adjoin-actions` rather than `conjoin-restrictions`.)

  While we've arranged for *vars with attribute `:kind` to write their
  instances to a registry here, we look forward to connecting more
  general knowledge representation via Datalog or Prolog.  Then an
  action might connect multiple var's bindings in a logical assertion.
  As with *var-based dynamic kind instance registration, we plan for
  any within-template, dynamic logical assertions to be provisional,
  at least until a match is complete.

  Given that even "qualified finally" bindings may be backtracked out
  of without having been included in a complete match, we recommend
  deferring material actions to a template's final var, if not to a
  calling application (considering that a given template and input
  string may not match uniquely).

  Example: `{:finally. (println (format-cl "Matched *part to
  \'~a\'..." *part))}`.

  Wrap multiple actions in `do`.

When a var occurs more than once in a template, a later occurrence's
annotation matters only if all earlier occurrences have been elided,
per their scope within `:optional` or `:choice` controls.  (We
recommend against multiple occurrences transcending scopes of earlier
occurrences' containing `:optional` or `:choice` controls.  Instead,
use our short-hand function `same-when-bound`.)  Otherwise, later
occurrences must share an earliest occurrence's binding.

## Attribute short-hand functions

Instead of an explicit attribute map, an annotated var may have a call
to an attribute "short-hand" function.  The spec `[*int (digits-alone
{} *int)]`, e.g., expands at parse time to the attribute map `[*int
{:max-tokens 1, :finally? (digit-string? *int)}]`.  This map's effect
is to restrict `*int`'s binding to a single token, all of whose
characters are (per the regex uesd in `digit-string?`) digits.

```clojure
(defn digit-string? [s]
  (re-matches #"\d*" s))

(defn digits-alone [attrs this]
  (assoc (assoc attrs :max-tokens 1)
         :finally? (conjoin-restrictions `(digit-string? ~this)
                                         (get attrs :finally?))))
```

To keep templates uncluttered, we walk them to quote short-hand
function calls' arguments automatically.  To test a function
like `digits-alone` outside the matcher, quote the arguments
manually as necessary, as in `(digits-alone {} '*int)`.

As you can see in our definitions for `digits-alone` and related
functions and tests, an attribute short-hand function...

- Should include at least an argument (we use `this`) to
  accommodate the plain version of the current var (e.g., `*int`).

- May include arguments to accommodate any vars in using
  templates.

- May include (as all ours do) an argument for an
  existing (perhaps empty) attribute map, thus supporting
  composition of short-hand function calls.

- May include other arguments as needed.  See our definition for
  `n-digits-alone`.

```clojure
(defn n-digits-alone [attrs this n]
  (assoc (assoc attrs :max-tokens 1)
         :finally? (conjoin-restrictions
                     `(~'and
                       (digit-string? ~this)
                       (= (count ~this) ~n))
                     (get attrs :finally?))))
```

## Template controls

Template controls can surround any terms and can be nested.

The mode control `:+case` arranges for its contained terms to be
processed with dynamic Clojure var `*case-sensitive*` bound to `true`,
`:-case` to `false`.

The sequence control `:optional` allows its content to be elided.

`:choice` allows any one (and only one) of its top-level content
items.  Matching considers these items in the order listed.

`:series` requires all of its top-level items, in order.  Ths can be
useful within the scope of a `:choice` control.  A top-level templates
implicitly is contained by a `:series` control.

## Template-matching interfaces

We provide several interfaces for interacting with the matcher.

- The function `match` returns either a single match (a hashmap
  associating template variables with token strings) or `nil` (for no
  match).  `match` returns the first match discovered when binding as
  as many tokens as possible to each variable when processing
  templates from left to right.

- Most general is the function `matches` that returns
  either `nil` or, in a two-element vector...

  - A set of either all matches or up to a specified number of
    matches

  - A version of `*kind-instances*` as augmented (per annotated *vars
    with attribute `:type`) locally during matching.

  Our intuition is that applications will require either...

  - A single match if it exists (`match` should suffice.)

  - All matches.  (Use `matches` with no limit.)

  - A single match and whether it is unique.  Use `matches` with a
    value of `2` for argument `limit`. Considering that if a returned
    match were not unique, there could be some undesirable ambiguity
    in a using application's templates and/or the presented input
    strings, `matches` affords the opportunity either to accept (say,
    `reset!` the global version) or discard the returned version of
    `*kind-instances*`.

  Multiple matches may be appropriate in some applications, not in
  others (e.g., knowledge acquisition via controlled natural
  language).  Multiple matches consistent with a given template/input
  pair are more likely when either...

  - The template has optional vars.

  - The template has consecutive vars---especially consecutive
    *vars.

  - The input includes consecutive like tokens that may match a var.

- `match-pre-parsed` and `matches-pre-parsed`---versions of `match`
  and `matches` that skip the work of parsing, in case a template may
  be applied repeatedly.

- Two macros for defining functions that match an input argument to a
  fixed template and expose the template's variables and matched
  values as locals in the defined function's body.  Here again, we
  bind any elided optional-scope vars to `nil`.  So far, these macros
  use only `match` (not `matches`).

  - `defn-templating-strings`

  - `defn-templating-symbols`

  See examples in the large comment in the source file's section
  "Template-matching defining forms," also the helper functions
  `tokens->symbol`, `symbol->tokens` that an application may benefit
  from.  This is the sole method of calling the matcher that we have
  used in our own (proprietary) application.

- REPL-serviceable macros `with-matching-template-strings`,
  `with-matching-template-symbols`.

## Future work ideas

We might pursue some of the following ideas towards increasing matcher
expressivity, efficiency, robustness, and scale, given motivating use
cases.

### Ideas for expressivity

Besides replacing our kind registry with logic-based type reasoning,
we might...

- Enable short-hand function calls at the sequence level, splicing in
  results to yield standard, internal form.  This is an easy lift,
  awaiting a real-world use case where it clearly furthers code
  duplication avoidance.

- Support explicit anonymous vars (say, `*_`, `+_`) for which
  no bindings are recorded (so, for which consistency across
  occurrences is not required).

- Handle anonymous vars implicit in the use of sequence controls.
  Consider the following hypothetical call---free of explicit
  vars---and its resulting hypothetical match.

  ```clojure
  > (match '(:series this
                     (:optional (:choice always never))
                     (:choice is was)
                     (:choice going (:series starting
                                             (:choice before after)
                                             (:choice daylight midnight)))
                     to be confusing)
           "this is starting before midnight to be confusing")
  {*:s1 "this is starting before midnight to be confusing"
   +:s1.o1 nil,
   *:s1.o1.c1 nil,
   +:s1.c2 "is",
   +:s1.c3 "starting before midnight",
   *:s1.c3.s1 "starting before midnight",
   +:s1.c3.s1.c1 "before",
   +:s1.c3.s1.c2 "midnight"}
  ```
  
  We suppose a using application might benefit by examining such
  bindings for sequence context vars.

  Delivering var-free instance bindings (as above) for series
  containing choices may involve initially (internally) including and
  then systematically de-referencing such vars.  E.g., an initial
  binding for `*:s1.c3.s1` might be `"starting +:s1.c3.s1.c1
  +:s1.c3.s1.c2"` (perhaps more efficiently left as the list
  `(starting +:s1.c3.s1.c1 +:s1.c3.s1.c2)`).

- Return either longest or shortest var bindings first.  With supposed
  dynamic variable `*shortest-first*` (and controls `:shortest`,
  `:longest`), prefer eliding optional content.  Handle corresponding
  controls `:shortest`, `:longest` like `:+case`, `:-case`.  Again,
  this is an easy lift for which we await a use case it would clearly
  matter in.

### Ideas for efficiency

Some ideas here might be advantageous for some use cases or
application mixes, depending on indexing and compilation overhead.

- Represent instances for a large-cardinality kind, rather than in a
  flat set as here, in an efficiently traversable trie (i.e., a token
  prefix tree---ideally with a hashamap of allowed next tokens at each
  prefix node).  Then replace our winnowing down of `current-+set`
  with successive trie queries.

- Represent templates in a trie---towards identifying which templates
  can match a given input string.

- Index templates' tokens to obviate unnecessary matching work.

  - At minimum, ensure each of a template's unelidable tokens occurs
    in input (non-empty set difference).

  - Note (compile templates') indices of occurrences within a template
    control scope.  Ensure that (e.g.)...

    - Templates' and inputs' consecutive tokens are consistent,
      pairwise.

    - The remaining indices of a template or control branch thereof do
      not exceed corresponding tokens' indices in input.

- Perhaps in addition to indexing, ...

  - Compute and incrementally track a template's token capacity---the
    maximum number of input tokens it can match.  Because a single
    *var can match infinitely many tokens, this makes sense only when
    a template (or traversed branch thereof) necessarily ends in a
    token or a +var---or (if we'd like to consider processing
    templates and inputs in reverse) begins so.

  - Track an input's remaining token load, then fail early, should
    load exceed capacity.  Manage also the symmetric case, to catch
    (e.g.) a template with more necessary (unelidable) tokens and vars
    than its corresponding input has tokens.

- Employ Clojure's transient collections where appropriate internally,
  returning persistent results.

- Consider both laziness and parallelism in multiple match
  processing (using `matches`).

### Ideas for robustness

To make the matcher accessible to a broader range of users and for a
broader range of use cases, we might...

- Perform systematic error checking and exception handling.

- Employ `recur` or `trampoline` for tail recursion optimization, to
  obviate deep call stacks.

- Enhance tests by arranging for them to bind explicit values for the
  dynamic Clojure vars we've offered for user/application
  customization---rather than just assuming our default values as in
  most tests now.  Consider customizing `deftest`, perhaps differently
  for different groups of tests, towards economizing expression in
  individual tests.

### Ideas for scale

Consider a trie database of kinds/types---ideally one capable of
optionally case-sensitive queries.  Address logical subtype and
subpredicate reasoning using this database.

## License

Copyright © 2023 Robert Carl Schrag

This program and the accompanying materials are made available under
the terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following
Secondary Licenses when the conditions for such availability set forth
in the Eclipse Public License, v. 2.0 are satisfied: GNU General
Public License as published by the Free Software Foundation, either
version 2 of the License, or (at your option) any later version, with
the GNU Classpath Exception which is available at
https://www.gnu.org/software/classpath/license.html.

## Acknowledgements

Thanks to folks at Franz (Inc.) and at Elemental Cognition (Inc.) for
posing inspiring problems and to folks on Clojurians Slack channel
`#beginners` for generous advice.
