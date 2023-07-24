(defproject token-matcher "1.0.1"
  :description "Match a template containing variables against an input string, creating appropriate token-level bindings that a using application may process further."
  :url "https://github.com/bobschrag/token-matcher/"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [riddley "0.2.0"]]
  :repl-options {:init-ns token-matcher.core}
  )
