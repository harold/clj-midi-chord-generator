(defproject clj-midi-chord-generator "0.1.0-SNAPSHOT"
  :description "A Clojure program for enumerating chords in midi files."
  :url "https://github.com/harold/clj-midi-chord-generator"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot clj-midi-chord-generator.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
