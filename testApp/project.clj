(defproject nova "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [seesaw "1.4.5"]
                 [net.mikera/core.matrix "0.50.0"]
                 [net.mikera/vectorz-clj "0.43.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [co.paralleluniverse/quasar-core "0.7.4"]
                 [co.paralleluniverse/pulsar "0.7.4"]]
  :java-agents [[co.paralleluniverse/quasar-core "0.7.4"]])
