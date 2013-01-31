(ns test.core)
(use 'test.map)
(use 'korma.db)
(use 'org.xerial/sqlite-jdbc)


(set! *warn-on-reflection* true)
(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
