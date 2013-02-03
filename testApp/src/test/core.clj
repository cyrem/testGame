(ns test.core
  (:use [seesaw.core]
        [test.map]
        [test.render]
        [test.civ]
        [korma.db]
        ;[org.xerial/sqlite-jdbc]
        ))






(def gameWindow (frame :title "gameWindow"
                       :content "test"
                       :width 600
                       :height 400
                       :on-close :exit))
(native!)

(-> gameWindow (pack!) (show!))

(set! *warn-on-reflection* true)
(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
