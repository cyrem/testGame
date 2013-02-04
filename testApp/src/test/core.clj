(ns test.core
  (:use [seesaw core graphics]
        test.map
        test.render
        test.civ
        korma.db
        ;[org.xerial/sqlite-jdbc]
        ))







(def gameWindow (frame :title "gameWindow"
                       :content  (canvas :paint #(.drawString %2 "I'm a canvas" 10 10))
                       :width 600
                       :height 400
                       :on-close :exit))
(native!)



(set! *warn-on-reflection* true)
(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
