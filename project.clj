(defproject reddit-daily-programmer "0.1.0-SNAPSHOT"
  :description "Solutions for some of the Reddit DailyProgrammer challenges"
  :url "https://www.reddit.com/r/dailyprogrammer/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-beta3"]]
  ;; :plugins [[lein-nodisassemble "0.1.3"]]
  :jvm-opts ^:replace ["-Xms1024m" "-Xmx1g" "-server"]
  :main rdp.214-intermediate
  )
