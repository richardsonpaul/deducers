(defproject deducers "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]]

  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.6.1"]
                             [org.clojure/core.typed "0.2.72"]]}})
