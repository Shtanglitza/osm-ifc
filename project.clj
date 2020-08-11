(defproject osm-ifc "0.1.0"
  :description "Osm to ifc convert library"
  :url "s3p://muta93srb-maven/release"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-antlr "0.2.5"]
                 [org.clojure/core.match  "0.3.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/data.codec "0.1.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [clj-http "3.10.1"]
                 [org.clojure/data.json "1.0.0"]
                 [org.osgeo/proj4j "0.1.0"]]
  :plugins [[s3-wagon-private "1.3.4"]]
  :repositories [["muta93srb-maven" {:url "s3p://muta93srb-maven/clojure/repos"
                                     :username :env/AWS_ACCESS_KEY_ID
                                     :passphrase :env/AWS_SECRET_ACCESS_KEY}]])