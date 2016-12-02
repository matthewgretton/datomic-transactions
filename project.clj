(defproject datomic-transactions "0.1.0-SNAPSHOT"
  :description "Library for logically comparing datomic transaction data"

  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"
                                   :creds :gpg}}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.datomic/datomic-pro "0.9.5394"]
                 [cursive/datomic-stubs "0.9.5153" :scope "provided"]
                 [spyscope "0.1.5"]])
