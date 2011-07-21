(ns indextank-clj.core-test
  (:use clojure.test)
  (:use [clojure.contrib.json :only [read-json json-str]])
  (:require [indextank-clj.core :as core]
	    [clojure.pprint :as pp]
	    [ring.adapter.jetty :as ring]))


(def localurl "http://localhost:26590")

(def blog-index {:started true
		 :code "8y76h"
		 :creation_time "2011-04-22T15:46:00"
		 :size 4})
(def new-index {:started false
		:code "3a56o"
		:creation_time "2011-05-26T22:54:00"
		:size 0})

(def my-indexes (assoc {:forum {:started true
				:code "6k34l"
				:creation_time "2011-04-26T18:32:00"
				:size 80}} :blog blog-index))

(def multi-docs [{:added true} {:added true}])

(defn handler [req]
  (pp/pprint req)
  (println) (println)
  (condp = [(:request-method req) (:uri req)]
      [:get "/v1/indexes/blog"] {:stauts 200 :body (json-str blog-index)}
      [:get "/v1/indexes"] {:status 200 :body (json-str my-indexes)}
      [:put "/v1/indexes/new"] {:status 201 :body (json-str new-index)}
      [:put "/v1/indexes/blog"] {:status 204 :body ""}
      [:delete "/v1/indexes/blog"] {:status 200 :body ""}
      [:put "/v1/indexes/blog/docs"] {:status 200 :body ""}
      [:put "/v1/indexes/forum/docs"] {:stauts 200 :body (json-str multi-docs)}
      [:delete "/v1/indexes/blog/docs"] {:stauts 200 :body ""}))

(defn run-server
  []
  (defonce server
    (future (ring/run-jetty handler {:port 26590}))))


;; Indexes test
(deftest test-indexes
  (run-server)
  (let [resp (core/with-client localurl (core/indexes))]
    (is (map? resp))
    (is (every? keyword? (keys resp)))
    (is (every? keyword? (keys (:blog resp))))))

;; Index test
(deftest test-index-meta
  (run-server)
  (let [resp (core/with-client localurl (core/index-meta "blog"))]
    (is (map? resp))
    (is (every? keyword? (keys resp)))))

;; create-index test
(deftest test-create-index
  (run-server)
  (let [resp (core/with-client localurl (core/create-index "new"))]
    (is (map? resp))))

(deftest test-index-already-exists
  (run-server)
  (let [resp (core/with-client localurl (core/create-index "blog"))]
    (is (nil? resp))
    (is (not (map? resp)))))

;; delete-index test
(deftest test-delete-index
  (run-server)
  (let [resp (core/with-client localurl (core/delete-index "blog"))]
    (is (true? resp))))

;; Indexing tests

(def blog-post1 {:docid "p1" :fields {:title "Hello, World" :text "Hello!"}})
(def forum-posts [blog-post1 {:docid "p2" :fields {:title "Second post"
						  :text "I'm a veteran now"}}])

;; add-document test
(deftest test-add-document
  (run-server)
  (let [resp (core/with-client localurl (core/add-document "blog" blog-post1))
        resp-multi (core/with-client localurl
                     (core/add-document "forum" forum-posts))]
    (is (true? resp))
    (is (thrown? IllegalArgumentException (core/with-client localurl
			     (core/add-document "blog" {:docid "p2"}))))
    (is (thrown? IllegalArgumentException (core/with-client localurl
			     (core/add-document "blog" {:fields {:a "b"}}))))
    (is (vector? resp-multi))
    (is (every? :added resp-multi))
    (is (thrown? IllegalArgumentException (core/with-client localurl
                                            (core/add-document "blog"
                                                               [{:docid "p4"}
                                                                {:fields {}}]))))))

;; delete-document test
(deftest test-delete-document
  (run-server)
  (let [resp (core/with-client localurl (core/delete-document "blog" "p2"))]
    (is (true? resp))))

