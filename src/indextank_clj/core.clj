(ns indextank-clj.core
  (:require [clj-http.client :as http])
  (:use [clojure.contrib.json :only [read-json]]))

(def *private-url* nil)

(defmacro with-client [client & body]
  `(binding [*private-url* ~client]
     (do ~@body)))

(defn indexes
  "Retrieves the metadata of every index in this account"
  []
  (let [resp (http/get (str *private-url* "/v1/indexes"))]
    (if (= 200 (:status resp))
      (read-json (:body resp))
      nil)))

(defn index-meta
  "Retrieves metadata for the index name"
  [name]
  (let [resp (http/get (str *private-url* "/v1/indexes/" name))]
    (if (= 200 (:status resp))
      (read-json (:body resp))
      nil)))

(defn create-index
  "Creates an index with the given name.
It cannot contain forward slashes /"
  [name]
  (let [resp (http/put (str *private-url* "/v1/indexes/" name))
	status (:status resp)]
    (cond (= 200 status) (read-json (:body resp))
	  (= 204 status) {:error " An index already existed for that name"}
	  (= 409 status) {:error "Too many indexes for this account"})))

;; IndexTank seems to not allow me to delete indexes, even from the web dashboard
;; 
(defn delete-index
  "Removes the index name from the account"
  [name]
  (let [resp (http/delete (str *private-url* "/v1/indexes/" name))]
    (if (= 200 (:status resp))
      (read-json (:body resp))
      nil)))