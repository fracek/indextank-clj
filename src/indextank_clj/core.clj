(ns indextank-clj.core
  (:require [clj-http.client :as http])
  (:use [clojure.contrib.json :only [read-json]]))

(def *private-url* nil)

(defmacro with-client [client & body]
  `(binding [*private-url* ~client]
     (do ~@body)))

(defmacro wrap-request [req-method req-url]
  `(try
     (let [resp# (http/request {:method ~req-method
			       :url (str *private-url* ~req-url)})
	   status# (:status resp#)]
       (when (or (= 200 status#) (= 201 status#))
	 (read-json (:body resp#))))
     (catch Exception e#
       (print e#))))

(defn indexes
  "Retrieves the metadata of every index in this account"
  []
  (wrap-request :get "/v1/indexes"))

(defn index-meta
  "Retrieves metadata for the index name"
  [name]
  (wrap-request :get (str "/v1/indexes/" name)))

(defn create-index
  "Creates an index with the given name.
It cannot contain forward slashes /"
  [name]
  (wrap-request :put (str "/v1/indexes/" name)))

;; IndexTank seems to not allow me to delete indexes, even from the web dashboard
;;
(defn delete-index
  "Removes the index name from the account"
  [name]
  (wrap-request :delete (str "/v1/indexes/" name)))
