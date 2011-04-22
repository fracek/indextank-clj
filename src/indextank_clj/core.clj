(ns indextank-clj.core
  (:require [clj-http.client :as http])
  (:use [clojure.contrib.json :only [read-json json-str]]))

(def *private-url* nil)

(defmacro with-client [client & body]
  `(binding [*private-url* ~client]
     (do ~@body)))

(defn- json-response
  "Handle an empty response body"
  [resp]
  (when (> (count (:body resp)) 0)
    (read-json (:body resp))))

(defn- request-map
  "Make an HTTP request map with an optional request body"
  [req-method req-url & [req-body]]
  (let [base-req {:method req-method :url req-url}]
    (if req-body
      (assoc base-req :body req-body)
      base-req)))

(defmacro wrap-request [req-method req-url & [req-body]]
  `(let [resp# (http/request (request-map ~req-method
					  (str *private-url* ~req-url)
					  ~req-body))
	 status# (:status resp#)]
     (when (or (= 200 status#) (= 201 status#))
       (json-response resp#))))

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

(defn delete-index
  "Removes the index name from the account"
  [name]
  (wrap-request :delete (str "/v1/indexes/" name)))

(defn- valid-doc?
  "Check if the doc has :docid and :fields"
  [doc]
  (and (contains? doc :docid) (contains? doc :fields)))

;; TODO: merge add-document and add-documents
(defn add-document
  "Adds a document to the index name"
  [name doc]
  (if (valid-doc? doc)
    (wrap-request :put (str "/v1/indexes/" name "/docs") (json-str doc))
    (throw (Exception. "the doc map must have :docid and :fields"))))

(defn add-documents
  "Adds a batch of documents to the index name"
  [name docs]
  (if (every? valid-doc? docs)
    (wrap-request :put (str "/v1/indexes/" name "/docs") (json-str docs))
    (throw (Exception. "the doc map must have :docid and :fields"))))

;; We use a querystring because we can't send a body in DELETE
(defn delete-document
  "Removes a document from the index name"
  [name docid]
  (wrap-request :delete (str "/v1/indexes/" name "/docs?docid=" docid)))

(defn score
  "Update the scoring variables of a document in index name"
  [name docid score-map]
  (let [req-body {:docid docid :variables score-map}]
    (wrap-request :put (str "/v1/indexes/" name "/docs/variables") (json-str req-body))))
