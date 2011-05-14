(ns indextank-clj.core
  (:require [clj-http.client :as http])
  (:use [clojure.contrib.json :only [read-json json-str]]))

(def #^{:private true} *private-url* nil)


(defmacro with-client [client & body]
  `(binding [*private-url* ~client]
     (do ~@body)))

(defn- json-response
  "Handle an empty response body"
  [resp]
  (if (> (count (:body resp)) 0)
      (read-json (:body resp))
      true))

(defn- request-map
  "Make an HTTP request map with an optional request body"
  [req-method req-url & [req-body]]
  (let [base-req {:method req-method :url req-url}]
    (if req-body
      (assoc base-req :body req-body)
      base-req)))

(defmacro wrap-request
  "Make a request to req-url, if succeed returns the response body or true,
if fail returns nil"
  [req-method private-url req-url & [req-body]]
  `(try (let [resp# (http/request (request-map ~req-method
					      (str ~private-url ~req-url)
					      ~req-body))
	     status# (:status resp#)]
	 (when (or (= 200 status#) (= 201 status#))
	   (json-response resp#)))
	(catch Exception e#
	  (print e#))))

(defmacro defrequest
  "Define a request, the last of args must be the private-url"
  [name docstring args & body]
  `(defn ~name
     ~docstring
      ([~@args]
	 ~@body)
      ([~@(butlast args)]
	 (~name ~@(butlast args) *private-url*))))


(defrequest indexes
  "Retrieves the metadata of every index in this account"
  [private-url]
  (wrap-request :get private-url "/v1/indexes"))



(defrequest index-meta
  "Retrieves metadata for the index name"
  [name private-url]
  (wrap-request :get private-url (str "/v1/indexes/" name)))

(defrequest create-index
  "Creates an index with the given name.
It cannot contain forward slashes /"
  [name private-url]
  (wrap-request :put private-url (str "/v1/indexes/" name)))

(defrequest delete-index
  "Removes the index name from the account"
  [name private-url]
  (wrap-request :delete private-url (str "/v1/indexes/" name)))

(defn- valid-doc?
  "Check if the doc has :docid and :fields"
  [doc]
  (and (contains? doc :docid) (contains? doc :fields)))

;; TODO: merge add-document and add-documents
(defrequest add-document
  "Adds a document to the index name"
  [name doc private-url]
  (if (valid-doc? doc)
    (wrap-request :put private-url
		  (str "/v1/indexes/" name "/docs") (json-str doc))
    (throw (Exception. "the doc map must have :docid and :fields"))))

(defrequest add-documents
  "Adds a batch of documents to the index name"
  [name docs private-url]
  (if (every? valid-doc? docs)
    (wrap-request :put private-url
		  (str "/v1/indexes/" name "/docs") (json-str docs))
    (throw (Exception. "the doc map must have :docid and :fields"))))

;; We use a querystring because we can't send a body in DELETE
(defrequest delete-document
  "Removes a document from the index name"
  [name docid private-url]
  (wrap-request :delete private-url
		(str "/v1/indexes/" name "/docs?docid=" docid)))

(defrequest score
  "Update the scoring variables of a document in index name"
  [name docid score-map private-url]
  (let [req-body {:docid docid :variables score-map}]
    (wrap-request :put private-url
		  (str "/v1/indexes/" name "/docs/variables")
		  (json-str req-body))))

(defrequest categorize
  "Update the categories of a document in index name"
  [name docid cat-map private-url]
  (let [req-body {:docid docid :categories cat-map}]
    (wrap-request :put private-url
		  (str "/v1/indexes/" name "/docs/categories")
		  (json-str req-body))))

(defrequest functions
  "Retrieves all the functions defined for the index name"
  [name private-url]
  (wrap-request :get private-url
		(str "/v1/indexes/" name "/functions")))

(defrequest define-function
  "Defines the function number num for the index name"
  [name fnum fdef private-url]
  (wrap-request :put private-url
		(str "/v1/indexes/" name "/functions/" fnum)
		(json-str {:definition fdef})))

(defrequest delete-function
  "Removes the function num from the index name"
  [name fnum private-url]
  (wrap-request :delete private-url
		(str "/v1/indexes/" name "/functions/" fnum)))


;; WARNING: Bad code here :(
(defn- build-query
  [query start len fnum]
  (let [q (str "q=" query)
	s (when start (str "&start=" start))
	l (when len   (str "&len=" len))
	f (when fnum (str "&function=" fnum))]
    (str q s l f)))

(defrequest search
  "Performs a search on the index name"
  [name query private-url {start :start len :len fnum :function :as opts}]
  (let [q-str (build-query query start len fnum)]
    (wrap-request :get private-url
		  (str "/v1/indexes/" name "/search?" q-str))))

(defrequest promote
  "Promotes a document for a query on the index name"
  [name docid query private-url]
  (wrap-request :put private-url
		(str "/v1/indexes/" name "/promote")
		(json-str {:docid docid :query query})))