(ns clj-ipfs-api.core
  (:require [org.httpkit.client :as http]
            [cheshire.core :refer [parse-string]]
            [clojure.string :refer [join]])
  (:refer-clojure :exclude [get resolve update cat]))

(def ^:private default-api-url "http://127.0.0.1:5001")

(defn- assemble-query
  "Assemble a map ready for request."
  [api-url cmd-vec all-args]
  (let [{args true, [params] false} (group-by string? all-args)
        base-url                    (clojure.core/get (:request params)
                                                      :url
                                                      api-url)
        full-url                    (str base-url "/api/v0/" (join "/" cmd-vec))
        ipfs-params                 (dissoc params :request)]
    ; text for cat, json for everything else
    (assoc (merge {:as (if (= (last cmd-vec) "cat") :text :json)
                   :method :get}
                  (:request params)) 
           :url full-url
           :query-params (if args (assoc ipfs-params :arg args) ipfs-params))))

(defn- api-request
  "The same as used by clj-http."
  [raw-map]
  (let [json?       (= :json (:as raw-map)) ; Fiddle around to make it look the same as clj-http
        request-map (conj raw-map (when json? [:as :text]))
        {:keys [status headers body error]} @(http/request request-map)]
    (when-not error (if json? (parse-string body true) body))))

(defn- command-fn
  "Generate a function for an IPFS API command."
  [api-url cmd-vec]
  (fn [& args]
    (api-request (assemble-query api-url cmd-vec args))))

(defn- unpack-cmds
  "Traverse the nested structure to get vectors of commands."
  [acc cmds]
  (mapcat (fn [{:keys [:Name :Subcommands]}]
            (if (empty? Subcommands)
                (list (conj acc Name))
                (unpack-cmds (conj acc Name) Subcommands)))
          cmds))

(defn setup!
  "Request and intern all of the commands."
  ([api-url]
   (if-let [cmd-raw  ((command-fn api-url ["commands"]))]
    (let [cmd-vecs (unpack-cmds [] (:Subcommands cmd-raw))]
      (doseq [cmd-vec cmd-vecs]
        (intern *ns*
                (symbol (join "-" cmd-vec))
                (command-fn api-url cmd-vec))))
    (println "Could not set up using the"
             @api-url
             "address, please run `setup!` with another address.")))
  ([] (setup! default-api-url)))
