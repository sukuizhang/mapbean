(ns mapbean.rmi
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [mapbean.bean :as mapbean]
            [mapbean.interceptor.maptype :as maptype])
  (:import [org.springframework.remoting.rmi RmiProxyFactoryBean]))

(defn ^:private get-service0
     [service-interface service-url]
     (try 
       (let [rmi (RmiProxyFactoryBean.)]
         (doto rmi
           (.setServiceInterface service-interface)
           (.setServiceUrl service-url)
           (.setLookupStubOnStartup true)
           (.setRefreshStubOnConnectFailure true)
           (.afterPropertiesSet))
         (.getObject rmi))
       (catch Exception e nil)))

(defn url
  [service-type rmi-url name]
  (str "rmi://" rmi-url "/" 
       (or name (let [s-name (.getSimpleName service-type)
                      s-name (if (.startsWith s-name "I") (.substring s-name 1) s-name)]
                  (str (Character/toLowerCase (.charAt s-name 0)) (.substring s-name 1))))))

(defn create-service-factory [urls]
  (memoize (fn [service-interface & [service-name]]
             (->> (if (fn? urls) (urls) urls)                  
                  (map #(get-service0 service-interface (url service-interface % service-name)))
                  (drop-while nil?)                  
                  (first)))))

(defn map-service
  [service-type urls & [ops]]
  (let [service-name (:service-name ops)
        service ((create-service-factory urls) service-type service-name)
        ns (or (:ns ops) (str *ns*))
        interceptors (or (:interceptors ops) [maptype/with-type])]
    (mapbean/map-bean service ns interceptors)))
