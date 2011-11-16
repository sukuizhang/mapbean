(ns mapbean.rmi
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [mapbean.bean :as mapbean]
            [mapbean.interceptor.maptype :as maptype])
  (:import [org.springframework.remoting.rmi RmiProxyFactoryBean]))

(defn- get-service0
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

(defn- delay-handler
  [factory service-type service-name ops]
  (let [real (atom nil)
        service-str (str "[service type:"
                         service-type                         
                         (and service-name (str " service-name:" service-name))   
                         "]")]
    (proxy [java.lang.reflect.InvocationHandler] []
      (invoke [proxy method args]
              (if (not @real)
                (swap! real (fn [a] {:service (factory service-type service-name)})))
              (let [service (:service @real)]
                (cond
                 (and (= "toString" (.getName method))
                         (= 0 (count (.getParameterTypes method))))
                 service-str
                 (and (= "hashCode" (.getName method))
                      (= 0 (count (.getParameterTypes method))))
                 (.hashCode service-type)
                 (and (= "equals" (.getName method))
                      (= 1 (count (.getParameterTypes method)))
                      (= Object (nth (.getParameterTypes method) 0)))
                 (= service (nth args 0))
                 (and (= "getClass" (.getName method))
                      (= 0 (count (.getParameterTypes method))))
                 service-type                 
                 :else
                 (if service
                   (.invoke method service args)
                   (if (not (:safe-invoke ops))
                     (throw (NullPointerException.
                             (str "can not get rmi service" service-str)))))))))))

(defn- wrap-delay
  [factory & [ops]]
  (fn [service-type service-name]
    (let [handler (delay-handler factory service-type service-name ops)]
      (java.lang.reflect.Proxy/newProxyInstance (.getClassLoader service-type)
                                                (into-array [service-type])
                                                handler))))

(defn map-service
  [service-type urls & [ops]]
  (let [service-name (:service-name ops)
        factory (-> (create-service-factory urls)
                    (wrap-delay ops))
        service (factory service-type service-name)
        ns (or (:ns ops) (str *ns*))
        interceptors (or (:interceptors ops) [maptype/with-type])]
    (mapbean/map-bean service ns interceptors)))

(defn map-services
  [service-types urls & [ops]]
  (doseq [service-type service-types]
    (map-service service-type urls ops)))
