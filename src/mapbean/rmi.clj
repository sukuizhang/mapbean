(ns mapbean.rmi
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [mapbean.bean :as mapbean]
            [mapbean.interceptor.maptype :as maptype])
  (:import [org.springframework.remoting.rmi RmiProxyFactoryBean]))

(defn- get-service0
  "get rmi service according to service interface and service url, if any exception occur, nil is returned."
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
  "build a rmi service url, if input service name is nil, use default rule to create service name.
   default rule is like below:
       abc.xyz.IUserService --> userService
       abc.xyz.ActionBuilder --> actionBuilder
  service-interface: service interface
  rmi-url: like 192.168.0.137:3257
  service-name: service name"
  [service-interface rmi-url service-name]
  (str "rmi://" rmi-url "/" 
       (or service-name (let [s-name (.getSimpleName service-interface)
                      s-name (if (.startsWith s-name "I") (.substring s-name 1) s-name)]
                  (str (Character/toLowerCase (.charAt s-name 0)) (.substring s-name 1))))))

(defn create-service-factory
  "build a service factory using an url list, an url like 192.168.0.137:3257
  the service factory take a service interface and an option service name, and return a service, it try all the url in urls one by one to get this service, and return one when first time success."
  [urls]
  (memoize (fn [service-interface & [service-name]]
             (->> (if (fn? urls) (urls) urls)                  
                  (map #(get-service0 service-interface (url service-interface % service-name)))
                  (drop-while nil?)                  
                  (first)))))

(defn- delay-handler
  "create an java.lang.reflect.InvocationHandler used to delay get service action, use it, we create a proxy for this service-type, and do get when first invoke on this proxy.
 factory: a service factory underlying create service.
 service-type: service interface
 service-name: input service name, if it is nil, use default rule to create
 ops: options."
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
                   (try (.invoke method service args)
                        (catch java.lang.reflect.InvocationTargetException e
                          (throw (.getTargetException e))))
                   (if (not (:safe-invoke ops))
                     (throw (NullPointerException.
                             (str "can not get rmi service" service-str)))))))))))

(defn- wrap-delay
  "create a wrapped service factory,it create a proxy for special service type.
  when first invoke on this proxy, it create real service using the underlying service factory, and delegate the invoke to the real service."
  [factory & [ops]]
  (fn [service-type service-name]
    (let [handler (delay-handler factory service-type service-name ops)]
      (java.lang.reflect.Proxy/newProxyInstance (.getClassLoader service-type)
                                                (into-array [service-type])
                                                handler))))

(defn map-service
  "map java service methods to clojure functions and put it into special namespace."
  [service-type urls & [ops]]
  (let [service-name (:service-name ops)
        factory (-> (create-service-factory urls)
                    (wrap-delay ops))
        service (factory service-type service-name)
        ns (or (:ns ops) (str *ns*))
        interceptors (or (:interceptors ops) [maptype/with-type])]
    (mapbean/map-bean service ns interceptors)))

(defn map-services
  "map many java services methods to clojure functions and put it into special namespace."
  [service-types urls & [ops]]
  (doseq [service-type service-types]
    (map-service service-type urls ops)))
