(ns mapbean.interceptor.maptype
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defmacro p-eval [form]
  (let [args (cons 'str (interpose " " (rest form)))
        f (name (first form))]
    `(let [result# ~form]
       (println (str "(" ~f " " ~args  "): " result#))
       result#)))

(declare map-arg)

(defn- set-bean
  [bean k v]
  (let [field (->> (iterate #(.getSuperclass %) (.getClass bean))
                        (take-while #(not= Object %))
                        (map (fn [cl]
                               (try (.getDeclaredField cl (name k))
                                    (catch Exception e nil))))
                        (filter #(not= nil %))
                        (first))]
    (if field
      (do
        (.setAccessible field true)
        (.set field bean (map-arg (.getType field) v)))))
  bean)

(defn map-arg
  [arg-type arg]
  (cond
   (or (nil? arg-type)
       (nil? arg)
       (.isInstance arg-type arg))
   arg
   (or (= Character arg-type) (= Character/TYPE arg-type)) 
   (.charAt (str arg) 0)   
   (or (= Boolean arg-type) (= Boolean/TYPE arg-type))
   (Boolean/valueOf (str arg))
   (or (= Byte arg-type) (= Byte/TYPE arg-type))
   (Byte/valueOf (str arg))
   (or (= Short arg-type) (= Short/TYPE arg-type))
   (Short/valueOf (str arg))
   (or (= Integer arg-type) (= Integer/TYPE arg-type))
   (Integer/valueOf (str arg))
   (or (= Long arg-type) (= Long/TYPE arg-type))
   (Long/valueOf (str arg))
   (or (= Float arg-type) (= Float/TYPE arg-type))
   (Float/valueOf (str arg))
   (or (= Double arg-type) (= Double/TYPE arg-type))
   (Double/valueOf (str arg))
   (.isArray arg-type)
   (let [c-type (.getComponentType arg-type)]
     (into-array (.getComponentType arg-type)
                 (map #(map-arg c-type %) arg)))
   (.isAssignableFrom java.util.Collection arg-type)
   (let [result (or (and (= java.util.List arg-type) (java.util.ArrayList.))
                    (and (= java.util.Set arg-type) (java.util.HashSet.))
                    (and (not (.isInterface arg-type)) (.newInstance arg-type)))]
     (doseq [item arg]
       (.add result item))
     result)
   (.isAssignableFrom java.util.Map arg-type)
   (let [result (java.util.HashMap.)]
     (doseq [[k v] arg]
       (.put result k v))
     result)
   :else
   (reduce
    #(set-bean %1 (first %2) (second %2)) (.newInstance arg-type) arg)))

(defn map-result
  [result]
  (if (nil? result) result
      (let [result-type (type result)]
        (cond
         (or (.isAssignableFrom clojure.lang.IPersistentMap result-type)
             (.isAssignableFrom clojure.lang.IPersistentList result-type)
             (.isAssignableFrom clojure.lang.IPersistentSet result-type)
             (.isPrimitive result-type)
             (= Character result-type)
             (= Boolean result-type)
             (= String result-type)
             (.isAssignableFrom Number result-type))
         result
         (.isAssignableFrom java.util.Set result-type)
         (seq result)
         (or (.isAssignableFrom java.util.List result-type)
             (.isArray result-type))
         (vec result)
         (.isAssignableFrom java.util.Map result-type)
         (reduce #(assoc %1 (.getKey %2) (.getValue %2)) {} result)
         :else
         (bean result)))))

(defn with-type
  [function method]
  (fn [& args]
    (let [method (get-in method [(count args) :method])
          args (or (and (nil? method) args)
                   (->> (map (fn [t v] [t v]) (.getParameterTypes method) args)
                        (map (fn [[t v]] (map-arg t v)))))]
      (->> (apply function args)
           (map-result)))))
