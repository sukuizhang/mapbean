(ns mapbean.bean)

(def type-map
     {Boolean/TYPE Boolean
      Character/TYPE Character
      Byte/TYPE Byte
      Short/TYPE Short
      Integer/TYPE Integer
      Long/TYPE Long
      Float/TYPE Float
      Double/TYPE Double})

(defn- sub-type-of [type sub-type]
  (let [type (type-map type type)
        sub-type (type-map sub-type sub-type)]
    (.isAssignableFrom type sub-type)))

(defn- check-method [m name arg-types]
  (and (= name (.getName m))
       (= (count (seq (.getParameterTypes m))) (count arg-types))
       (reduce
        #(and %1 (or (nil? (first %2))
                     (sub-type-of (first %2) (second %2))))
        true
        (map (fn [a b] [a b]) (seq (.getParameterTypes m)) arg-types))))

(def find-method
  (memoize (fn [service-type name arg-types]
             (->> (.getMethods service-type)
                 (seq)
                 (filter #(check-method % name arg-types))
                 (first)))))

(defn invoke [service name arg-types args]
  (let [cl (if (= Class (.getClass service)) service (type service))
        invoke-instance (if (= service cl) nil service)
        arg-types (or
                   (and arg-types arg-types)
                   (map type args))
        m (find-method (.getClass service) name arg-types)]
    (.invoke m invoke-instance (into-array Object args))))

(defn- add-single-method
  [ms method]
  (let [m-name (.getName method)
        arg-types (vec (.getParameterTypes method))]
    (update-in ms [m-name (count arg-types)]
               #(if % {:multi true} {:types arg-types :method method}))))

(defn- load-methods [service-type]
  (reduce add-single-method nil (.getMethods service-type)))

(defn- bind-to-ns [ns-name var-name var]
  (let [var-name (symbol var-name)
        ns (or (and ns-name (create-ns (symbol ns-name)))
               *ns*)]
    (intern ns var-name var)))

(defn- make-s-array [len]
  (vec (map #(symbol (str "arg" %)) (range len))))

(defonce ^:private place-bean-ns "place-bean-ns-auto-1377")
(defonce ^:private place-bean-id-seek (atom 0))
(defn- generate-bean-name [bean]
  (-> bean
      (.getClass)
      (.getName)
      (.toLowerCase)
      (.replaceAll "[.]" "-")
      (#(str % "-auto-" (swap! place-bean-id-seek inc)))))

(defn create-fn [bean f-name method interceptors]
  (let [bean-name (generate-bean-name bean)]
    (bind-to-ns place-bean-ns bean-name bean)
    (let [body (map (fn [[len m]]
                      (let [args (make-s-array len)
                            types (get-in  method [len :types])]
                        (list args
                              (list 'mapbean.bean/invoke
                                    (symbol (str place-bean-ns "/" bean-name))
                                    f-name
                                    types args))))
                    method)
          fn-list (cons 'fn body)]
      (-> (eval fn-list)
          (#(reduce
             (fn [f interceptor] (interceptor f method))
             % interceptors))))))

(defn create-fns [bean interceptors]
  (->> (load-methods (.getClass bean))
       (map
        (fn [[f-name method]]
          [f-name (create-fn bean f-name method interceptors)]))
       (into {})))

(defn- translate-function-name [java-name]
  (reduce
   #(str %1
         (if (<= (int \A) (int %2) (int \Z))
           (str "-" (Character/toLowerCase %2)) 
           %2))
   ""
   java-name))

(defn map-bean
  [bean ns interceptors]
  (let [ns (str ns)]
    (doseq [[f-name f] (create-fns bean interceptors)]      
      (bind-to-ns ns (translate-function-name f-name) f))))
