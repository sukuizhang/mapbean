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

(defn- sub-type-of
"check 'sub type of' relation of two types, ignore difference from primitive type and it's wrapped type"
  [type sub-type]
  (let [type (type-map type type)
        sub-type (type-map sub-type sub-type)]
    (.isAssignableFrom type sub-type)))

(defn- suit-for-method
  "check if the name and arg type list suit for special method"
  [m name arg-types]
  (and (= name (.getName m))
       (= (count (seq (.getParameterTypes m))) (count arg-types))
       (reduce
        #(and %1 (or (nil? (first %2))
                     (sub-type-of (first %2) (second %2))))
        true
        (map (fn [a b] [a b]) (seq (.getParameterTypes m)) arg-types))))

(def ^{:doc "find method from special java type according to method name and arg type list"} find-method
  (memoize (fn [service-type name arg-types]
             (->> (.getMethods service-type)
                 (seq)
                 (filter #(suit-for-method % name arg-types))
                 (first)))))

(defn invoke
  "invoke java method on a java obj
   target: invoke target, it can be a java.lang.Class if it's static invoke.
   m-name: method name
   arg-types: arg type list
   args: invoke args"
  [target m-name arg-types args]
  (let [target-type (if (= Class (.getClass target)) target (type target))
        invoke-target (if (= target target-type) nil target)
        arg-types (or
                   (and arg-types arg-types)
                   (map type args))
        m (find-method target-type m-name arg-types)]
    (try
      (.invoke m invoke-target (into-array Object args))
      (catch java.lang.reflect.InvocationTargetException e
        (throw (.getTargetException e))))))

(defn- add-single-method
  "add a method to methods struction,follow is an example of methods struction for some methods of java.lang.ClassLoader:
  {\"forName\" {1 {:method <Method Class<?> forName(String className)> :types [String]}
            3 {:method <Method Class<?> forName(String name, boolean initialize, ClassLoader loader)> :types [String boolean ClassLoader]}}
  \"getFields\" {0 {:method <Method Field[] getFields()> :types []}}}
if more then 1 methods use same method name, as follow:
UserInfo getUser(String name)
UserInfo getUser(long id)
it will be {\"getUser\" {1 {:multi true}}}
because parameters of clojure function has no type, in this instance, we cann't determine what type the parameters shoud be !
ms: functions struction
method: a java method"
  [ms method]
  (let [m-name (.getName method)
        arg-types (vec (.getParameterTypes method))]
    (update-in ms [m-name (count arg-types)]
               #(if % {:multi true} {:types arg-types :method method}))))

(defn- load-methods
  "load methods struction from a java type, add all public methods in it"
  [service-type]
  (reduce add-single-method nil (.getMethods service-type)))

(defn- make-s-array
  "make a symbol vector use as arg name"
  [len]
  (vec (map #(symbol (str "arg" %)) (range len))))

(defn create-fn
  "create a clojure function with java method name, method struction, interceptors and target to invoke on.
   it first build a create clojure list like follow:
  (fn
    ([target arg1] mapbean.bean/invoke target \"forName\" [String] [arg1])
    ([target arg1 arg2 arg3] mapbean.bean/invoke target [String boolean ClassLoader] [arg1 arg2 arg3]))
  and eval it to create a clojure function, and then partial it to inject target and intercept it.
  target: underlying invoke target
  m-name: java method name
  method: method struction in methods struction define preview
  interceptors: interceptors to interceptor this invoke."
  [target m-name method interceptors]
  (let [body (map (fn [[len m]]
                    (let [args (make-s-array len)
                          all-args (vec (cons 'target args))
                          types (get-in method [len :types])]
                      (list all-args
                            (list 'mapbean.bean/invoke
                                  'target
                                  m-name
                                  types args))))
                  method)
        fn-list (cons 'fn body)] 
    (-> (eval fn-list)
        (partial target)
        (#(reduce
           (fn [f interceptor] (interceptor f method))
           % interceptors)))))

(defn create-fns
  "create a map contains all java method name and clojure function pairs from a java obj."
  [obj interceptors]
  (->> (load-methods (.getClass obj))
       (map
        (fn [[f-name method]]
          [f-name (create-fn obj f-name method interceptors)]))
       (into {})))

(defn- bind-to-ns
  "bind var to namespace with special symbol name"
  [ns-name var-name var]
  (let [var-name (symbol var-name)
        ns (or (and ns-name (create-ns (symbol ns-name)))
               *ns*)]
    (intern ns var-name var)))

(defn- translate-function-name
  "translate java method name to more clojure function name, for example:
    getUserInfo -> get-user-info"
  [java-name]
  (reduce
   #(str %1
         (if (<= (int \A) (int %2) (int \Z))
           (str "-" (Character/toLowerCase %2)) 
           %2))
   ""
   java-name))

(defn map-bean
  "create clojure function map,and bind them to special namespace."
  [bean ns interceptors]
  (let [ns (str ns)]
    (doseq [[f-name f] (create-fns bean interceptors)]      
      (bind-to-ns ns (translate-function-name f-name) f))))
