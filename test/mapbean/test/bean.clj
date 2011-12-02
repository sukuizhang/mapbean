(ns mapbean.test.bean
  (:use [clojure.test]
        [mapbean.bean])
  (:import [java.util Date]))


(deftest test-find-method
  (let [m1 (find-method Class "forName" (into-array [String Boolean/TYPE ClassLoader]))
        m2 (find-method Class "forName" (into-array [String Boolean ClassLoader]))]
    (is (= "forName" (.getName m1) (.getName m2)))
    (is (= [String Boolean/TYPE ClassLoader]
             (seq (.getParameterTypes m1))             
             (seq (.getParameterTypes m2)))))  
  (is (= "getName" (.getName (find-method Class "getName" (into-array [])))))
  (is (find-method Class "isInstance" (into-array [Object])))
  (is (nil? (find-method Class "isInstance" (into-array []))))
  (is (nil? (find-method Class "xyz-abc" (into-array [Object])))))

(deftest test-invoke
  (let [d (Date. 1000000000)]
    (is (= 1000000000 (invoke d "getTime" [] [])))
    (invoke d "setTime" [Long] [100000000])
    (is (= 100000000 (invoke d "getTime" [] [])))))


(deftest test-create-fn
  (let [methods (@#'mapbean.bean/load-methods Date)
        j (Date.)
        mget (methods "getTime")
        mset (methods "setTime")
        fget (create-fn j "getTime" mget nil)
        fset (create-fn j "setTime" mset nil)
        interceptor (fn [f method]
                      (fn [& args]                        
                        (+ 1 (apply f args))))
        fget-with-interceptor
        (create-fn j "getTime" mget [interceptor interceptor interceptor interceptor])]
    (fset 100000000)
    (is (= 100000000 (fget)))
    (is (= 100000004 (fget-with-interceptor)))))

(deftest test-create-fns
  (let [j (Date.)
        fs (create-fns j nil)
        fset (fs "setTime")
        fget (fs "getTime")]
    (fset 100000000)
    (is (= 100000000 (fget)))))

(map-bean (Date.) "xyz.abc.123" nil)

(deftest test-map-bean
  (xyz.abc.123/set-time 10000)
  (is (= 10000 (xyz.abc.123/get-time))))


