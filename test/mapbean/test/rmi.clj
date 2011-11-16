(ns mapbean.test.rmi
  (:use [mapbean.rmi]
        [clojure.test])
  (:import [java.lang.reflect InvocationHandler]))

(deftest test-wrap-delay
  (let [i (atom 0)
        f (fn [t n] (swap! i inc) nil)
        f1 (@#'mapbean.rmi/wrap-delay f)
        s1 (f1 InvocationHandler "service-name")
        f2 (@#'mapbean.rmi/wrap-delay f {:safe-invoke true})
        s2 (f2 InvocationHandler "service-name")]
    (is (.isAssignableFrom InvocationHandler (type s1)))
    (is (.isAssignableFrom InvocationHandler (type s2)))
    (is (= 0 @i))
    (.invoke s2 nil nil nil)
    (is (= 1 @i))
    (.invoke s2 nil nil nil)
    (is (= 1 @i))
    (try (do
           (.invoke s1 nil nil nil)
           (is nil))
         (catch NullPointerException e
           (is (= 2 @i))))))
