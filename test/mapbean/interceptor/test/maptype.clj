(ns mapbean.interceptor.test.maptype
  (:use [mapbean.interceptor.maptype]
        [clojure.test])
  (:import [java.util Date]))

(deftest test-set-bean
  (let [f @#'mapbean.interceptor.maptype/set-bean
        j (Date.)]
    (f j :fastTime 100000000)
    (is (= 100000000 (.getTime j)))))

(defn- array-t [t]
  (type (make-array t 0)))

(deftest test-map-arg
  (let [array1 (map-arg (array-t Long) [1 2 3 4 5])
        array2 (map-arg (array-t (array-t Long/TYPE)) [[1 2] [3 4]])
        list1 (map-arg java.util.ArrayList '(1 2 3))
        list2 (map-arg java.util.List {1,2 3,4})
        set1 (map-arg java.util.Set [1 2 3])
        map1 (map-arg java.util.HashMap {1 2 3 4})
        date1 (map-arg java.util.Date {:fastTime 10000})]
    (is (= Byte (type (map-arg Byte 3))))
    (is (= (array-t Long) (type array1)))
    (is (= [1 2 3 4 5] (vec array1)))
    (is (= (array-t (array-t Long/TYPE)) (type array2)))
    (is (= [[1 2] [3 4]] (map vec (vec array2))))
    (is (= java.util.ArrayList (type list1) (type list2)))
    (is (= '(1 2 3) (seq list1)))
    (is (= java.util.HashSet (type set1)))
    (is (= 3 (count set1)))
    (is (reduce #(and %1 (.contains set1 %2)) true [1 2 3]))
    (is (= java.util.HashMap (type map1)))
    (is (= 2 (.size map1)))
    (is (= [2 4] (map #(.get map1 %) [1 3])))
    (is (= java.util.Date (type date1)))
    (is (= 10000 (.getTime date1)))))

(defrecord A [a b])
(deftest test-map-result
  (let [not-changes [1 "s" true nil
                     [1 2 3] '(1 2) {1 2 3 4}
                     #{1 2} (A. 1 2)]]
    (is (= not-changes (map map-result not-changes))))
  (let [list1 (map-arg java.util.ArrayList [1 2 3])
        list2 (map-arg (array-t Byte) [1 2])
        set1 (map-result (map-arg java.util.HashSet [1 2 3]))
        map1 (map-result (map-arg java.util.HashMap {1 2 3 4}))]
    (is (= [1 2 3] (map-result list1)))
    (is (= [1 2] (map-result list2)))
    (is (seq? set1))
    (is (and (= 3 (count set1))
             (some #(= 1 %) set1)
             (some #(= 2 %) set1)
             (some #(= 3 %) set1)))
    (is (map? map1))
    (is (= 2 (count map1)))
    (is [2 4] (map #(map1 %) [1 3]))))
