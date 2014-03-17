(ns csvdb.core
  (:require [clojure-csv.core :as csv]))

(defn- parse-int [int-str]
  (Integer/parseInt int-str))

(def student-tbl (csv/parse-csv (slurp "student.csv")))
(def subject-tbl (csv/parse-csv (slurp "subject.csv")))
(def student-subject-tbl (csv/parse-csv (slurp "student_subject.csv")))

;; (table-keys student-tbl)
;; => [:id :surname :year :group_id]
;;
;; Hint: vec, map, keyword, first
(defn table-keys [tbl]
  (vec (map keyword (first tbl))))

;; (key-value-pairs [:id :surname :year :group_id] ["1" "Ivanov" "1996"])
;; => (:id "1" :surname "Ivanov" :year "1996")
;;
;; Hint: flatten, map, list
(defn key-value-pairs [tbl-keys tbl-record]
  (flatten (map list tbl-keys tbl-record)))

;; (data-record [:id :surname :year :group_id] ["1" "Ivanov" "1996"])
;; => {:surname "Ivanov", :year "1996", :id "1"}
;;
;; Hint: apply, hash-map, key-value-pairs
(defn data-record [tbl-keys tbl-record]
  (apply hash-map (key-value-pairs tbl-keys tbl-record)))

;; (data-table student-tbl)
;; (data-table student-subject-tbl)
;; (table-keys student-subject-tbl)
;; => ({:surname "Ivanov", :year "1996", :id "1"}
;;     {:surname "Petrov", :year "1996", :id "2"}
;;     {:surname "Sidorov", :year "1997", :id "3"})
;;
;; Hint: let, map, next, table-keys, data-record
(defn data-table [tbl]
  (let [tbl-keys (table-keys tbl)
        tbl-rows (rest tbl)]
  (map #(data-record tbl-keys %) tbl-rows)))

;; (str-field-to-int :id {:surname "Ivanov", :year "1996", :id "1"})
;; => {:surname "Ivanov", :year "1996", :id 1}
;;
;; Hint: assoc, Integer/parseInt, get
(defn str-field-to-int [field rec]
  (assoc rec field (parse-int (field rec))))

(def student (->> (data-table student-tbl)
                  (map #(str-field-to-int :id %))
                  (map #(str-field-to-int :year %))))

(def subject (->> (data-table subject-tbl)
                  (map #(str-field-to-int :id %))))

(def student-subject (->> (data-table student-subject-tbl)
                          (map #(str-field-to-int :subject_id %))
                          (map #(str-field-to-int :student_id %))))

;; (where* student (fn [rec] (> (:id rec) 1)))
;; => ({:surname "Petrov", :year 1997, :id 2} {:surname "Sidorov", :year 1996, :id 3})
;;
;; Hint: if-not, filter
(defn where* [data condition-func]
  (filter condition-func data))

;; (limit* student 1)
;; => ({:surname "Ivanov", :year 1998, :id 1})
;;
;; Hint: if-not, take
(defn limit* [data lim]
  (take lim data))

;; (order-by* student :year)
;; (order-by* nil :year)
;; => ({:surname "Sidorov", :year 1996, :id 3} {:surname "Petrov", :year 1997, :id 2} {:surname "Ivanov", :year 1998, :id 1})
;; Hint: if-not, sort-by
;; FIXME: Not sure for what reason to use if-not function here.
;; My current assumption is to check data on nil.
(defn order-by* [data column]
  (sort-by column data))
;;(defn order-by* [data column]
;;  (if-not (nil? column)(sort-by column data)))

;; (join* (join* student-subject :student_id student :id) :subject_id subject :id)
;; => [{:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1}
;;     {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2}
;;     {:subject "CS", :subject_id 2, :surname "Petrov", :year 1997, :student_id 2, :id 2}
;;     {:subject "CS", :subject_id 2, :surname "Sidorov", :year 1996, :student_id 3, :id 3}]
;;
;; Hint: reduce, conj, merge, first, filter, get
;; Here column1 belongs to data1, column2 belongs to data2.
(defn join* [data1 column1 data2 column2]
  ;; 1. Start collecting results from empty collection.
  ;; 2. Go through each element of data1.
  ;; 3. For each element of data1 (lets call it element1) find all elements of data2 (lets call each as element2) where column1 = column2.
  ;; 4. Use function 'merge' and merge element1 with each element2.
  ;; 5. Collect merged elements.
  (reduce
   (fn [acc elem]
     (conj
        acc
        (first
          (reduce
            #(conj %1 (merge %2 elem))
            []
            (filter #(= (column1 elem) (column2 %)) data2)))))
   []
   data1
))


;; This is part implementation of 3
;; 3. For each element of data1 (lets call it element1) find all elements of data2 (lets call each as element2) where column1 = column2.
(let [data1 student-subject
      column1 :student_id
      data2 student
      column2 :id
      element1 (first data1)]
  (reduce (fn [acc elem] (vector acc elem)) (filter #(= (column1 element1) (column2 %)) data2)))

;; 4. Use function 'merge' and merge element1 with each element2.
(let [data1 student-subject
      column1 :student_id
      data2 student
      column2 :id]
  (reduce (fn [acc elem] (conj acc (reduce #(conj %1 (merge %2 elem)) [] (filter #(= (column1 elem) (column2 %)) data2)))) [] data1)

  )



(function1 (first student-subject))



(let [data1 student-subject
      column1 :student_id
      data2 student
      column2 :id
      element1 (merge (first data1) {:data "data" :more_data "more_data"} )]
  (reduce #(vector %1 (merge %2 element1)) (filter #(= (column1 element1) (column2 %)) data2)))

(conj (first student-subject) [:data "data"])
;; 5. Collect merged elements.
(let [data1 student-subject
      column1 :student_id
      data2 student
      column2 :id
      element1 (first data1)]
  (reduce #(vector %1 (merge element1 %2)) (filter #(= (column1 element1) (column2 %)) data2)))



;; (let [data1 student-subject                        ;; ;;
;;       column1 :student_id                          ;; ;;
;;       data2 student                                ;; ;;
;;       column2 :id                                  ;; ;;
;;       element1 (first data1)]                      ;; ;;
;; (filter #(= (column1 element1) (column2 %)) data2) ;; ;;
;;   )                                                ;; ;;



(merge {:id 1} {:id 2 :data 3})
(let [element1 {:id 1}
     column1 :id
     column2 :id]
 (filter #(= (column1 %) (column2 element1)) student))
(filter #(= (:id %) 1) student)

;; (perform-joins student-subject [[:student_id student :id] [:subject_id subject :id]])
;; => [{:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1}
;;     {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2}
;;     {:subject "CS", :subject_id 2, :surname "Petrov", :year 1997, :student_id 2, :id 2}
;;     {:subject "CS", :subject_id 2, :surname "Sidorov", :year 1996, :student_id 3, :id 3}]
;;
;; Hint: loop-recur, let, first, next, join*
(defn perform-joins [data joins*]
  (loop [data1 data
         joins joins*]
    (if (empty? joins)
      data1
      (let [[col1 data2 col2] (first joins)]
        (recur (join* data1 col1 data2 col2)
               (next joins))))))

(defn select [data & {:keys [where limit order-by joins]}]
  (-> data
      (perform-joins joins)
      (where* where)
      (order-by* order-by)
      (limit* limit)))

(select student)
;; => [{:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"}]

(select student :order-by :year)
;; => ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"} {:id 1, :year 1998, :surname "Ivanov"})

(select student :where #(> (:id %) 1))
;; => ({:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})

(select student :limit 2)
;; => ({:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"})

(select student :where #(> (:id %) 1) :limit 1)
;; => ({:id 2, :year 1997, :surname "Petrov"})

(select student :where #(> (:id %) 1) :order-by :year :limit 2)
;; => ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"})

(select student-subject :joins [[:student_id student :id] [:subject_id subject :id]])
;; => [{:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1} {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2} {:subject "CS", :subject_id 2, :surname "Petrov", :year 1997, :student_id 2, :id 2} {:subject "CS", :subject_id 2, :surname "Sidorov", :year 1996, :student_id 3, :id 3}]

(select student-subject :limit 2 :joins [[:student_id student :id] [:subject_id subject :id]])
;; => ({:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1} {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2})
