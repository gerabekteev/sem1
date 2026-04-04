(ns deque)
;; --------------- Создание ---------------

(defn create-deque
  "Создаёт пустую двустороннюю очередь."
  []
  {:front '() :back '()})

;; --------------- Добавление ---------------

(defn push-front
  "Добавляет элемент в начало очереди.
   Возвращает новую очередь. O(1)."
  [dq item]
  (assoc dq :front (cons item (:front dq))))

(defn push-back
  "Добавляет элемент в конец очереди.
   Возвращает новую очередь. O(1)."
  [dq item]
  (assoc dq :back (cons item (:back dq))))

;; --------------- Удаление ---------------

(defn pop-front
  "Удаляет элемент с начала очереди.
   Если front не пуст — берём rest.
   Если front пуст — переворачиваем back и берём rest.
   Возвращает новую очередь."
  [dq]
  (let [front (:front dq)
        back  (:back dq)]
    (cond
      (seq front) (assoc dq :front (rest front))
      (seq back)  (let [rev (reverse back)]
                    {:front (rest rev) :back '()})
      :else       dq)))

(defn pop-back
  "Удаляет элемент с конца очереди.
   Если back не пуст — берём rest.
   Если back пуст — переворачиваем front и берём rest.
   Возвращает новую очередь."
  [dq]
  (let [front (:front dq)
        back  (:back dq)]
    (cond
      (seq back)  (assoc dq :back (rest back))
      (seq front) (let [rev (reverse front)]
                    {:front '() :back (rest rev)})
      :else       dq)))

;; --------------- Просмотр ---------------

(defn peek-front
  "Возвращает первый элемент очереди без удаления.
   nil если очередь пуста."
  [dq]
  (let [front (:front dq)
        back  (:back dq)]
    (cond
      (seq front) (first front)
      (seq back)  (last back)
      :else       nil)))

(defn peek-back
  "Возвращает последний элемент очереди без удаления.
   nil если очередь пуста."
  [dq]
  (let [front (:front dq)
        back  (:back dq)]
    (cond
      (seq back)  (first back)
      (seq front) (last front)
      :else       nil)))

;; --------------- Утилиты ---------------

(defn deque-empty?
  "Проверяет, пуста ли очередь."
  [dq]
  (and (empty? (:front dq))
       (empty? (:back dq))))

(defn deque-size
  "Возвращает количество элементов в очереди."
  [dq]
  (+ (count (:front dq))
     (count (:back dq))))

(defn to-list
  "Преобразует очередь в обычный список (от начала к концу)."
  [dq]
  (concat (:front dq) (reverse (:back dq))))

;; ============================================================
;; Демонстрация работы (Вложенная структура)
;; ============================================================

(defn -main []
  (println "=== Immutable Deque Demonstration ===\n")

  ;; 1. Создание
  (let [q1 (create-deque)]
    (println "1. Create empty q1:")
    (println "   q1 =" (to-list q1))
    (println "   Is empty?" (deque-empty? q1))

    ;; 2. Добавление в конец
    (let [q2 (-> q1 (push-back 1) (push-back 2) (push-back 3))]
      (println "\n2. Push 1, 2, 3 to back (create q2):")
      (println "   q2 =" (to-list q2))
      (println "   Size:" (deque-size q2))

      ;; 3. Добавление в начало
      (let [q3 (push-front q2 0)]
        (println "\n3. Push 0 to front (create q3):")
        (println "   q3 =" (to-list q3))

        ;; 4. Просмотр крайних элементов
        (println "\n4. Peek at q3 edges:")
        (println "   Front:" (peek-front q3))
        (println "   Back: " (peek-back q3))

        ;; 5. Первое удаление с начала
        (let [q4 (pop-front q3)]
          (println "\n5. First pop from front (create q4):")
          (println "   Removed:" (peek-front q3))
          (println "   q4 =" (to-list q4))

          ;; 6. Второе удаление с начала
          (let [q5 (pop-front q4)]
            (println "\n6. Second pop from front (create q5):")
            (println "   Removed:" (peek-front q4))
            (println "   q5 =" (to-list q5))

            ;; 7. Удаление с конца
            (let [q6 (pop-back q5)]
              (println "\n7. Pop from back (create q6):")
              (println "   Removed:" (peek-back q5))
              (println "   q6 =" (to-list q6))

              ;; 8. Проверка иммутабельности
              (println "\n8. IMMUTABILITY PROOF:")
              (println "   q2 is still:" (to-list q2))
              (println "   q3 is still:" (to-list q3))
              (println "   q4 is still:" (to-list q4)))))))))

;; Запуск
(-main)