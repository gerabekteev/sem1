(ns deque)

;; --------------- Создание ---------------

(defn create-deque
  "Создаёт пустую двустороннюю очередь."
  []
  {:front '() :back '()})

;; --------------- Добавление ---------------

(defn push-front
  "Добавляет элемент в начало очереди.
   Возвращает новую очередь."
  [dq item]
  (assoc dq :front (cons item (get dq :front))))

(defn push-back
  "Добавляет элемент в конец очереди.
   Возвращает новую очередь."
  [dq item]
  (assoc dq :back (cons item (get dq :back))))

;; --------------- Удаление ---------------

(defn pop-front
  "Удаляет элемент с начала очереди.
   Если front не пуст — берём rest.
   Если front пуст — переворачиваем back и берём rest.
   Возвращает новую очередь."
  [dq]
  (let [front (get dq :front)
        back  (get dq :back)]
    (if (not (empty? front))
      (assoc dq :front (rest front))
      (if (not (empty? back))
        (let [rev (reverse back)]
          (assoc dq :front (rest rev) :back '()))
        dq))))

(defn pop-back
  "Удаляет элемент с конца очереди.
   Если back не пуст — берём rest.
   Если back пуст — переворачиваем front и берём rest.
   Возвращает новую очередь."
  [dq]
  (let [front (get dq :front)
        back  (get dq :back)]
    (if (not (empty? back))
      (assoc dq :back (rest back))
      (if (not (empty? front))
        (let [rev (reverse front)]
          (assoc dq :front '() :back (rest rev)))
        dq))))

;; --------------- Просмотр ---------------

(defn peek-front
  "Возвращает первый элемент очереди без удаления.
   nil если очередь пуста."
  [dq]
  (let [front (get dq :front)
        back  (get dq :back)]
    (if (not (empty? front))
      (first front)
      (if (not (empty? back))
        (last back)
        nil))))

(defn peek-back
  "Возвращает последний элемент очереди без удаления.
   nil если очередь пуста."
  [dq]
  (let [front (get dq :front)
        back  (get dq :back)]
    (if (not (empty? back))
      (first back)
      (if (not (empty? front))
        (last front)
        nil))))

;; --------------- Утилиты ---------------

(defn deque-empty?
  "Проверяет, пуста ли очередь."
  [dq]
  (if (and (empty? (get dq :front))
           (empty? (get dq :back)))
    true
    false))

(defn deque-size
  "Возвращает количество элементов в очереди."
  [dq]
  (+ (count (get dq :front))
     (count (get dq :back))))

(defn to-list
  "Преобразует очередь в обычный список (от начала к концу)."
  [dq]
  (concat (get dq :front) (reverse (get dq :back))))

;; ============================================================
;; Демонстрация работы
;; ============================================================

(defn -main []
  (println "=== Демонстрация иммутабельной двусторонней очереди (Deque) ===\n")

  ;; 1. Создание
  (let [q1 (create-deque)]
    (println "1. Создание пустой очереди q1:")
    (println "   q1 =" (to-list q1))
    (println "   Пустая?" (deque-empty? q1))

    ;; 2. Добавление в конец (пошагово)
    (let [q2-step1 (push-back q1 1)
          q2-step2 (push-back q2-step1 2)
          q2       (push-back q2-step2 3)]
      (println "\n2. Добавление 1, 2, 3 в конец очереди (создание q2):")
      (println "   q2 =" (to-list q2))
      (println "   Размер:" (deque-size q2))

      ;; 3. Добавление в начало
      (let [q3 (push-front q2 0)]
        (println "\n3. Добавление 0 в начало очереди (создание q3):")
        (println "   q3 =" (to-list q3))

        ;; 4. Просмотр крайних элементов
        (println "\n4. Просмотр крайних элементов q3:")
        (println "   Начало (Front):" (peek-front q3))
        (println "   Конец (Back):  " (peek-back q3))

        ;; 5. Первое удаление с начала
        (let [q4 (pop-front q3)]
          (println "\n5. Первое удаление с начала очереди (создание q4):")
          (println "   Удалён:" (peek-front q3))
          (println "   q4 =" (to-list q4))

          ;; 6. Второе удаление с начала
          (let [q5 (pop-front q4)]
            (println "\n6. Второе удаление с начала очереди (создание q5):")
            (println "   Удалён:" (peek-front q4))
            (println "   q5 =" (to-list q5))

            ;; 7. Удаление с конца
            (let [q6 (pop-back q5)]
              (println "\n7. Удаление с конца очереди (создание q6):")
              (println "   Удалён:" (peek-back q5))
              (println "   q6 =" (to-list q6))

              ;; 8. Проверка иммутабельности
              (println "\n8. ДОКАЗАТЕЛЬСТВО ИММУТАБЕЛЬНОСТИ:")
              (println "   Очередь q2 по-прежнему:" (to-list q2))
              (println "   Очередь q3 по-прежнему:" (to-list q3))
              (println "   Очередь q4 по-прежнему:" (to-list q4)))))))))

;; Запуск
(-main)