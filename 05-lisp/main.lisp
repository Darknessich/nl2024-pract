(defpackage :multithread
  (:use :cl :sb-thread))

(in-package :multithread)

(defun parallel-process-array (n f arr)
  "Вычисляет значение, применяя ассоциативную функцию f к массиву arr в n потоках."
  (let* ((array-length (length arr))
         (chunk-size (ceiling array-length n))
         (chunks (loop for i from 0 below array-length by chunk-size
                       collect (subseq arr i (min array-length (+ i chunk-size)))))
         (results (make-array (length chunks)))
         (threads (make-array (length chunks))))
    ;; Создаём потоки для обработки каждого куска массива
    (loop for i from 0 below (length chunks)
          do (setf (aref threads i)
                   (sb-thread:make-thread
                    (let ((chunk-index i))
                      (lambda ()
                        (setf (aref results chunk-index) (reduce f (nth chunk-index chunks))))))))
    ;; Ждём завершения всех потоков
    (loop for i from 0 below (length threads)
          do (sb-thread:join-thread (aref threads i)))
    ;; Возвращаем результат объединения всех частей
    (reduce f results)))

;; Тест 1
(let ((n 2)
    (f #'+)
    (arr #(1 2 3 4 5 6))
    (expect 21))
(let ((result (parallel-process-array n f arr)))
(if (= result expect)
    (format t "Test #1: Success (got: ~A, expected: ~A)~%" result expect)
    (format t "Test #1: Failed  (got: ~A, expected: ~A)~%" result expect))))

;; Тест 2
(let ((n 3)
    (f #'*)
    (arr (loop for i from 1 to 20 collect i))
    (expect 2432902008176640000))
(let ((result (parallel-process-array n f arr)))
(if (= result expect)
    (format t "Test #2: Success (got: ~A, expected: ~A)~%" result expect)
    (format t "Test #2: Failed  (got: ~A, expected: ~A)~%" result expect))))

;; Тест 3
(let ((n 4)
    (f #'+)
    (arr (loop for i from 1 to 1000000 collect i))
    (expect 500000500000))
(let ((result (parallel-process-array n f arr)))
(if (= result expect)
    (format t "Test #3: Success (got: ~A, expected: ~A)~%" result expect)
    (format t "Test #3: Failed  (got: ~A, expected: ~A)~%" result expect))))