(defparameter *country-constant* "NGA")

(defun ascii-val (c) (char-code c))

(defun name-frequency (s)
  (mod (reduce #'+ (map 'list #'char-code
                         (remove-if-not #'alpha-char-p s))) 100))

(defun pythagoras-mod (a b)
  (let ((sin (mod a b))
        (cos (if (= a 0) 1 (mod b a)))
        (tan (if (= cos 0) 1 (mod a cos))))
    (list sin cos tan)))

(defun split-words (str)
  (uiop:split-string str :separator "))

(defun predict (query)
  (let* ((words (split-words query))
         (remainder 0))
    (format t "Input: ~a~%" query)

    ;; Step 1: every 2 words
    (loop for i from 0 to (- (length words) 2) by 2 do
      (let* ((w1 (nth i words))
             (w2 (nth (1+ i) words))
             (a (ascii-val (char w1 0)))
             (b (ascii-val (char w2 0)))
             (trig (pythagoras-mod a b))
             (sin (first trig)) (cos (second trig)) (tan (third trig))
             (freq (name-frequency (concatenate 'string w1 w2))))
        (format t "Chunk: ~a ~a sin=~a cos=~a tan=~a freq=~a~%" w1 w2 sin cos tan freq)
        (setf remainder (mod (+ remainder sin cos tan) 97))))

    ;; Step 2: begin % end
    (let ((begin (ascii-val (char query 0)))
          (end (ascii-val (char query (1- (length query))))))
      (setf remainder (mod (+ remainder (mod begin end)) 97))
      (format t "Begin%End: ~a~%" (mod begin end)))

    ;; Step 3: cycle
    (loop for cycle from 0 below 10 while (/= remainder 0) do
      (let ((div (+ cycle 2)))
        (setf remainder (mod remainder div))
        (format t "Cycle ~a: rem=~a char=~c~%" (1+ cycle) remainder
                (code-char (+ 97 (mod remainder 26))))))))

(predict "I going to the market")