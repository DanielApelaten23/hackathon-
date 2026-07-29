(defparameter *country-constant* "NGA")

(defun ascii-val (c)
  (char-code c))

(defun name-freq (str)
  (mod (reduce #'+ (map 'list #'char-code
                        (remove-if-not #'alpha-char-p str))) 100))

(defun pythagoras-mod (a b)
  (let ((sin (mod a b))
        (cos (if (= a 0) 1 (mod b a))))
    (let ((tan (if (= cos 0) 1 (mod a cos))))
      (list sin cos tan))))

(defun generate-matrix (sin cos tan name-freq)
  (list
    (list sin cos name-freq)
    (list tan (length *country-constant*) sin)
    (list name-freq cos (length *country-constant*))))

(defun split-words (str)
  (split-sequence:split-sequence #\Space str))

(defun predict (query)
  (let* ((words (split-words query))
         (remainder 0)
         (prediction ""))
    (format t "Input: ~a~%" query)

    ;; process every 2 words
    (loop for i from 0 below (- (length words) 1) by 2 do
      (let* ((w1 (elt words i))
             (w2 (elt words (+ i 1)))
             (a (ascii-val (char w1 0)))
             (b (ascii-val (char w2 0)))
             (trig (pythagoras-mod a b))
             (sin (first trig)) (cos (second trig)) (tan (third trig))
             (nf (name-freq (concatenate 'string w1 w2))))
        (format t "Chunk: ~a ~a -> sin=~a cos=~a tan=~a~%" w1 w2 sin cos tan)
        (setf remainder (mod (+ remainder sin cos tan) 97))))

    ;; begin % end
    (let ((begin (ascii-val (char query 0)))
          (end (ascii-val (char query (- (length query) 1)))))
      (setf remainder (mod (+ remainder (mod begin end)) 97))
      (format t "Begin%End: ~a~%" (mod begin end)))

    ;; cycle until 0
    (loop for cycle from 0 below 10 while (/= remainder 0) do
      (let ((div (+ cycle 2)))
        (setf remainder (mod remainder div))
        (setf prediction (concatenate 'string prediction
                          (string (code-char (+ 97 (mod remainder 26))))))))
    (format t "Prediction: ~a~%" prediction)
    prediction))

;; Example
(predict "I going to the market")