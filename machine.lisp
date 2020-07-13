(deftype mix-bit  () 'bit)
(deftype mix-sign () 'mix-bit)

(deftype mix-byte (bits) `(simple-array mix-bit (,bits)))
(deftype mix-half-byte () '(mix-byte 3))
(deftype mix-full-byte () '(mix-byte 6))

(let ((alist '((2 . mix-half-word)
               (5 . mix-full-word))))
  (macrolet
      ((defword (bytes)
         (let* ((elem (assoc bytes alist :test '=))
                (name (cdr elem)))
           `(defstruct ,name
              (sign 0 :type mix-sign)
              (contents (make-array ,bytes :element-type 'mix-full-byte))))))
    (dolist (n '(2 5)) `(defword ,n))))

(defgeneric sign (word))

(let ((alist '(
               ;; bytes
               (mhb . mix-half-byte)
               (mfb . mix-full-byte)
               ;; words
               (mhw . mix-half-word)
               (mfw . mix-full-word)
               )))
  (dolist (elem alist)
    `(define-symbol-macro ,(car elem) ',(cdr elem))))

(defun sign-char (bit)
  "Calculate the corresponding char to a sign bit for a half or full word."
  (declare (type mix-sign bit))
  (code-char
   ;; The formula 43 + 2*n.
   (+ 43 (* 2 bit))))

(let ((control-string "[ ~c | ~{~b~} ]")
      (alist '((3 . (mix-half-word
                     mix-half-word-sign
                     mix-half-word-contents))
               (6 . (mix-full-word
                     mix-full-word-sign
                     mix-full-word-contents)))))
  (macrolet
      ((print-word (length)
         (let ((lst (assoc 3 alist))
               (word (gensym "WORD"))
               (sign (gensym "SIGN"))
               (plus/minus (gensym "PLUS/MINUS"))
               (contents (gensym "CONTENTS")))
           `(defmethod print-object ((,word ,(first lst)) stream)
              (let* ((,sign (,(second lst) ,word))
                     (,plus/minus (sign-char ,sign)))
                (let ((,contents (,(third lst) ,word)))
                  (format stream
                          ,control-string
                          ,plus/minus ,contents)))))))
    (dolist (len '(3 6)) (print-word len))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rJ-word-p (word)
    "Is WORD an rJ-word? Means just being positive fixed."
    (when (mix-half-word-p word)
      ;; The sign must bei +, so the value constant 0.
      (= 0 (mix-half-word-sign word)))))

;; type for the rJ register, always positive signed
(deftype rJ-word ()
  '(and mix-half-word (satisfies rJ-word-p)))

(macrolet ((index-register (reg)
             `(,reg (make-mix-half-word) :type mhw)))
  (defstruct mix-machine
  ;;; REGISTERS
    ;; accumulator register, five bytes plus sign
    (rA (make-mix-full-word) :type mfw)
    ;; Extension Register
    ;; five bytes plus sign
    (rX (make-mix-full-word) :type mfw)
    ;; Index Registers
    ;; two bytes plus sign
    (dolist (reg '(rI1 rI2 rI3 rI4 rI5 rI6))
      (index-register reg))
    ;; Jump Address Register
    ;; holds two bytes, sign always +
    (rJ  (make-mix-half-word) :type rJ-word)
    ;; Overflow Toggle
    (overflow-toggle 0 :type bit)
    ;; Comparison Indicator
    ;; we have '((< . -1) (= . 0) (> . 1))
    (comparison-indicator 0 :type (integer -1 1))
  ;;; Memory Cells
    ;; MIX has 4000 congruent memory cells of 6 MIX-Bytes.
    (memory-cells
     (make-array 4000
                 :initial-element (make-array 6 :element-type 'mfb)
                 :element-type '(simple-array mfb (6)))
     :type (simple-array (simple-array mfb (6)) (4000)))
  ;;; Is the rest RAM?
    ;; Magnetic Tape Units
    (magnetic-tape-units (make-array 8 :element-type 'mfb)
     :type (simple-array mfb (8)))
    ;; Disk and Drums
    (disk-and-drums (make-array 8 :element-type 'mfb)
     :type (simple-array mfb (8)))
    ;; Card Reader
    (card-reader 0 :type mfb)
    ;; Card Punch
    (card-punch 0 :type mfb)
    ;; Line Printer
    (line-printer 0 :type mfb)
    ;; Paper Tape
    (paper-tape 0 :type mfb)))

(defun comparison-indicator-char (toggle)
  "Gets you the corresponding char of the comparison indicator."
  (declare (type (integer -1 1) toggle))
  (code-char
   ;; The formula 61 + n.
   (+ 61 toggle)))
