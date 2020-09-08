;;;;
;;;; Simple experiment in language classification using edit distance to compute
;;;; language similarity, then use this similarity measure to construct a clado-
;;;; gram of language relationships.
;;;;
;;;; (c) Isaac Stead, August 2020
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria
                  :dexador
                  :postmodern
                  :str
                  :lquery
                  :pomo-elver
                  :parse-float
                  :draw-cons-tree
                  :edit-distance)))

(defpackage #:austronesian
  (:use :common-lisp
        :postmodern)
  (:import-from :alexandria
                :curry
                :rcurry
                :flatten
                :hash-table-values
                :make-gensym-list)
  (:import-from :parse-float
                :parse-float)
  (:import-from :lquery
                :load-page
                :$)
  (:import-from :pomo-elver
                :defmigration
                :current-version
                :initial-migration
                :apply-migration
                :apply-all-migrations)
  (:import-from :draw-cons-tree
                :draw-tree)
  (:import-from :edit-distance
                :distance))

(in-package :austronesian)

;;;
;;; A couple of utilities
;;;

(defun slice (vector start end)
  (let ((end (if (eq end :end)
                 (length vector)
                 end)))
    (make-array (- end start)
                :element-type (array-element-type vector)
                :displaced-to vector
                :displaced-index-offset start)))

(defmacro if-let (var test-expr pass-expr fail-expr)
  "If, but bind the result of the test to a variable in success expr"
  (let ((result-val (gensym)))
    `(let ((,result-val ,test-expr))
       (if ,result-val
           ,(subst result-val var pass-expr)
           ,fail-expr))))

;;;
;;; Basic binary tree
;;;

(defparameter *test-tree*
  `(8 (10 (14 (13)))
      (3 (6 (7)
            (4))
         (1))))

(defun traverse (tree)
  (if (atom tree)
      tree
      (cons (traverse (car tree))
            (traverse (cdr tree)))))

(defun binsearch (x tree)
  (let ((root (car tree)))
    (cond ((null root)
           nil)
          ((= x root)
           t)
          ((< x root)
           (binsearch x (third tree)))
          (:else
           (binsearch x (second tree))))))

(defun insert (tree item)
  (let ((root (car tree)))
    (cond ((null root)
           (list item nil))
          ((= item root)
           t) ; Element already exists in tree
          ((< item root)
           (list root (second tree) (insert item (third tree))))
          (:else
           (list root (insert item (second tree)) (third tree))))))

;;;
;;; Scrape the Austronesian basic vocabulary database
;;;

(defun get-wordlists (directory &key (type "xml") (range 1639))
  "Download all data spreadsheets from https://abvd.shh.mpg.de/austronesian/
and save to DIRECTORY. Possible TYPEs are csv, tdf, xml"
  (let ((url-template "https://abvd.shh.mpg.de/utils/save/?type=~a&section=austronesian&language=~a"))
    (loop
       for i from 1 below (1+ range)
       do (let* ((url   (format nil url-template type i))
                 (data  (dex:get url))
                 (fname (pathname (str:concat directory
                                              (write-to-string i)
                                              "." type))))
            (str:to-file fname data)))))

;;;
;;; Language and word classes
;;;

(defparameter *db-parameters* '("abvd" "isaac" "" "localhost"))

(defclass language ()
  ((name :type 'string :initarg :name)
   (words :type 'cons :initarg :words))
  (:documentation "Represents a language in the ABVD.
Constructors should retrieve the relevant info from the database.
All word objects for the language are stored in a hash table indexed
by the English gloss for that word for easy lookup."))

(defun make-language (name words)
  (let ((words-hash (make-hash-table :test 'equal)))
    (dolist (word words)
      (setf (gethash (word-gloss word) words-hash) word))
    (make-instance 'language
                   :name name
                   :words words-hash)))

(defgeneric lang-word (language word-spec)
  (:documentation "Get a WORD for the language by some identifier."))

(defmethod lang-word ((language language) (gloss string))
  "Get a word from LANGUAGE internal hash table by its English gloss."
  (gethash gloss (slot-value language 'words)))

(defgeneric lang-words (language)
  (:documentation "Get a sequence of all words for the language."))

(defmethod lang-words ((language language))
  (hash-table-values (slot-value language 'words)))

(defclass word ()
  ((gloss :type 'string :initarg :gloss :accessor word-gloss)
   (string :type 'string :initarg :string :accessor word-string)))

(defun make-word (gloss string)
  (make-instance 'word :gloss gloss :string string))

(defgeneric word-distance (word-a word-b)
  (:documentation "Calculate the difference between two WORDs by some measure."))

(defmethod word-distance ((word-a word) (word-b word))
  "Calculate Levenshtein distance between two words as strings."
  (distance (word-string word-a) (word-string word-b)))

(defun get-languages (filter-expr)
  "Return language objects constructed from database rows.
FILTER-EXPR should be an S-SQL :where clause - if nil, returns all
languages."
  (let* ((filter-expr (if (null filter-expr) t filter-expr))
         (rows (query (sql-compile (append '(:select 'id 'language :from 'languages)
                                           `(:where ,filter-expr))))))
    (mapcar (lambda (row)
              (make-language (second row)
                             (mapcar (lambda (it) (apply #'make-word it))
                                     (query (:select 'glosses.name 'lexemes.transcript
                                             :from 'lexemes
                                             :inner-join 'glosses :on (:= 'glosses.id
                                                                          'lexemes.gloss-id)
                                             :inner-join 'languages :on (:= 'lexemes.language-id
                                                                            'languages.id)
                                             :where (:= 'languages.id
                                                        (first row)))))))
            rows)))



(defun language-difference* (lang-a lang-b)
  "Compute the average edit distance between all equivalent words of 
languages a and b. Horribly inefficient because it has to traverse the
list 3 times, O(n³) I think...?"
  (let ((distances (mapcar (lambda (w)
                             (word-distance w (lang-word lang-b (word-gloss w))))
                           (lang-words lang-a))))
    (float (/ (reduce #'+ distances)
              (length distances)))))

(defun language-distance (a b)
  "Compute the average edit distance between all equivalent words of 
languages a and b. Rewritten using loop for more efficiency"
  (loop
     for word-a in (lang-words a)
     for word-b = (lang-word b (word-gloss word-a))
     with i = 0
     with total = 0
     do (when (and word-a word-b) ; Ignore words with no equivalent, cf. Greenhill 2012
          (incf i) (incf total (word-distance word-a word-b)))
     finally (return (float (/ total i)))))

(defun list->hash (list &key (test #'equal))
  "Convert a list to a hash table where keys are list element indices 
and values are list elements."
  (let* ((table (make-hash-table :test test)))
    (loop
       for item in list
       for i from 0
       do (setf (gethash i table) item))
    (values table (hash-table-count table))))

(defun distance-matrix (languages)
  "Build a distance matrix of given languages"
  (multiple-value-bind (index count) (list->hash languages)
    (let ((dist-matrix  (make-array (list count count) :initial-element 0))
          (memo-table   (make-hash-table :test #'equal)))

      (flet ((ints->string (x y)
               (format nil "~a~a" x y)))
        
        (loop for i from 0 below count do
             (loop for j from 0 below count do
                ;; Check for previously computed value for this language combo
                  (let ((val (gethash (ints->string j i) memo-table)))
                    (if val
                        (setf (aref dist-matrix i j) val)
                        ;; Else
                        (let ((lang-dist (language-distance (gethash i index)
                                                            (gethash j index))))
                          (progn (setf (aref dist-matrix i j)
                                       lang-dist)
                                 (setf (gethash (ints->string i j) memo-table)
                                       lang-dist)))))))
        (values dist-matrix index)))))

(defun array-slice (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
      :displaced-index-offset (* row (array-dimension arr 1))))

(defun vector-min (vec &key (threshold 0))
  "Return the smallest element in the vector along with the index.
Ignores values lower than THRESHOLD"
  (loop
     for el across vec and i from 0
     with smallest = (aref vec 0) and index = 0
     do (when (and (> smallest el) (<= threshold el))
          (setf smallest el) (setf index i))
     finally (return (values smallest index))))

(defun matrix-min (matrix &key (threshold 0))
  "Return the smallest value in a 2D vector along with its indices"
  (loop
     for i below (array-dimension matrix 0)
     with min and min-i and min-j do
       (loop
          for j below (array-dimension matrix 1)
          for el = (aref matrix i j) do
            (when (and (or (null min) (> min el))
                       (<= threshold el))
              (setf min el) (setf min-i i) (setf min-j j)))
     finally (return (values min (list min-i min-j)))))
