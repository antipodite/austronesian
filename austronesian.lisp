;;;;
;;;; Simple experiment in language classification using edit distance to compute
;;;; language similarity, then use this similarity measure to construct a clado-
;;;; gram of language relationships.
;;;;
;;;; (c) Isaac Stead, August 2020
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria
                  :lparallel
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
                :if-let
                :curry
                :rcurry
                :flatten
                :hash-table-values
                :make-gensym-list
                :xor)
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

(setf lparallel:*kernel* (lparallel:make-kernel 8))

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
  ((name :type 'string :initarg :name :reader lang-name)
   (words :type 'cons :initarg :words)
   (family :type 'list :initarg :family :reader lang-family))
  (:documentation "Represents a language in the ABVD.
Constructors should retrieve the relevant info from the database.
All word objects for the language are stored in a hash table indexed
by the English gloss for that word for easy lookup."))

(defmethod print-object ((language language) stream)
  (print-unreadable-object (language stream :type t)
    (format stream "~s" (lang-name language))))

(defun make-language (name words family-str)
  ;; Add words to a hash indexed by gloss for easy lookup
  (let ((words-hash (make-hash-table :test 'equal)))
    (dolist (word words)
      (setf (gethash (word-gloss word) words-hash) word))
    (make-instance 'language
                   :name name
                   :words words-hash
                   :family (mapcar #'str:trim
                                   (str:split "," family-str)))))

(defgeneric lang-word (language word-spec)
  (:documentation "Get a WORD for the language by some identifier.")
  (:method ((language language) (gloss string))
    "Get a word from LANGUAGE internal hash table by its English gloss."
    (gethash gloss (slot-value language 'words))))

(defgeneric lang-words (language)
  (:documentation "Get a sequence of all words for the language.")
  (:method ((language language))
    (hash-table-values (slot-value language 'words))))

(defun lang-get-related (language level &key (lang-sequence nil))
    "If LANG-SEQ is not specified, query the database for related languages.
Otherwise, search for them in LANG-SEQ"
    (let* ((subgroup   (lang-family language))
           (parent     (subseq subgroup
                             0 (- (length subgroup) level)))
           (parent-str (str:join ", " parent)))
      (format t "~a ~a ~a~&" subgroup parent parent-str)
      (if lang-sequence
          (remove-if-not (lambda (lang) (search parent (lang-family lang) :test #'string=))
                         lang-sequence)
          (get-languages `(:~ 'classification ,parent-str)))))

(defclass word ()
  ((gloss :type 'string :initarg :gloss :accessor word-gloss)
   (string :type 'string :initarg :string :accessor word-string)))

(defun make-word (gloss string)
  (make-instance 'word :gloss gloss :string string))

(defgeneric word-distance (word-a word-b)
  (:documentation "Calculate the difference between two WORDs by some measure.")
  (:method ((word-a word) (word-b word))
    "Calculate Levenshtein distance between two words as strings."
    (distance (word-string word-a) (word-string word-b))))

(defun get-languages (&optional (filter-expr nil) (params *db-parameters*))
  "Return an array of language objects constructed from database rows.
FILTER-EXPR should be an S-SQL :where clause - if nil, returns all
languages."
  (let* ((filter-expr (if (null filter-expr) t filter-expr))
         (rows (with-connection params
                 (query (sql-compile (append '(:select
                                               'id 'language 'classification
                                               :from 'languages)
                                             `(:where ,filter-expr)))))))
    (flet ((retrieve-words (language-id)
             (query (:select 'glosses.name 'lexemes.transcript
                             :from 'lexemes
                             :inner-join 'glosses :on (:= 'glosses.id
                                                          'lexemes.gloss-id)
                             :inner-join 'languages :on (:= 'lexemes.language-id
                                                            'languages.id)
                             :where (:= 'languages.id
                                        language-id)))))
    (lparallel:pmap 'vector
                    (lambda (row)
                      (destructuring-bind (lang-id lang-name lang-family) row
                        (make-language lang-name
                                       (mapcar (lambda (word-row) (apply #'make-word word-row))
                                               (with-connection params
                                                 (retrieve-words lang-id)))
                                       lang-family)))
                    rows))))

(defun language-distance (a b)
  "Compute the average edit distance between all equivalent words of 
languages a and b. Rewritten using loop for more efficiency"
  (handler-case 
      (loop
         for word-a in (lang-words a)
         for word-b = (lang-word b (word-gloss word-a))
         with i = 0
         with total = 0
         do (when (and word-a word-b) ; Ignore words with no equivalent, cf. Greenhill 2012
              (incf i) (incf total (word-distance word-a word-b)))
         finally (return (float (/ total i))))
    (division-by-zero ()
      0)))

(defclass distance-matrix ()
  ((matrix :type simple-array :initarg :matrix :accessor dm-matrix)
   (index  :type hash-table   :initarg :index  :accessor dm-index))
  (:documentation "Store distance matrix itself and the associated
hash table of language indices in one place"))

(defun make-dist-matrix (items compare-fn)
  "Pretty fast now, does every language in the ABVD in about 110s"
  (let* ((n       (array-dimension items 0))
         (matrix  (make-array (list n n))))
    (dotimes (i n)
      (lparallel:pdotimes (j n)
        (setf (aref matrix i j)
              (let ((memo (aref matrix j i)))
                (if (> memo 0)
                    memo
                    (funcall compare-fn
                             (aref items i) (aref items j)))))))
    (make-instance 'distance-matrix :index items :matrix matrix)))

(defun vector->hash (vector &key (test #'eq))
  "Convert a vector to a hash table with array positions as keys
and contents as values."
  (let ((hash (make-hash-table :test test)))
    (loop
       for i below (array-dimension vector 0)
       for el = (aref vector i)
       do (setf (gethash i hash) el))
    hash))

(defun cluster (dm)
  "Cluster a DISTANCE-MATRIX using the unweighted pair group algorithm.
Languages with no words or no words that match words for other languages
should be removed from the input data before the DISTANCE-MATRIX is created,
or it will fail.
Rather than resizing the matrix to remove rows/columns for clusterswhen a 
new cluster is created, the matrix is placed in the matrix position for the 
left-hand cluster, and all the positions for the right-hand cluster are zeroed
so MATRIX-MIN will ignore them."
  (let ((n               (array-dimension (dm-matrix dm) 0))
        (cluster-indices (vector->hash (dm-index dm)))
        (matrix          (alexandria:copy-array (dm-matrix dm))))

    (flet ((update-matrix (x y)
             (dotimes (i n)
               (when  (/= i x y)
                 (let ((avg-distance (/ (+ (aref matrix x i)
                                           (aref matrix y i))
                                        2.0)))
                   (setf (aref matrix i x) avg-distance
                         (aref matrix x i) avg-distance))))
             (setq-row matrix y 0)
             (setq-column matrix y 0)))

      (dotimes (i (1- n))
        (multiple-value-bind (_ x y) (matrix-min matrix :threshold 0.01)
          (setf (gethash x cluster-indices)
                (list (gethash y cluster-indices) (gethash x cluster-indices)))
          (remhash y cluster-indices)
          (update-matrix x y)))

      (car (hash-table-values cluster-indices)))))

(defun setq-column (matrix colnum value)
  (dotimes (i (array-dimension matrix 0))
    (setf (aref matrix i colnum) value)))

(defun setq-row (matrix rownum value)
  (dotimes (i (array-dimension matrix 1))
    (setf (aref matrix rownum i) value)))

(defun matrix-min (matrix &key (threshold 0) (x-pad 0) (y-pad 0))
  "Return the smallest value in a 2D vector along with its indices"
  (loop
     for i from x-pad below (array-dimension matrix 0)
     with min and min-i and min-j do
       (loop
          for j from y-pad below (array-dimension matrix 1)
          for el = (aref matrix i j) do
            (when (and (or (null min) (> min el))
                       (<= threshold el))
              (setf min el) (setf min-i i) (setf min-j j)))
     finally (return (values min min-i min-j))))
