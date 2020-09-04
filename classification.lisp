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
                  :parse-float)))

(defpackage classification
  (:use :common-lisp
        :postmodern)
  (:import-from :alexandria
                :curry
                :flatten
                :hash-table-values)
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
                :apply-all-migrations))

(in-package classification)

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

(defun insert (x tree)
  (let ((root (car tree)))
    (cond ((null root)
           (list x nil))
          ((= x root)
           t) ; Element already exists in tree
          ((< x root)
           (list root (second tree) (insert x (third tree))))
          (:else
           (list root (insert x (second tree)) (third tree))))))

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
;;; Database code - the XML files are obviously dumped from an SQL DB, so I'll
;;; just rebuild that and store the data that way.
;;;

(defparameter *db-parameters* '("abvd" "isaac" "" "localhost"))

(defun emptyp (item)
  "Extend the concept of NIL to strings, arrays and hashes"
  (typecase item
    (string     (string= item ""))
    (array      (zerop (length item)))
    (hash-table (zerop (hash-table-count item)))))

(defclass language ()
  ((id
    :col-type integer :primary-key t :col-identity t :reader language-id)
   (language
    :col-type text :initarg :language :reader language-name)
   (author
    :col-type (or text db-null) :initarg :author)
   (sil-code
    :col-type (or text db-null) :initarg :sil-code :reader language-sil-code)
   (glotto-code
    :col-type (or text db-null) :initarg :glotto-code :reader language-glotto-code)
   (notes
    :col-type (or text db-null) :initarg :notes :accessor language-notes)
   (problems
    :col-type (or text db-null) :initarg :problems)
   (classification
    :col-type (or text db-null) :initarg :classification :reader language-classification)
   (typed-by
    :col-type (or text db-null) :initarg :typed-by)
   (checked-by
    :col-type (or text db-null) :initarg :checked-by)
   (source
    :col-type (or text db-null) :initarg :source :reader language-source)
   (latitude
    :col-type (or double-precision db-null) :initarg :latitude :reader language-latitude)
   (longitude
    :col-type (or double-precision db-null) :initarg :longitude :reader language-longitude)
   (lexemes ; Not a column -- cache for LANGUAGE-WORDS method
    :type hash-table :initform (make-hash-table :test 'equal)))
  (:metaclass dao-class)
  (:keys id)
  (:table-name languages)
  (:documentation "Table and access object for an Austronesian language in the ABVD"))

(defun make-language (lang auth sil glotto notes probs classif typed checked src lat lng)
  "Constructor for LANGUAGE class."
  (flet ((double/db-null (string)
<<<<<<< HEAD
           (handler-case
=======
           (handler-case ; For some reason lxml very occasionally turns lat/lng to an int...
>>>>>>> bloods
               (if-let float (parse-float string :type 'double-float :junk-allowed t)
                       float
                       :null)
             (error () :null))))
    (make-instance 'language
                   :language lang :author auth :sil-code sil
                   :glotto-code glotto :notes notes :problems probs
                   :classification classif :typed-by typed
                   :checked-by checked :source src
                   :latitude (double/db-null lat)
                   :longitude (double/db-null lng))))

(defclass gloss ()
  ((id
    :col-type integer :primary-key t :col-identity t :reader gloss-id)
   (name
    :col-type text :unique t :initarg :name :reader gloss-name))
  (:metaclass dao-class)
  (:keys id)
  (:table-name glosses)
  (:documentation "The English gloss of a word in the ABVD"))

(defun make-gloss (name)
  "Constructor for GLOSS class."
  (make-instance 'gloss :name name))

(defclass lexeme ()
  ((id
    :col-type integer :primary-key t :col-identity t :reader lexeme-id)
   (transcript
    :col-type text :initarg :transcript :accessor lexeme-transcript)
   (gloss-id
    :col-type integer :references ((glosses id)) :initarg :gloss-id :reader lexeme-gloss-id)
   (language-id
    :col-type integer :references ((languages id)):initarg :lang-id :reader lexeme-language-id)
   (annotation
    :col-type (or text db-null) :initarg :note :accessor lexeme-note)
   (loan
    :col-type boolean :initarg :loan :reader lexeme-loan?)
   (cognacy
    :col-type (or float db-null) :initarg :cognacy :accessor lexeme-cognacy)
   (pmp-cognacy
    :col-type (or float db-null) :initarg :pmp-cognacy :accessor lexeme-pmp-cognacy)
   ;; Cache slots so we don't have to do another query each time the
   ;; value of this slot is needed
   (language-name :type string :initform nil)
   (gloss-name :type string :initform nil))
  (:metaclass dao-class)
  (:keys id)
  (:table-name lexemes)
  (:documentation "An actual word in some Austronesian language."))

(defun make-lexeme (lang-id gloss-id trans note loan cog pmpcog)
  "Constructor for LEXEME class. Note that the floating point numbers in
the cognacy entries are in the European format, with , as the decimal char"
  (flet ((single/db-null (string)
           (if-let fl (parse-float string :decimal-character #\, :junk-allowed t)
                   fl
                   :null)))
    (make-instance 'lexeme
                   :lang-id lang-id :gloss-id gloss-id
                   :transcript trans :note note
                   :loan (not (emptyp loan))
                   :cognacy (single/db-null cog)
                   :pmp-cognacy (single/db-null pmpcog))))

(defmigration (:version 1
               :comment "Created the base DAO tables for languages, glosses, and lexemes.")
  (dao-table-definition 'language)
  (dao-table-definition 'gloss)
  (dao-table-definition 'lexeme))

;;; These methods let us treat the database access classes like normal objects.
;;; The DAO classes are set up so data is normalised, but we want to be able
;;; to easily get e.g. the English gloss text for a lexeme without having to
;;; run another query.

(defgeneric lexeme-language (lexeme &rest args)
  (:documentation "Get the name of the language associated with this lexeme"))

(defmethod lexeme-language ((lexeme lexeme) &key (init nil))
  (when (null (slot-value lexeme 'language-name))
    (setf (slot-value lexeme 'language-name)
          (with-connection *db-parameters*
            (language-name (get-dao 'language
                                    (lexeme-language-id lexeme))))))
  (unless init ; Suppress output 
    (slot-value lexeme 'language-name)))

(defmethod initialize-instance :after ((lexeme lexeme) &key)
  (handler-case
      (lexeme-language lexeme :init t)
    ;; The instance has been created but not inserted, so doesn't
    ;; make sense to query-populate the other slots:
    (unbound-slot () nil))) 
           

(defgeneric lexeme-gloss (lexeme &rest args)
  (:documentation "Get the English gloss associated with this lexeme"))

(defmethod lexeme-gloss ((lexeme lexeme) &key (init nil))
  (when (null (slot-value lexeme 'gloss-name))
    (setf (slot-value lexeme 'gloss-name)
          (with-connection *db-parameters*
            (gloss-name (get-dao 'gloss (lexeme-gloss-id lexeme))))))
  (unless init
    (slot-value lexeme 'gloss-name)))

(defmethod initialize-instance :after ((lexeme lexeme) &key)
  (handler-case
      (lexeme-language lexeme :init t)
    ;; The instance has been created but not inserted, so doesn't
    ;; make sense to query-populate the other slots:
    (unbound-slot () nil))) 


(defgeneric language-lexemes (language &rest args)
  (:documentation "Get a list of all lexemes associated with this language"))

(defmethod language-lexemes ((language language) &key (init nil))
  "Caches the returned lexemes in a hash table in the LANGUAGE to improve performance"
  (let ((lex-table (slot-value language 'lexemes)))
    (when (emptyp lex-table)
      (with-connection *db-parameters*
        (do-select-dao (('lexeme lex) (:= 'language-id (language-id language)))
          (setf (gethash (lexeme-gloss lex) lex-table) lex))))
    (unless init
      (hash-table-values (slot-value language 'lexemes)))))

;;;
<<<<<<< HEAD
;;; Routine to convert XML from ABVD into DAO objects and populate database
;;;

(defun wipe-db ()
  (with-connection *db-parameters*
    (query (:delete-from 'languages))
    (query (:delete-from 'lexemes))
    (query (:delete-from 'glosses))))
=======
;;; Wrapper classes around the DAOs, as the pomo DAO class does not mix well
;;; with normal class stuff
;;;

;;;
;;; Routine to convert XML from ABVD into DAO objects and populate database
;;;
>>>>>>> bloods

(defun xml-to-database (directory &key (n 2000))
  "Load the XML files from DIRECTORY, extract information, and insert"
  (let ((lang-fields   '("language" "author" "silcode" "glottocode" "notes" "problems"
                         "classification" "typedby" "checkedby" "source" "latitude"
                         "longitude"))
        (lexeme-fields '("item" "annotation" "loan" "cognacy" "pmpcognacy"))
        (gloss-fields  '("word")))

    (flet ((get-fields (xml fields)
             (mapcar (lambda (f) (aref (lquery:$ xml f (text)) 0))
                     fields)))
      (handler-case
          (loop
             for path in (uiop:directory-files directory)
             for i below n
             do (handler-case
                    (let* ((xml  (lquery:load-page path))
                           (lang (apply #'make-language (append (get-fields xml lang-fields)
                                                                '(:init t))))
                           ;; First & second records contain  language info and coords, cut them off
                           (word-recs (slice (lquery:$ xml "record") 2 :end)))
                      (with-connection *db-parameters*
                        (with-transaction () ; Don't insert anything for this file on error
                          (insert-dao lang)  ; Insert the language data first so we can use its ID
                          (loop
                             for rec across word-recs
                             do
                               (let* ((gl-name (car (get-fields rec gloss-fields)))
                                      ;; Check whether we already have this gloss
                                      (gloss   (if-let gl (first (select-dao 'gloss (:= 'name gl-name)))
                                                       gl
                                                       (insert-dao (make-gloss gl-name))))
                                      (lex     (insert-dao (apply #'make-lexeme
                                                                  (append (list (language-id lang)
                                                                                (gloss-id gloss))
                                                                          (get-fields rec lexeme-fields)
                                                                          '(:init t))))))
                                 (when verbose
                                   (format t "Inserted ~a (~a) for language ~a~&"
                                           (lexeme-transcript lex)
                                           (gloss-name gloss)
                                           (language-name lang)))))))
                      (format t "Finished processing language ~a~%" (language-name lang)))
                  (error (condition)
                    (format t "Couldn't process ~a: Error ~a was encountered~&" path condition))))))))

<<<<<<< HEAD
=======
      (loop
         for path in (uiop:directory-files directory)
         for i below n
         do (handler-case
                (let* ((xml  (lquery:load-page path))
                       (lang (apply #'make-language (get-fields xml lang-fields)))
                       ;; First & second records contain  language info and coords, cut them off
                       (word-recs (slice (lquery:$ xml "record") 2 :end)))
                  
                  (with-connection *db-parameters*
                    (with-transaction () ; Don't insert anything for this file on error
                      (insert-dao lang)  ; Insert the language data first so we can use its ID
                      (loop
                         for rec across word-recs
                         do (let* ((gl-name (car (get-fields rec gloss-fields)))
                                   (gloss   (if (select-dao 'gloss (:= 'name gl-name))
                                                (first (select-dao 'gloss (:= 'name gl-name)))
                                                (insert-dao (make-gloss gl-name)))))
                              (insert-dao (apply #'make-lexeme
                                                 (append (list (language-id lang)
                                                               (gloss-id gloss))
                                                         (get-fields rec lexeme-fields))))))))
                  (format t "Finished processing ~a (~a)~&" (language-name lang) path))
              (error (c)
                (format t "Couldn't process ~a: Error ~s~&" path c))))))) 
>>>>>>> bloods
