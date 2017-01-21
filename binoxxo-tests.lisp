(in-package :binoxxo)

;;; http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~&~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;; Parameters für die Tests

;;; Tests
(defparameter *valides-spiel*
  #2A((5 0 0 0 0 5)
      (0 0 0 0 0 1)
      (0 0 0 5 1 5)
      (0 0 0 1 5 5)
      (0 0 0 5 0 1)
      (5 0 0 0 0 1)))

(defparameter *valides-spiel2*
  #2A((5 0 0 0 0 0)
      (0 0 0 1 0 0)
      (0 0 1 0 5 5)
      (1 0 0 0 0 0)
      (0 0 1 0 5 0)
      (0 1 0 0 0 0)))

(defparameter *valides-spiel3*
  #2A((1 0 0 0 0 0)
      (0 0 0 5 0 0)
      (0 0 5 0 1 1)
      (5 0 0 0 0 0)
      (0 0 5 0 1 0)
      (0 0 5 0 0 0)))

(defparameter *tripel-spiel*
  #2A((5 0 0 0 0 5)
      (0 0 0 0 0 1)
      (0 0 0 5 1 5)
      (0 0 0 1 5 5)
      (0 0 5 5 5 1)
      (5 0 0 0 0 1)))

(defparameter *vierer-spiel*
  #2A((5 0 0 5 5 5)
      (0 0 0 0 0 1)
      (0 0 0 5 1 5)
      (0 0 0 1 5 5)
      (0 0 0 5 0 1)
      (5 0 0 0 0 1)))

(defparameter *gelöstes-spiel*
  #2A((5 1 5 1 1 5)
      (1 5 5 1 5 1)
      (5 1 1 5 1 5)
      (1 1 5 1 5 5)
      (1 5 1 5 5 1)
      (5 5 1 5 1 1)))

(defparameter *gelöstes-spiel2*
  #2A((5 1 5 5 1 1)
      (5 1 5 1 5 1)
      (1 5 1 1 5 5)
      (1 1 5 5 1 5)
      (5 5 1 1 5 1)
      (1 5 1 5 1 5)))

(defparameter *gelöstes-spiel3*
  #2A((1 5 1 1 5 5)
      (1 5 1 5 1 5)
      (5 1 5 5 1 1)
      (5 5 1 1 5 1)
      (1 1 5 5 1 5)
      (5 1 5 1 5 1)))

;;; Definiere Tests

(deftest valide ()
  (check
    (eql t   (valides-spiel *valides-spiel*))
    (eql t   (valides-spiel *valides-spiel2*))
    (eql t   (valides-spiel *valides-spiel3*))))

(deftest invalide ()
  (check
    (eql nil (valides-spiel *vierer-spiel*))
    (eql nil (valides-spiel *tripel-spiel*))))

(deftest gelöst ()
  (check
    (eql t (rätsel-gelöst *gelöstes-spiel*))
    (eql t (rätsel-gelöst *gelöstes-spiel2*))
    (eql t (rätsel-gelöst *gelöstes-spiel3*))
    (eql nil (rätsel-gelöst *valides-spiel*))))

(deftest einfache-tests ()
  (combine-results
    (valide)
    (invalide)
    (gelöst)))

(deftest suche ()
  (check
    (equalp *gelöstes-spiel* (suche-lösung (list *valides-spiel*) #'rätsel-gelöst #'mögliche-spiele))
    (equalp nil (suche-lösung (list *valides-spiel2*) #'rätsel-gelöst #'mögliche-spiele))
    (equalp *gelöstes-spiel3* (suche-lösung (list *valides-spiel3*) #'rätsel-gelöst #'mögliche-spiele))
    (equalp nil (suche-lösung '() #'rätsel-gelöst #'mögliche-spiele))))

(deftest rätsel-lösen ()
  (check
    (eql t (binoxxo "2016-10-23.txt"))
    (eql t (binoxxo "input.txt"))
    (eql t (binoxxo "input8.txt"))
    (eql t (binoxxo "2015-08-23.txt"))))

(deftest rätsel-erstellen ()
  (check
    (eq t (valides-spiel (erstelle-rätsel 6)))
    (eq t (valides-spiel (erstelle-rätsel 8)))
    (eq t (rätsel-gelöst (suche-lösung (list (erstelle-rätsel 6)) #'rätsel-gelöst #'mögliche-spiele)))
    (eq t (rätsel-gelöst (suche-lösung (list (erstelle-rätsel 8)) #'rätsel-gelöst #'mögliche-spiele)))
    ))

(deftest alle-tests ()
  (combine-results
    (einfache-tests)
    (suche)
    (rätsel-lösen)
    (rätsel-erstellen)))

;;; Teste
(alle-tests)
