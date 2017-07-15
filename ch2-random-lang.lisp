(in-package #:artificial-cl)

;;; - Chapter 2 Random English

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball women table)
    (Verb -> bit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defparameter *polish-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> na o z)
    (Adj -> duży mały zielony niebieski)
    (Article -> ten ta)
    (Name -> Janusz Grażyna Robert Kasia)
    (Noun -> mężczyzna kobieta piłka balkon)
    (Verb -> udeżył zobaczył polubił zabrał)
    (Pronoun -> on ona ono oni my wy)))

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially this is *simple-grammar*,
   but we can switch to other grammars.")

(setf *grammar* *polish-grammar*)

(defun random-elt (choices)
  "Returns random element from list of choices."
  (elt choices (random (length choices))))
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))
(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))
