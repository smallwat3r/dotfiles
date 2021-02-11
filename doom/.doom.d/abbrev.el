;; -*- coding: utf-8; lexical-binding: t; -*-

;; Manage abbrev

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; General
    ("btw"  "by the way")
    ("pov"  "point of view")
    ("atm"  "at the moment" )
    ("plz"  "please")
    ("ty"   "thank you")
    ("afaict" "as far as I can tell")

    ;; Typos
    ("altough"  "although")
    ("widht"    "width")
    ("thougth"  "thought")
    ("tought"   "thought")
    ("lenght"   "length")
    ("strenght" "strength")
    ("weigth"   "weight")
    ("wether"   "whether")

    ;; Unicode stuff
    ("uno" "✗")
    ("uok" "✓")
    ("ura" "→" )

    ;; Personal stuff
    ("zma" "Matthieu Petiteau")
    ("zme" "mpetiteau.pro@gmail.com")
    ("zco" "Copyright 2020 Matthieu Petiteau, all rights reserved.")
    ))

;;
;; Define abbreviations from specific major-mode
;;

(define-abbrev-table 'prog-mode-abbrev-table
  '(
    ("rt" "return")
    ("ud" "undefined")
    ))

(define-abbrev-table 'sh-mode-abbrev-table
  '(
    ("shb" "#!/usr/bin/env bash\n")
    ("ec" "echo")
    ("pf" "printf")
    ))

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("shb" "#!/usr/bin/env python\n")
    ("ffi" "from flask import")
    ("pr" "print")
    ("cl" "class")
    ("ifn" "if __name__ == \"__main__\":\n    ")
    ("pdb" "import pdb; pdb.set_trace()  # Debug")
    ))

(define-abbrev-table 'sql-mode-abbrev-table
  '(
    ("s" "SELECT")
    ("f" "FROM")
    ("w" "WHERE")
    ("d" "DISTINCT")
    ("ob" "ORDER BY")
    ("gb" "GROUP BY")
    ("l" "LIMIT")
    ("u" "UNION")
    ("j" "JOIN")
    ("lj" "LEFT JOIN")
    ("rj" "RIGHT JOIN")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
