;;-*-coding: utf-8;-*-

(define-abbrev-table 'makefile-mode-abbrev-table
  '(("ph" ".PHONY:" nil 0)))

(define-abbrev-table 'python-mode-abbrev-table
  '(("ifn" "if __name__ == \"__main__\":\n    " nil 0)
    ("pdb" "import pdb; pdb.set_trace()  # debug" nil 0)
    ("shb" "#!/usr/bin/env python3\n" nil 0)))

(define-abbrev-table 'sh-mode-abbrev-table
  '(("shb" "#!/usr/bin/env bash\n" nil 0)))

(define-abbrev-table 'org-mode-abbrev-table
  '(("bs" "#+begin_src " nil 0)
    ("bsb" "#+begin_src bash " nil 0)
    ("bsp" "#+begin_src python " nil 0)
    ("bse" "#+begin_src emacs-lisp" nil 0)
    ("es" "#+end_src" nil 0)))
