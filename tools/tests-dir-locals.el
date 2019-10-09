;; Don't show generated files in the tests directory when using Emacs' dired
;; mode.  From the root directory, run:
;;
;; ln -s ../tools/tests-dir-locals.el tests/.dir-locals.el

((nil
  . ((dired-omit-files
      . "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$\\|\\.c\\|\\.bin$"))))
