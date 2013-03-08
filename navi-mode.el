;; * navi-mode.el --- major-mode for easy buffer-navigation
;; ** Copyright

;; Copyright (C) 2013 Thorsten Jolitz

;; Maintainer: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 0.9
;; Keywords: occur

;; ** Licence

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ** Commentary

;; This file implements extensions for occur-mode.
;;
;; ** Installation
;;
;; ** Usage
;;
;; ** Compatibility:

;; * Requires

(require 'outshine)
(require 'outorg)

;; * Mode Definitions

(define-derived-mode navi-mode
  occur-mode "Navi"
  "Major mode for easy buffer-navigation.
In this mode (derived from `occur-mode') you can easily navigate
in an associated original-buffer via one-key commands in the
navi-buffer. You can alter the displayed document structure in
the navi-buffer by sending one-key commands that execute
predefined occur searches in the original buffer. `navi-mode' is
especially useful in buffers with outline structure, e.g. buffers
with `outline-minor-mode' activated and `outshine' extensions
loaded.
\\{navi-mode-map}"
  (set (make-local-variable 'revert-buffer-function) 'navi-revert-function)
  (setq case-fold-search nil))
 
;; * Variables
;; ** Consts
;; ** Vars

(defvar navi-mode-version 0.9
  "Version number of `navi-mode.el'")

(defvar navi "navi"
  "Symbol that holds pairs of buffer-marker names in its plist.
Keys are buffernames as keyword-symbols, values are markers that
point to original-buffers")

(defvar navi-regexp-quoted-line-at-point ""
  "Regexp that matches the line at point in navi-buffer.")

;; ** Hooks
;; ** Fonts
;; ** Customs
;; *** Custom Groups 

(defgroup navi-mode nil
  "Library for outline navigation and Org-mode editing in Lisp buffers."
  :prefix "navi-"
  :group 'lisp)

;; *** Custom Vars

;; TODO one (small basic) set of standard keys for all languages!!
;; :f functions & macros
;; :v variables, consts, customs
;; :x classes, objects, methods, structs, 
;; :b db store & select
;; :a all
;; navigation:
;; :p previous
;; :n next
;; :g revert
;; :d display
;; :o other
;; :c copy
;; :m mark
;; :n narrow
;; :w widen
;; :s switch
;; :k kill (y-or-n-p)
;; :y yank
;; :e edit (outorg)
;; :q quit
;; :h help
;; :+ promote
;; :- demote
;; :t transpose
;; :^ move up (same level)
;; :< move down (same level)


(defcustom navi-key-mappings
  '(("emacs-lisp" . ((:ALL . "a")
                     (:FUN . "f")
                     (:VAR . "v")
                     (:OBJ . "x")
                     (:DB . "b")
                     (:defun . "F")
                     (:defvar . "V")
                     (:defconst . "C")
                     (:defgroup . "G")
                     (:defcustom . "U")
                     (:defadvice . "A")
                     (:defmarcro . "M")
                     (:defface . "E")
                     (:defstruct . "S")
                     (:defclass . "L")))
    ("picolisp" . ((:ALL . "a")
                     (:FUN . "f")
                     (:VAR . "v")
                     (:OBJ . "x")
                     (:DB . "b")
                     (:de . "D")
                   (:def . "F")
                   (:class . "C")
                   (:dm . "M")
                   (:rel . "R")
                   (:var . "V")
                   (:extend . "E")
                   (:obj . "O")
                   (:object . "J")
                   (:new . "N")
                   (:symbols . "S")
                   (:pool . "L")
                   (:tree . "T")
                   (:clause . "U")
                   (:goal . "G")
                   (:be . "B")
                   (:prove . "P"))))
  "Mappings between keybindings and keys in `navi-keywords'"
  :group 'outshine
  :type '(alist :key-type string
                :value-type alist))


(defcustom navi-keywords
  '(("emacs-lisp" . (

                     (:defun . "^[[:space:]]*(defun ")
                     (:defvar . "^[[:space:]]*(defvar ")
                     (:defconst . "^[[:space:]]*(defconst ")
                     (:defgroup . "^[[:space:]]*(defgroup ")
                     (:defcustom . "^[[:space:]]*(defcustom ")
                     (:defadvice . "^[[:space:]]*(defadvice ")
                     (:defmarcro . "^[[:space:]]*(defmacro ")
                     (:defface . "^[[:space:]]*(defface ")
                     (:defstruct . "^[[:space:]]*(defstruct ")
                     (:defclass . "^[[:space:]]*(defclass ")
                     (:defmethod . "^[[:space:]]*(defmethod ")
                     ;; TODO make regexp alternatives!
                     (:ALL . "^[[:space:]]*(def[a-z]+ ")
                     (:OBJ . "^[[:space:]]*(def[smc][^auo][a-z]+ ")
                     (:VAR . "^[[:space:]]*(def[vcgf][^l][a-z]+ ")
                     (:FUN . "^[[:space:]]*(def[mau][^e][a-z]+ ")
                     (:C . "^[[:space:]]*(def[cg][^ol][a-z]+ ")

                     ))
    ("picolisp" . ((:d . "^[[:space:]]*(de ")
                   (:f . "^[[:space:]]*(def ")
                   (:c . "^[[:space:]]*(class ")
                   (:m . "^[[:space:]]*(dm ")
                   (:r . "^[[:space:]]*(rel ")
                   (:v . "^[[:space:]]*(var ")
                   (:x . "^[[:space:]]*(extend ")
                   (:o . "^[[:space:]]*(obj ")
                   (:e . "^[[:space:]]*(object ")
                   (:n . "^[[:space:]]*(new ")
                   (:s . "^[[:space:]]*(symbols ")
                   (:p . "^[[:space:]]*(pool ")
                   (:t . "^[[:space:]]*(tree ")
                   (:u . "^[[:space:]]*(clause ")
                   (:g . "^[[:space:]]*(goal ")
                   (:b . "^[[:space:]]*(be ")
                   (:i . "^[[:space:]]*(prove ")
                   (:C . (concat
                          "^[[:space:]]*("
                          "\\(class \\|"
                          "extend \\|"
                          "dm \\|"
                          "var \\|"
                          "rel \\)"))
                   (:O . (concat
                          "^[[:space:]]*("
                          "\\(pool \\|"
                          "obj \\|"
                          "object \\|"
                          "tree \\|"
                          "new \\)"))
                   (:P . (concat
                          "^[[:space:]]*("
                          "\\(prove \\|"
                          "clause \\|"
                          "goal \\|"
                          "be \\)"))
                   (:D . (concat
                          "^[[:space:]]*("
                          "\\(de \\|"
                          "def \\|"
                          "symbols \\)")))))

;; '(("emacs-lisp" . ((:f . "^[[:space:]]*(defun ")
;;                      (:v . "^[[:space:]]*(defvar ")
;;                      (:c . "^[[:space:]]*(defconst ")
;;                      (:g . "^[[:space:]]*(defgroup ")
;;                      (:u . "^[[:space:]]*(defcustom ")
;;                      (:a . "^[[:space:]]*(defadvice ")
;;                      (:m . "^[[:space:]]*(defmacro ")
;;                      (:e . "^[[:space:]]*(defface ")
;;                      (:s . "^[[:space:]]*(defstruct ")
;;                      (:l . "^[[:space:]]*(defclass ")
;;                      (:h . "^[[:space:]]*(defmethod ")
;;                      (:D . "^[[:space:]]*(def[a-z]+ ")
;;                      (:O . "^[[:space:]]*(def[smc][^auo][a-z]+ ")
;;                      (:V . "^[[:space:]]*(def[vc][^l][a-z]+ ")
;;                      (:F . "^[[:space:]]*(def[mau][a-z]+ ")
;;                      (:C . "^[[:space:]]*(def[cg][^ol][a-z]+ ")))
;;     ("picolisp" . ((:d . "^[[:space:]]*(de ")
;;                    (:f . "^[[:space:]]*(def ")
;;                    (:c . "^[[:space:]]*(class ")
;;                    (:m . "^[[:space:]]*(dm ")
;;                    (:r . "^[[:space:]]*(rel ")
;;                    (:v . "^[[:space:]]*(var ")
;;                    (:x . "^[[:space:]]*(extend ")
;;                    (:o . "^[[:space:]]*(obj ")
;;                    (:e . "^[[:space:]]*(object ")
;;                    (:n . "^[[:space:]]*(new ")
;;                    (:s . "^[[:space:]]*(symbols ")
;;                    (:p . "^[[:space:]]*(pool ")
;;                    (:t . "^[[:space:]]*(tree ")
;;                    (:u . "^[[:space:]]*(clause ")
;;                    (:g . "^[[:space:]]*(goal ")
;;                    (:b . "^[[:space:]]*(be ")
;;                    (:i . "^[[:space:]]*(prove ")
;;                    (:C . (concat
;;                           "^[[:space:]]*("
;;                           "\\(class \\|"
;;                           "extend \\|"
;;                           "dm \\|"
;;                           "var \\|"
;;                           "rel \\)"))
;;                    (:O . (concat
;;                           "^[[:space:]]*("
;;                           "\\(pool \\|"
;;                           "obj \\|"
;;                           "object \\|"
;;                           "tree \\|"
;;                           "new \\)"))
;;                    (:P . (concat
;;                           "^[[:space:]]*("
;;                           "\\(prove \\|"
;;                           "clause \\|"
;;                           "goal \\|"
;;                           "be \\)"))
;;                    (:D . (concat
;;                           "^[[:space:]]*("
;;                           "\\(de \\|"
;;                           "def \\|"
;;                           "symbols \\)")))))
  
  "Alist of language-specific keywords for occur-searches in
  navi-mode.

This customization variable holds a nested alist with 2 levels:

1st level:

The name of the language (key-string) should be the associated
major-mode name without the '-mode' suffix. Run 'M-x major-mode'
in a buffer to find out about the name, in an Emacs Lisp buffer
you get 'emacs-lisp-mode', in a PicoLisp buffer you get
'picolisp-mode', thus the alist keys for these two languages
should be 'emacs-lisp' and 'picolisp'.

2nd level:

The key of each language-alist are one-character keywords used
for selecting the regexp, the value is the regexp itself, e.g.

 (:defcustom . \"^[[:space:]]*(defcustom \")"

  :group 'outshine
  :type '(alist :key-type string
                :value-type alist))

;; * Defuns
;; ** Functions

;; (defun navi-mode-hook-function ()
;;   "Function to be run after `navi-mode' is loaded."
;;   (add-to-list 'occur-hook 'occur-rename-buffer))

;; TODO implement
(defun navi-map-keybinding-to-key ()
  "Map keybinding to key in `navi-keywords'.") 

;; modified `occur-1' from `replace.el'
(defun navi-1 (regexp nlines bufs &optional buf-name)
  (unless (and regexp (not (equal regexp "")))
    (error "Occur doesn't work with the empty regexp"))
  (unless buf-name
    (setq buf-name "*Navi*"))
  (let (occur-buf
	(active-bufs (delq nil (mapcar #'(lambda (buf)
					   (when (buffer-live-p buf) buf))
				       bufs))))
    ;; Handle the case where one of the buffers we're searching is the
    ;; output buffer.  Just rename it.
    (when (member buf-name (mapcar 'buffer-name active-bufs))
      (with-current-buffer (get-buffer buf-name)
	(rename-uniquely)))

    ;; Now find or create the output buffer.
    ;; If we just renamed that buffer, we will make a new one here.
    (setq occur-buf (get-buffer-create buf-name))

    (with-temp-buffer
      (setq navi-tmp-buffer-marker (point-marker))
      (if (stringp nlines)
	  (fundamental-mode) ;; This is for collect operation.
        (navi-mode))
      (let ((inhibit-read-only t)
	    ;; Don't generate undo entries for creation of the initial contents.
	    (buffer-undo-list t))
	(let ((count
	       (if (stringp nlines)
                   ;; Treat nlines as a regexp to collect.
		   (let ((bufs active-bufs)
			 (count 0))
		     (while bufs
		       (with-current-buffer (car bufs)
			 (save-excursion
			   (goto-char (point-min))
			   (while (re-search-forward regexp nil t)
                             ;; Insert the replacement regexp.
			     (let ((str (match-substitute-replacement nlines)))
			       (if str
				   (with-current-buffer
                                       (marker-buffer navi-tmp-buffer-marker)
				     (insert str)
				     (setq count (1+ count))
				     (or (zerop (current-column))
					 (insert "\n"))))))))
                       (setq bufs (cdr bufs)))
                     count)
		 ;; Perform normal occur.
		 (occur-engine
		  regexp active-bufs (marker-buffer navi-tmp-buffer-marker)
		  (or nlines list-matching-lines-default-context-lines)
		  (if (and case-fold-search search-upper-case)
		      (isearch-no-upper-case-p regexp t)
		    case-fold-search)
		  list-matching-lines-buffer-name-face
		  nil list-matching-lines-face
		  (not (eq occur-excluded-properties t))))))
	  (let* ((bufcount (length active-bufs))
		 (diff (- (length bufs) bufcount)))
	    (message "Searched %d buffer%s%s; %s match%s%s"
		     bufcount (if (= bufcount 1) "" "s")
		     (if (zerop diff) "" (format " (%d killed)" diff))
		     (if (zerop count) "no" (format "%d" count))
		     (if (= count 1) "" "es")
		     ;; Don't display regexp if with remaining text
		     ;; it is longer than window-width.
		     (if (> (+ (length regexp) 42) (window-width))
			 "" (format " for `%s'" (query-replace-descr regexp)))))
          (if (= count 0)
              nil
            (with-current-buffer occur-buf
              (setq occur-revert-arguments (list regexp nlines bufs))
              (erase-buffer)
              (insert-buffer-substring
               (marker-buffer navi-tmp-buffer-marker))
              (display-buffer occur-buf)
              (setq next-error-last-buffer occur-buf)
              (setq buffer-read-only t)
              (set-buffer-modified-p nil)
              (run-hooks 'occur-hook)))))
      (set-marker navi-tmp-buffer-marker nil))))


(defun non-empty-string-p (str)
  "Return t if function argument STR is a string of length > 0, nil otherwise."
 (if (and (stringp str) (> (length str) 0))
     str
   nil))

(defun navi-get-regexp (language key)
  "Return the value of KEY for LANGUAGE in `navi-keywords'."
  (if (not (and (non-empty-string-p language)
                (assoc language navi-keywords)))
      (progn
        (message
         (format "%s%s%s"
          "Language "
          language
          " not registered in 'navi-keywords'"))
        nil)
    (let* ((result (assoc key (cdr (assoc language navi-keywords))))
           (rgxp  (and result (cdr result))))
      (cond
       ((stringp rgxp) rgxp)
       ((and (listp rgxp) (functionp (car rgxp)) (eval rgxp)))
       (t nil)))))

;; TODO deeper test of the results
(defun navi-make-regexp-alternatives (&rest rgxps)
  "Enclose the set of regexp alternatives.
The regexps are given as the list of strings RGXPS."
  (and rgxps
       (replace-regexp-in-string
        (regexp-quote "\\|\\)")
        (regexp-quote "\\)")
        (concat
         "\\("
         (mapconcat
          'identity rgxps "\\|")
         "\\)"))))

(defun navi-get-language-alist (language)
  "Return the alist with keys and regexps for LANGUAGE from `navi-keywords'."  
  (if (not (and (non-empty-string-p language)
                (assoc language navi-keywords)))
      (error (concat "Language not registered in customizable "
                     "variable 'navi-keywords'"))
     (cdr (assoc language navi-keywords))))

(defun navi-set-regexp-quoted-line-at-point ()
  "Set `navi-regexp-quoted-line-at-point' to the value calculated by
`navi-regexp-quote-line-at-point'."
  (setq navi-regexp-quoted-line-at-point
        (navi-regexp-quote-line-at-point))
  (format "%s" navi-regexp-quoted-line-at-point))

(defun navi-regexp-quote-line-at-point ()
  "Store a quoted regexp for line at point.
Leading and trailing whitespace is deleted."
  ;; (setq navi-regexp-quoted-line-at-point
  (regexp-quote
   (outshine-chomp
    (substring-no-properties
     (buffer-string) (point-at-bol) (point-at-eol)))))
  ;; (format "%s" navi-regexp-quoted-line-at-point))

(defun navi-get-line-number-from-regexp-quoted-line-at-point (rgxp)
  "Return as Integer the line number in regexp-quoted-line-at-point."
  (string-to-int
   (car (split-string rgxp ":" 'OMIT-NULLS))))

(defun navi-in-buffer-headline-p ()
  "Return `line-number-at-position' if in first line, nil otherwise."
  (and (string-equal major-mode "navi-mode")
       (if (eq (line-number-at-pos) 1) 1 nil)))

(defun navi-search-less-or-equal-line-number (&optional num)
  "Search closest result-line to given line-number.
This function searches a result-line in a navi-buffer with
line-number less-or-equal to line-number of
`navi-regexp-quoted-line-at-point' or NUM. Its not about
line-numbers in the navi-buffer, but about the line-numbers in
the original-buffer shown in the occur-search results."
  (let* ((line-num (or
                    (and num (integer-or-marker-p num) (>= num 1) num)
                    (navi-get-line-number-from-regexp-quoted-line-at-point
                     navi-regexp-quoted-line-at-point)))
         (line-num-str (int-to-string line-num))
         (match-point))
    (save-excursion
      (goto-char (point-min))
      (forward-line)
      (unless (< line-num
                 (navi-get-line-number-from-regexp-quoted-line-at-point
                  (navi-regexp-quote-line-at-point)))
        (forward-line -1)
        (while (and (>= line-num 1)
                    (not
                     (setq match-point
                           (re-search-forward
                            (concat "^[[:space:]]*"
                                    line-num-str
                                    ":") 
                            nil 'NO-ERROR))))
          (goto-char (point-min))
          (setq line-num (1- line-num))
          (setq line-num-str (int-to-string line-num)))
        (if match-point
            (goto-char match-point)
          (forward-line)))
      (forward-line)
      (occur-prev)
      (point))))

(defun navi-goto-occurrence-other-window ()
  "Moves navi-buffer marker to point before switching buffers."
  (interactive)
  (move-marker
   (car (navi-get-twin-buffer-markers)) (point))
  (occur-mode-goto-occurrence-other-window))

(defun navi-make-buffer-key (&optional buf)
  "Return the (current) buffer-name or string BUF as interned keyword-symbol"
  (let ((buf-name
         (file-name-sans-extension
          (car (split-string (or buf (buffer-name)) "[*]" 'OMIT-NULLS)))))
  (intern (concat ":" buf-name))))

(defun navi-make-marker-name (&optional buf)
  "Return marker-name by expansion of (current) buffer-name or string BUF."
  (let ((buf-name
         (file-name-sans-extension
          (car (split-string (or buf (buffer-name)) "[*]" 'OMIT-NULLS)))))
  (concat buf-name "-marker")))

(defun navi-get-twin-buffer-markers ()
  "Return list with two markers pointing to buffer-twins or nil.
CAR of the return-list is always the marker pointing to
 current-buffer, CDR the marker pointing to its twin-buffer."
  (let* ((curr-buf-split
          (split-string (buffer-name) "[*:]" 'OMIT-NULLS))
         (is-navi-buffer-p
          (string-equal (car curr-buf-split) "Navi"))
         (twin-of-navi
          (and is-navi-buffer-p
               (get 'navi (navi-make-buffer-key (cadr curr-buf-split)))))
         (self-navi
          (and is-navi-buffer-p
               (get 'navi (navi-make-buffer-key
                           (concat
                            (car curr-buf-split)
                            ":"
                            (cadr curr-buf-split))))))
         (twin-of-orig
          (unless is-navi-buffer-p
            (get 'navi (navi-make-buffer-key
                        (concat "Navi:" (car curr-buf-split))))))
         (self-orig
          (unless is-navi-buffer-p
            (get 'navi (navi-make-buffer-key (car curr-buf-split))))))
    (if is-navi-buffer-p
        (and self-navi twin-of-navi
             (list self-navi twin-of-navi))
      (and self-orig twin-of-orig
           (list self-orig twin-of-orig)))))


;; modified `occur-rename-buffer' from `replace.el'
(defun navi-rename-buffer (&optional unique-p)
  "Rename the current *Occur* buffer to *Navi: original-buffer-name*.
Here `original-buffer-name' is the buffer name where Occur was
originally run. When given the prefix argument, the renaming will
not clobber the existing buffer(s) of that name, but use
`generate-new-buffer-name' instead. You can add this to
`occur-hook' if you always want a separate *Occur* buffer for
each buffer where you invoke `occur'."
  (let ((orig-buffer-name ""))
    (with-current-buffer
        (if (eq major-mode 'occur-mode) (current-buffer) (get-buffer "*Occur*"))
      (setq orig-buffer-name
            (mapconcat
             #'buffer-name
             (car (cddr occur-revert-arguments)) "/"))
      (rename-buffer (concat "*Navi:" orig-buffer-name "*") unique-p)
      ;; make marker for this navi-buffer
      ;; and store it in `navi''s plist
      (put 'navi
           (navi-make-buffer-key)
           (set
            (intern
             (navi-make-marker-name
              (cadr (split-string (buffer-name) "[*:]" 'OMIT-NULLS))))
            (point-marker))))))

(defun navi-calc-headline-regexp (level &optional NO-PARENT-LEVELS)
  "Calculate regexp to show headers of original-buffer.
Regexp should result in an occur-search showing up to
outline-level LEVEL headlines in navi-buffer. If NO-PARENT-LEVELS
in non-nil, only headers of level LEVEL are shown."
  (let* ((orig-buf (marker-buffer
                    (cadr (navi-get-twin-buffer-markers))))
         (outline-base-string
          (with-current-buffer orig-buf
            (outshine-transform-normalized-outline-regexp-base-to-string)))
         (rgxp-string
          (regexp-quote
           (outshine-chomp
            (format
             "%s" (car (rassoc 1 (with-current-buffer orig-buf
                                   outline-promotion-headings)))))))
         (rgxp (if (not (and level
                             (integer-or-marker-p level)
                             (>= level 1)
                             (<= level 8)))
                   (error "Level must be an integer between 1 and 8")
                 (if NO-PARENT-LEVELS
                     (regexp-quote
                      (format
                       "%s"
                       (car
                        (rassoc level
                                (with-current-buffer orig-buf
                                  outline-promotion-headings)))))
                   (concat
                    (dotimes (i (1- level) rgxp-string)
                      (setq rgxp-string
                            (concat rgxp-string
                                    (regexp-quote
                                     outline-base-string)
                                    "?")))
                    " ")))))
    rgxp))


(defun navi-show-headers (level &optional args)
  "Show headers up-to level LEVEL."
  (if args
      (navi-revert-function
       (navi-calc-headline-regexp level 'NO-PARENT-LEVELS))
    (navi-revert-function
     (navi-calc-headline-regexp level))))


;; FIXME magit-like (org-export-like) selection menu
(defun navi-show-keywords (key)
  "Show matches of occur-search with KEY.
Language is derived from major-mode."
  (let ((language
         (with-current-buffer
             (marker-buffer
              (cadr (navi-get-twin-buffer-markers)))
           (car
            (split-string
             (symbol-name major-mode)
             "-mode" 'OMIT-NULLS)))))
    (navi-revert-function
     (navi-get-regexp language key))))


(defun navi-show-headers-and-keywords (level key &optional args)
  "Show headers up-to level LEVEL and matches of occur-search with KEY.
Language is derived from major-mode."
  (let* ((language
          (with-current-buffer
              (marker-buffer
               (cadr (navi-get-twin-buffer-markers)))
            (car
             (split-string
              (symbol-name major-mode)
              "-mode" 'OMIT-NULLS))))
         (rgxp
          (navi-make-regexp-alternatives
           (if args
               (navi-calc-headline-regexp level 'NO-PARENT-LEVELS)
             (navi-calc-headline-regexp level))
           (navi-get-regexp language key))))
    (navi-revert-function rgxp)))

(defun navi-clean-up ()
  "Clean up `navi' plist and left-over markers after killing navi-buffer."
  (setq navi-revert-arguments nil)
  (setq navi-regexp-quoted-line-at-point nil))


;; (add-to-list 'occur-hook 'navi-rename-buffer)

;; ** Commands

;; Convenience function copied from whom ??
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun navi-search-and-switch ()
  "Call `occur' and immediatley switch to `*Navi:original-buffer-name*' buffer"
  (interactive)
  (let ((1st-level-headers
         (regexp-quote
          (outshine-calc-outline-string-at-level 1))))
    (put 'navi (navi-make-buffer-key (buffer-name))
         (set (intern (navi-make-marker-name)) (point-marker)))
    (occur 1st-level-headers)
    (navi-rename-buffer)
    (navi-switch-to-twin-buffer)
    (navi-mode)
    (occur-next)
    (move-marker
     (car (navi-get-twin-buffer-markers)) (point))
    (navi-set-regexp-quoted-line-at-point)))

;; (defun navi-quit-and-switch ()
;;   "Quit navi-buffer and immediatley switch back to original-buffer"
;;   (interactive)
;;   (quit-window)
;;   (switch-to-buffer
;;    (marker-buffer original-buffer-marker))
;;   (goto-char
;;    (marker-position original-buffer-marker)) ; necessary?
;;   (set-marker navi-buffer-marker nil)
;;   (set-marker original-buffer-marker nil))

(defun navi-switch-to-twin-buffer ()
  "Switch to associated twin-buffer of current buffer or do nothing."
  (interactive)
  (let* ((marker-list (navi-get-twin-buffer-markers))
         (self-marker (car marker-list))
         (twin-marker (cadr marker-list)))
    (and marker-list
         (move-marker self-marker (point) (marker-buffer self-marker))
         (switch-to-buffer-other-window (marker-buffer twin-marker))
         (goto-char (marker-position twin-marker))
         (and (eq major-mode 'navi-mode)
              (navi-revert-function)))))

;; adapted from 'replace.el'
(defun navi-revert-function (&optional regexp)
  "Handle `revert-buffer' for navi-buffers."
  (interactive)
  (let ((navi-revert-arguments
         (if regexp
            (append
             (list regexp) (cdr occur-revert-arguments))
           occur-revert-arguments)))
    (navi-set-regexp-quoted-line-at-point)
    ;; (apply 'occur-1 (append navi-revert-arguments (list (buffer-name))))
    (apply 'navi-1 (append navi-revert-arguments (list (buffer-name))))
    ;; redundant with navi-1 instead of occur-1?
    (unless
        (string-equal major-mode "navi-mode") (navi-mode))
    (goto-char 
      (navi-search-less-or-equal-line-number))))

;; convenience function
(defun navi-show-headers-1 (args)
  "Show headers up to level 1"
  (interactive "P")
  (if args
      (navi-show-headers 1 args)
    (navi-show-headers 1)))

;; convenience function
(defun navi-show-headers-2 (args)
  "Show headers up to level 2"
  (interactive "P")
  (if args
      (navi-show-headers 2 args)
    (navi-show-headers 2)))

;; convenience function
(defun navi-show-headers-3 (args)
  "Show headers up to level 3"
  (interactive "P")
  (if args
      (navi-show-headers 3 args)
    (navi-show-headers 3)))

;; convenience function
(defun navi-show-headers-4 (args)
  "Show headers up to level 4"
  (interactive "P")
  (if args
      (navi-show-headers 4 args)
    (navi-show-headers 4)))

;; convenience function
(defun navi-show-headers-5 (args)
  "Show headers up to level 5"
  (interactive "P")
  (if args
      (navi-show-headers 5 args)
    (navi-show-headers 5)))

;; convenience function
(defun navi-show-headers-6 (args)
  "Show headers up to level 6"
  (interactive "P")
  (if args
      (navi-show-headers 6 args)
    (navi-show-headers 6)))

;; convenience function
(defun navi-show-headers-7 (args)
  "Show headers up to level 7"
  (interactive "P")
  (if args
      (navi-show-headers 7 args)
    (navi-show-headers 7)))

;; convenience function
(defun navi-show-headers-8 (args)
  "Show headers up to level 8"
  (interactive "P")
  (if args
      (navi-show-headers 8 args)
    (navi-show-headers 8)))

;; * Keybindings

;; example keybinding
;; (define-key navi-mode-map
;;  [down-mouse-3] 'do-hyper-link)


;; Occur mode: new/better keybindings
(global-set-key (kbd "M-s n") 'navi-search-and-switch)
(global-set-key (kbd "M-s s") 'navi-switch-to-twin-buffer)
(global-set-key (kbd "M-s M-s") 'navi-switch-to-twin-buffer)
(define-key navi-mode-map (kbd "s") 'navi-switch-to-twin-buffer)
(define-key navi-mode-map (kbd "d") 'occur-mode-display-occurrence)
(define-key navi-mode-map (kbd "o") 'navi-goto-occurrence-other-window)
(define-key navi-mode-map (kbd "n") 'occur-next)
(define-key navi-mode-map (kbd "p") 'occur-prev)
(define-key navi-mode-map (kbd "r") 'navi-revert-function)
(define-key navi-mode-map (kbd "q") 'navi-quit-and-switch)
(define-key navi-mode-map (kbd "1") 'navi-show-headers-1)
(define-key navi-mode-map (kbd "2") 'navi-show-headers-2)
(define-key navi-mode-map (kbd "3") 'navi-show-headers-3)
(define-key navi-mode-map (kbd "4") 'navi-show-headers-4)
(define-key navi-mode-map (kbd "5") 'navi-show-headers-5)
(define-key navi-mode-map (kbd "6") 'navi-show-headers-6)
(define-key navi-mode-map (kbd "7") 'navi-show-headers-7)
(define-key navi-mode-map (kbd "8") 'navi-show-headers-8)
(define-key isearch-mode-map (kbd "M-s i") 'isearch-occur)

;; TODO define prefix-key "^" for combining letters with keywords
;; e.g. 'C-3 d'b
;; transform chars into keyword-symbols (d -> :d)

;; * Run Hooks and Provide

(provide 'navi-mode)

;; navi-mode.el ends here

