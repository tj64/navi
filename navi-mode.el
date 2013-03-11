;; * navi-mode.el --- major-mode for easy buffer-navigation
;; ** Copyright

;; Copyright (C) 2013 Thorsten Jolitz

;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Maintainer: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 0.9
;; Created: 11th March 2013
;; Keywords: occur, outlines, navigation

;; ** Licence

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ** Credits

;; This library is inspired by the Org-mode command `org-goto', which shows
;; the documemt structure of an Org-mode file in a temporary buffer, and
;; enables navigation in that buffer without changing the original-buffer's
;; visibility-level, and by the authors frequent use of (M-x) `occur' (and the
;; resulting *Occur* buffer) for buffer navigation purposes.

;; ** Commentary

;; *** About navi-mode

;; This file implements extensions for occur-mode. You can think of a navi-buffer
;; as a kind of 'remote-control' for an (adecuately) outline-structured
;; original-buffer. It enables quick navigation and basic structure editing in
;; the original-buffer without (necessarily) leaving the navi-buffer. When
;; switching to the original-buffer and coming back after some modifications, the
;; navi-buffer is always reverted (thus up-to-date).

;; Besides the fundamental outline-heading-searches (8 outline-levels) and the 5
;; basic keyword-searches (:FUN, :VAR, :DB, :OBJ and :ALL), all languages can
;; have their own set of searches and keybindings (see `navi-key-mappings' and
;; `navi-keywords'). Heading-searches and keyword-searches can be combined,
;; offering a vast amount of possible 'views' at the original-buffer.

;; *** Installation

;; Download (or clone the github-repos of) the three required libraries

;; | `navi-mode.el' | (https://github.com/tj64/navi)     |
;; | `outshine.el'  | (https://github.com/tj64/outshine) |
;; | `outorg.el'    | (https://github.com/tj64/outorg)   |

;; and put them in a place where Emacs can find them (on the Emacs 'load-path').
;; Follow the installation instructions in `outshine.el' and `outorg.el'.

;; Install `navi-mode.el' by adding

;; ;; #+begin_src emacs-lisp
;; ;;  (require 'navi-mode)
;; ;; #+end_src

;; to your .emacs file. 

;; *** Usage

;; For `navi-mode' to work, the original-buffer must be outline-structured 'the
;; outshine way', i.e. with the headlines being proper Org-mode headlines, marked
;; and outcommented with `comment-region'. As an example, to generate a 3rd level
;; outshine-headline in an Emacs Lisp file, write down

;; ,-----------------------
;; | *** Third Level Header
;; `-----------------------

;; mark the header line, and apply `comment-region' on it:

;; ,-----------------------
;; | ;; *** Third Level Header
;; `-----------------------

;; In a LaTeX file, an adecuate header will look like this:

;; ,-----------------------
;; | % *** Third Level Header
;; `-----------------------

;; and in a PicoLisp file like this (always depending of the major-mode specific
;; values of `comment-start', `comment-end', `comment-add' and
;; `comment-padding'):

;; ,-----------------------
;; | ## *** Third Level Header
;; `-----------------------

;; The second assumption is that `outline-minor-mode' is activated in the
;; original-buffer and `outshine.el' loaded like described in its installation
;; instructions, i.e.

;; ;; #+begin_src emacs-lisp
;; ;;  (require 'outshine)
;; ;;  (add-hook ‘outline-minor-mode-hook ‘outshine-hook-function)
;; ;; #+end_src

;; When these pre-conditions are fullfilled (`outorg.el' must be loaded too), you
;; can use 'M-s n' (`navi-search-and-switch') to open a navi-buffer and
;; immediately switch to it. The new navi-buffer will show the first-level
;; headings of the original-buffer, with point at the first entry.

;; You can then:

;; 1. Show headlines (up-to) different levels:

;; | key     | command            | function-name        |
;; |---------+--------------------+----------------------|
;; | 1 ... 8 | show levels 1 to 8 | navi-generic-command |

;; 2. Navigate up and down in the search results shown in the navi-buffer:

;; | key | command  | function-name |
;; |-----+----------+---------------|
;; | p   | previous | occur-prev    |
;; | DEL |          |               |
;; | n   | next     | occur-next    |
;; | SPC |          |               |

;; 3. Revert the navi-buffer (seldom necessary), show help for the user-defined
;;    keyword-searches, and quit the navi-buffer and switch-back to the
;;    original-buffer:

;; | key | command                   | function-name        |
;; |-----+---------------------------+----------------------|
;; | g   | revert buffer             | navi-revert-function |
;; | h   | show help                 | navi-show-help       |
;; | q   | quit navi-mode and switch | navi-quit-and-switch |

;; 4. Switch to the original-buffer and back to the navi-buffer, display and
;;    occurence in the original-buffer or go to the occurence:

;; | key     | command                | function-name                     |
;; |---------+------------------------+-----------------------------------|
;; | M-s n   | launch navi-buffer     | navi-search-and-switch            |
;; | M-s s   | switch to other buffer | navi-switch-to-twin-buffer        |
;; | M-s M-s |                        |                                   |
;; | s       |                        |                                   |
;; | d       | display occurrence     | occur-mode-display-occurrence     |
;; | o       | goto occurrence        | navi-goto-occurrence-other-window |

;; 5. Structure editing on subtrees and visibility cycling

;; | key       | command                        | function-name          |
;; |-----------+--------------------------------+------------------------|
;; | TAB       | cycle subtrees                 | navi-cycle-subtree     |
;; | <backtab> | cycle buffer                   | navi-cycle-buffer      |
;; | +         | Demote Subtree                 | navi-demote-subtree    |
;; | -         | promote subtree                | navi-promote-subtree   |
;; | \^        | move up subtree (same level)   | navi-move-up-subtree   |
;; | <         | move down subtree (same level) | navi-move-down-subtree |

;; 6. Miscancellous actions on subtrees

;; | key | command                    | function-name                     |
;; |-----+----------------------------+-----------------------------------|
;; | m   | mark subtree               | navi-mark-subtree-and-switch      |
;; | c   | copy subtree               | navi-copy-subtree-to-register-s   |
;; | k   | kill subtree               | navi-kill-subtree                 |
;; | y   | yank killed/copied subtree | navi-yank-subtree-from-register-s |
;; | u   | undo last change           | navi-undo                         |
;; | r   | narrow to subtree          | navi-narrow-to-subtree            |
;; | w   | widen                      | navi-widen                        |
;; | l   | query-replace              | navi-query-replace                |
;; | i   | isearch                    | navi-isearch                      |
;; | e   | edit as org (outorg)       | navi-edit-as-org                  |

;; 7. Furthermore, there are five (semantically) predefined keyword-searches:

;; | key | keyword-symbol | searches for               |
;; |-----+----------------+----------------------------|
;; | f   | :FUN           | functions, macros etc.     |
;; | v   | :VAR           | vars, consts, customs etc. |
;; | x   | :OBJ           | OOP (classes, methods etc) |
;; | b   | :DB            | DB (store and select)      |
;; | a   | :ALL           | all                        |


;; 8. And (potentially) many more user-defined keyword-searches
;; (example Emacs Lisp):

;; | key | keyword-symbol | searches for |
;; |-----+----------------+--------------|
;; | F   | :defun         | (defun       |
;; | V   | :defvar        | (defvar      |
;; | C   | :defconst      | (defconst    |
;; | G   | :defgroup      | (defgroup    |
;; | U   | :defcustom     | (defcustom   |
;; | A   | :defadvice     | (defadvice   |
;; | M   | :defmacro      | (defmacro    |
;; | E   | :defface       | (defface     |
;; | S   | :defstruct     | (defstruct   |
;; | L   | :defclass      | (defclass    |

;; 9. Headline-searches and keyword-searches can be combined, e.g.

;; ,------
;; | C-2 f
;; `------

;; in an Emacs Lisp (outshine-)buffer shows all headlines up-to level 2 as well
;; as all function, macro and advice definitions in the original-buffer,

;; ,------
;; | C-5 a
;; `------

;; shows all headlines up-to level 5 as well as all functions, variables,
;; classes, methods, objects, and database-related definitions. The exact meaning
;; of the standard keyword-searches 'f' and 'a' must be defined with a regexp in
;; the customizable variable `navi-keywords' (just like the user-defined
;; keyword-searches).

;; *** Emacs Version

;; `navi-mode.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+
;; Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing with older
;; versions or other types of Emacs have been made (yet).

;; ** ChangeLog

;; | date            | author(s)       | version |
;; |-----------------+-----------------+---------|
;; | <2013-03-11 Mo> | Thorsten Jolitz |     0.9 |


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

(defvar navi-regexp-quoted-line-before-narrowing ""
  "Regexp that matched the line at point in navi-buffer before narrowing.")

;; ** Hooks

;; (defvar navi-mode-hook nil
;;   "Hook run after navi-mode is called.")

;; ** Fonts
;; ** Customs
;; *** Custom Groups 

(defgroup navi-mode nil
  "Library for outline navigation and Org-mode editing in Lisp buffers."
  :prefix "navi-"
  :group 'lisp)

;; *** Custom Vars

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
  "Mappings between keybindings and keyword-symbols used in `navi-keywords'.

All ASCII printing characters (see
http://www.csgnetwork.com/asciiset.html) are available as keys,
except those used for the core commands of 'navi-mode' itself:

| key | command                        |
|-----+--------------------------------|
| p   | previous                       |
| DEL |                                |
| n   | next                           |
| SPC |                                |
| g   | revert buffer                  |
| d   | display occurrence             |
| o   | goto occurrence                |
| c   | copy subtree                   |
| m   | mark subtree                   |
| n   | narrow to subtree              |
| w   | widen                          |
| s   | switch (to original buffer)    |
| k   | kill subtree                   |
| i   | isearch                        |
| l   | query-replace                  |
| y   | yank killed/copied subtree     |
| q   | quit navi-mode and switch      |
| h   | show help                      |
| +   | demote subtree                 |
| -   | promote subtree                |
| \^  | move up subtree (same level)   |
| <   | move down subtree (same level) |


And you should not use the following keys and (uppercase)
keyword-symbols for other than the (semantically) predefined
keywords-searches. They define the 5 standard occur-searches that
should be available for every programming language, with the same
keybindings and similar semantics:

| key | keyword-symbol | command                    |
|-----+----------------+----------------------------|
| f   | :FUN           | functions, macros etc.     |
| v   | :VAR           | vars, consts, customs etc. |
| x   | :OBJ           | OOP (classes, methods etc) |
| b   | :DB            | DB (store and select)      |
| a   | :ALL           | all                        |

All other ASCII printing characters are free and can be used as
one-key keybindings for occur-searches for a programming
language. The keybindings are independent for different
programming languages, so while it would be a good thing to have
similar bindings in different languages, it is by no means
necessary.

Defining occur-searches for a programming language is a two-step
process:
 
 1. Customize `navi-key-mappings', i.e. add a new language-alist
    and populate it with pairs of keyword-symbols (that should
    represent the language keywords searched for) and ASCII
    characters (as strings of length 1).

 2. Customize `navi-keywords', i.e. add a new language alist and
    populate it with pairs of keyword-symbols (that should
    represent the language keywords searched for) and regexps,
    using exactly the same keyword-symbols as in
    `navi-key-mappings'.

Thus, the following two entries together will map the keybinding
'a' to an occur-search with the regexp:

\"^[[:space:]]*(def[a-z]+\":

;; #+begin_src emacs-lisp
;; (defcustom navi-key-mappings
;;   '((\"emacs-lisp\" . ((:ALL . \"a\") ... ))))

;; (defcustom navi-keywords
;;   '((\"emacs-lisp\" . ((:ALL . \"^[[:space:]]*(def[a-z]+ \") ...))))
;; #+end_src

There is no need for a third step - defining the keybindings. In
`navi-mode', there are by default keybindings defined for all
ASCII printing characters. Conditional on the programming
language major-mode of the original-buffer, navi-mode checks the
customizable variables `navi-key-mappings' and `navi-keywords'
for an entry with a key pressed by the user. If it doesn't find
one, nothing happens, if it finds one, it looks up the associated
regexp and performs an occur-search with it."
  :group 'outshine
  :type '(alist :key-type string
                :value-type alist))

(defcustom navi-keywords
  '(("emacs-lisp" . ((:ALL . "^[[:space:]]*(def[a-z]+ ")
                     (:OBJ . "^[[:space:]]*(def[smc][^auo][a-z]+ ")
                     (:VAR . "^[[:space:]]*(def[vcgf][^l][a-z]+ ")
                     (:FUN . "^[[:space:]]*(def[mau][^e][a-z]* ")
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
                     (:defmethod . "^[[:space:]]*(defmethod ")))
    ("picolisp" . ((:de . "^[[:space:]]*(de ")
                   (:def . "^[[:space:]]*(def ")
                   (:class . "^[[:space:]]*(class ")
                   (:dm . "^[[:space:]]*(dm ")
                   (:rel . "^[[:space:]]*(rel ")
                   (:var . "^[[:space:]]*(var ")
                   (:extend . "^[[:space:]]*(extend ")
                   (:obj . "^[[:space:]]*(obj ")
                   (:object . "^[[:space:]]*(object ")
                   (:new . "^[[:space:]]*(new ")
                   (:symbols . "^[[:space:]]*(symbols ")
                   (:pool . "^[[:space:]]*(pool ")
                   (:tree . "^[[:space:]]*(tree ")
                   (:clause . "^[[:space:]]*(clause ")
                   (:goal . "^[[:space:]]*(goal ")
                   (:be . "^[[:space:]]*(be ")
                   (:prove . "^[[:space:]]*(prove ")
                   (:OBJ . (concat
                            "^[[:space:]]*("
                            "\\(class \\|"
                            "extend \\|"
                            "dm \\|"
                            "var \\|"
                            "rel \\)"))
                   (:DB . (concat
                           "^[[:space:]]*("
                           "\\(pool \\|"
                           "obj \\|"
                           "object \\|"
                           "tree \\|"
                           "new \\|"
                           "prove \\|"
                           "clause \\|"
                           "goal \\|"
                           "be \\)"))
                   (:FUN . (concat
                            "^[[:space:]]*("
                            "\\(de \\|"
                            "def \\|"
                            "symbols \\)")))))

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

The keys of each language-alist are keywords-symbols used for
selecting the regexp, the value is the regexp itself"
  :group 'outshine
  :type '(alist :key-type string
                :value-type alist))

;; * Defuns
;; ** Functions

;; (defun navi-mode-hook-function ()
;;   "Function to be run after `navi-mode' is loaded.")

(defun navi-map-keyboard-to-key (language kbd-key)
  "Map pressed keyboard-key KBD-KEY to key in `navi-keywords'."
  (let ((mappings (navi-get-language-alist language 'MAPPINGS)))
    (and (rassoc kbd-key mappings)
         (car (rassoc kbd-key mappings)))))

(defun navi-msg (key language)
  "Tell user that key is not defined for language."
  (message "Key %s is not defined for language %s" key language))

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

(defun navi-get-language-alist (language &optional MAPPINGS)
  "Return the alist with keys and regexps for LANGUAGE from `navi-keywords'.
If MAPPINGS is non-nil, return the alist with key-mappings from
`navi-key-mappings' instead."
(let ((custom-alist (if MAPPINGS navi-key-mappings navi-keywords)))
  (if (not (and (non-empty-string-p language)
                (assoc language custom-alist)))
      (message "Language not registered in customizable variable `%s'"
                (symbol-name custom-alist))
    (cdr (assoc language custom-alist)))))

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
  (and (eq major-mode 'navi-mode)
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


(defun navi-get-language-name ()
  "Return language name for major-mode of original-buffer."
  (with-current-buffer
      (marker-buffer
       (cadr (navi-get-twin-buffer-markers)))
    (car
     (split-string
      (symbol-name major-mode)
      "-mode" 'OMIT-NULLS))))

(defun navi-show-keywords (key)
  "Show matches of occur-search with KEY.
Language is derived from major-mode."
  (let ((language (navi-get-language-name)))
    (navi-revert-function
     (navi-get-regexp language
                      (navi-map-keyboard-to-key language key)))))

(defun navi-show-headers-and-keywords (level key &optional args)
  "Show headers up-to level LEVEL and matches of occur-search with KEY.
Language is derived from major-mode."
  (let* ((language (navi-get-language-name))
         (rgxp
          (navi-make-regexp-alternatives
           (if args
               (navi-calc-headline-regexp level 'NO-PARENT-LEVELS)
             (navi-calc-headline-regexp level))
           (navi-get-regexp language
                            (navi-map-keyboard-to-key language key)))))
    (navi-revert-function rgxp)))

(defun navi-clean-up ()
  "Clean up `navi' plist and left-over markers after killing navi-buffer."
  (setq navi-revert-arguments nil)
  (setq navi-regexp-quoted-line-at-point nil)
  (mapc
   (lambda (marker) (set-marker marker nil))
   (navi-get-twin-buffer-markers)))

;; (add-to-list 'occur-hook 'navi-rename-buffer)

;; ** Commands

(defun navi-goto-occurrence-other-window ()
  "Moves navi-buffer marker to point before switching buffers."
  (interactive)
  (move-marker
   (car (navi-get-twin-buffer-markers)) (point))
  (navi-set-regexp-quoted-line-at-point)
  (occur-mode-goto-occurrence-other-window))

;; Convenience function copied from whom ??
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun navi-search-and-switch ()
  "Call `occur' and immediatley switch to `*Navi:original-buffer-name*' buffer"
  (interactive)
  (let ((buf-markers (navi-get-twin-buffer-markers)))
    (if (and
         buf-markers 
         (buffer-live-p (marker-buffer (car buf-markers)))
         (buffer-live-p (marker-buffer (cadr buf-markers))))
        (navi-switch-to-twin-buffer)
      (let ((1st-level-headers
             ;; (regexp-quote
             ;;  (outshine-calc-outline-string-at-level 1))))
             (regexp-quote
              (car (rassoc 1 outline-promotion-headings)))))
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
      (make-variable-buffer-local 'kill-buffer-hook)
      (add-to-list 'kill-buffer-hook 'navi-clean-up)))

(defun navi-quit-and-switch ()
  "Quit navi-buffer and immediatley switch back to original-buffer"
  (interactive)
  (navi-goto-occurrence-other-window)
  (kill-buffer (marker-buffer (cadr (navi-get-twin-buffer-markers))))
  (navi-clean-up))

(defun navi-switch-to-twin-buffer ()
  "Switch to associated twin-buffer of current buffer or do nothing."
  (interactive)
  (let* ((marker-list (navi-get-twin-buffer-markers))
         (self-marker (car marker-list))
         (twin-marker (cadr marker-list)))
    (and marker-list
         (buffer-live-p (marker-buffer self-marker))
         (buffer-live-p (marker-buffer twin-marker))
         (move-marker self-marker (point) (marker-buffer self-marker))
         (if (eq major-mode 'navi-mode)
             (navi-set-regexp-quoted-line-at-point) t)
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
    (apply 'navi-1 (append navi-revert-arguments (list (buffer-name))))
    ;; FIXME redundant with navi-1 instead of occur-1?
    (unless
        (eq major-mode 'navi-mode) (navi-mode))
    (goto-char 
      (navi-search-less-or-equal-line-number))))

;; this command executes all user-defined occur-searches
(defun navi-generic-command (key prefix)
  "One size fits all (user-defined header and keyword searches)."
  (interactive (list last-command-event current-prefix-arg))
  (let ((keystrg (format "%c" key))
        (numval-prefix (and prefix (prefix-numeric-value prefix))))
    (if prefix
        (cond
         ((memq numval-prefix (number-sequence 1 8))
          (navi-show-headers-and-keywords numval-prefix keystrg))
         ((and
           (not (memq numval-prefix (number-sequence 1 8))
                (not (memq key (number-sequence 49 56)))))
          (navi-show-headers keystrg prefix))
         (t nil))
      (cond
       ((memq key (number-sequence 49 56))
        (navi-show-headers (string-to-int (format "%c" key))))
       ((memq key (number-sequence 57 126))
        (navi-show-keywords keystrg))
       (t nil)))))

(defun navi-mark-subtree-and-switch ()
  "Mark subtree at point in original-buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (outline-mark-subtree)
      (message "Only subtrees may be marked via navi-mode")))
  ;; (navi-switch-to-twin-buffer)) ; FIXME deactivates region - workaround?

(defun navi-copy-subtree-to-register-s ()
  "Copy subtree at point in original-buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (progn
        (outline-mark-subtree)
        (and
         (use-region-p)
         (copy-to-register ?s (region-beginning) (region-end)))
        (deactivate-mark))
    (message "Only subtrees may be copied via navi-mode"))
  (navi-switch-to-twin-buffer))

(defun navi-narrow-to-subtree ()
  "Narrow original buffer to subtree at point."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (progn
        (outline-mark-subtree)
        (and
         (use-region-p)
         (narrow-to-region (region-beginning) (region-end)))
        (deactivate-mark))
    (message "Navi-mode can only narrow to subtrees"))
  (setq navi-regexp-quoted-line-before-narrowing
        navi-regexp-quoted-line-at-point)
  (navi-switch-to-twin-buffer))

(defun navi-widen ()
  "Widen original buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (widen)
  (navi-switch-to-twin-buffer)
  (setq navi-regexp-quoted-line-at-point
        navi-regexp-quoted-line-before-narrowing)
  (goto-char
   (navi-search-less-or-equal-line-number)))

(defun navi-kill-subtree ()
  "Kill subtree at point in original-buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (progn
        (outline-mark-subtree)
        (and
         (use-region-p)
         (and (y-or-n-p
               "Really kill this subtree in the original-buffer ")
              (copy-to-register ?s (region-beginning) (region-end) 'DELETE-FLAG)))
        (deactivate-mark))
    (message "Only subtrees may be killed via navi-mode"))
  (navi-switch-to-twin-buffer))

(defun navi-undo ()
  "Undo last (undoable) action in original-buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (undo)
  (navi-switch-to-twin-buffer))

(defun navi-yank-subtree-from-register-s ()
  "Yank in original-buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (and
       (outline-on-heading-p)
       (get-register ?s))
      (progn
        (newline)
        (forward-line -1)
        (insert-register ?s))
    (message "Not on subtree-heading or no subtree to yank."))
  (navi-switch-to-twin-buffer))

(defun navi-query-replace ()
  "Call `query-replace' interactively on subtree at point."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (progn
        (outline-mark-subtree)
        (and
         (use-region-p)
         (call-interactively 'query-replace))
        (deactivate-mark))
    (message "Navi-mode can perform query-replace only on subtrees"))
  (navi-switch-to-twin-buffer))

(defun navi-isearch ()
  "Call `isearch' interactively on subtree at point."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (save-restriction
        (outline-mark-subtree)
        (and
         (use-region-p)
         (narrow-to-region (region-beginning) (region-end)))
        (deactivate-mark)
        (isearch-mode t nil nil t nil))
    (message "Navi-mode can perform isearches only on subtrees"))
  (navi-switch-to-twin-buffer))

(defun navi-demote-subtree ()
  "Demote subtree at point."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (outline-demote 1)
    (message "Navi-mode can only demote subtrees"))
  (navi-switch-to-twin-buffer))

(defun navi-promote-subtree ()
  "Promote subtree at point."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (outline-promote 1)
    (message "Navi-mode can only promote subtrees"))
  (navi-switch-to-twin-buffer))

(defun navi-move-up-subtree ()
  "Move subtree at point one position up."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (outline-move-subtree-up 1)
    (message "Navi-mode can only move subtrees"))
  (navi-switch-to-twin-buffer))

(defun navi-move-down-subtree ()
  "Move subtree at point one position down."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (outline-move-subtree-down 1)
    (message "Navi-mode can only move subtrees"))
  (navi-switch-to-twin-buffer))

(defun navi-show-help ()
  "Show navi-keybindings for major-mode of original-buffer."
  (interactive)
  (let ((mappings
         (navi-get-language-alist (navi-get-language-name) 'MAPPINGS))
        (navi-buf-marker (car (navi-get-twin-buffer-markers))))
    (switch-to-buffer-other-window
     (get-buffer-create
      (concat "*Navi:" (navi-get-language-name) ":HELP")))
    (save-restriction
      (widen)
      (when (string-equal
             (buffer-substring-no-properties (point-min) (point-max)) "")
        (insert "[KEY] : [SEARCH]\n")
        (forward-line -1)
        (underline-line-with ?=)
        (forward-line 2)
        (mapc
         (lambda (association)
           (insert
            (format "\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\t%s : %s\n"
                    (cdr association)
                    (car
                     (split-string
                      (symbol-name (car association))
                      ":" 'OMIT-NULLS)))))
         mappings))
      (goto-char (point-min))
      (view-buffer (current-buffer)))))

(defun navi-cycle-subtree ()
  "Cycle the visibility state of subtree at point in the original-buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (outline-cycle 1)
    (message "Not on subtree - can't cycle subtree visibility state."))
  (navi-switch-to-twin-buffer))

(defun navi-cycle-buffer ()
  "Cycle the visibility state of the original-buffer."
  (interactive)
  (navi-goto-occurrence-other-window)
  (outline-cycle '(4))
  (navi-switch-to-twin-buffer))

(defun navi-edit-as-org (&optional args)
  "Edit subtree at point (or whole buffer if ARGS are given) with `outorg'.
Editing takes place in a separate temporary Org-mode edit-buffer."
  (interactive "P")
  (navi-goto-occurrence-other-window)
  (if (outline-on-heading-p)
      (if args
          (outorg-edit-as-org args)
        (outorg-edit-as-org))
    (message "Only subtrees (or the whole buffer) may be edited via navi-mode"))
  (navi-switch-to-twin-buffer))

;; * Keybindings

;; key-bindings for user-defined occur-searches
;; see `navi-key-mappings' and `navi-keywords'.
;; reserved keys to be removed from num-seq:
;; | ?\s |  32 |
;; | ?\+ |  43 |
;; | ?\- |  45 |
;; | ?\^ |  60 |
;; | ?\< |  94 |
;; | ?c  |  99 |
;; | ?d  | 100 |
;; | ?e  | 101 |
;; | ?g  | 103 |
;; | ?h  | 104 |
;; | ?k  | 107 |
;; | ?l  | 108 |
;; | ?m  | 109 |
;; | ?n  | 110 |
;; | ?o  | 111 |
;; | ?p  | 112 |
;; | ?q  | 113 |
;; | ?r  | 114 |
;; | ?s  | 115 |
;; | ?u  | 117 |
;; | ?w  | 119 |
;; | ?y  | 121 |
;; | ?\d | 127 |
(mapc #'(lambda (key)
          (define-key navi-mode-map (format "%c" key)
            'navi-generic-command))
      (let ((num-seq (number-sequence 32 127))) ; all ascii printing chars
        (mapc #'(lambda (num)
                  (setq num-seq (delq num num-seq))) 
              '(32 43 45 60 94 99 100 101 103 104 107 108 109 110 111 112 113
                   114 115 117 119 121 127))    ; reserved keys defined elsewhere
        num-seq))

;; TODO navi-edit-mode "e"
;; keybindings for basic navi-mode (or occur-mode) commands
(global-set-key (kbd "M-s n") 'navi-search-and-switch)
(global-set-key (kbd "M-s s") 'navi-switch-to-twin-buffer)
(global-set-key (kbd "M-s M-s") 'navi-switch-to-twin-buffer)
(define-key navi-mode-map (kbd "s") 'navi-switch-to-twin-buffer)
(define-key navi-mode-map (kbd "d") 'occur-mode-display-occurrence)
(define-key navi-mode-map (kbd "o") 'navi-goto-occurrence-other-window)
(define-key navi-mode-map (kbd "n") 'occur-next)
(define-key navi-mode-map (kbd "p") 'occur-prev)
(define-key navi-mode-map (kbd "SPC") 'occur-next)
(define-key navi-mode-map (kbd "DEL") 'occur-prev)
(define-key navi-mode-map (kbd "TAB") 'navi-cycle-subtree)
(define-key navi-mode-map (kbd "<backtab>") 'navi-cycle-buffer)
(define-key navi-mode-map (kbd "m") 'navi-mark-subtree-and-switch)
(define-key navi-mode-map (kbd "c") 'navi-copy-subtree-to-register-s)
(define-key navi-mode-map (kbd "r") 'navi-narrow-to-subtree)
(define-key navi-mode-map (kbd "w") 'navi-widen)
(define-key navi-mode-map (kbd "l") 'navi-query-replace)
(define-key navi-mode-map (kbd "i") 'navi-isearch)
(define-key navi-mode-map (kbd "k") 'navi-kill-subtree)
(define-key navi-mode-map (kbd "y") 'navi-yank-subtree-from-register-s)
(define-key navi-mode-map (kbd "u") 'navi-undo)
(define-key navi-mode-map (kbd "e") 'navi-edit-as-org)
(define-key navi-mode-map (kbd "h") 'navi-show-help)
(define-key navi-mode-map (kbd "+") 'navi-demote-subtree)
(define-key navi-mode-map (kbd "-") 'navi-promote-subtree)
(define-key navi-mode-map (kbd "^") 'navi-move-up-subtree)
(define-key navi-mode-map (kbd "<") 'navi-move-down-subtree)
(define-key navi-mode-map (kbd "g") 'navi-revert-function)
(define-key navi-mode-map (kbd "q") 'navi-quit-and-switch)
(define-key isearch-mode-map (kbd "M-s i") 'isearch-occur)

;; * Run Hooks and Provide

;; (add-to-list 'navi-mode-hook 'navi-mode-hook-function)
;; (run-mode-hooks)

(provide 'navi-mode)

;; navi-mode.el ends here
