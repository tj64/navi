;; * navi.el --- extensions for easy navigation via occur-buffers

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

;; * Variables
;; ** Consts
;; ** Vars

(defvar navi-version 0.9
  "Version number of `navi.el' library")

(defvar navi nil
  " list that holds pairs of buffer-marker names.
Keys are names of markers that point to original-buffers, values
are the names of associated navi-buffers")

;; ** Hooks

(defvar navi-hook nil
  "Hook run after `navi' is loaded")

;; ** Fonts
;; ** Customs
;; *** Custom Groups 
;; *** Custom Vars
;; * Defuns
;; ** Functions

;; (defun navi-hook-function ()
;;   "Function to be run after `navi' is loaded."
;;   (add-to-list 'occur-hook 'occur-rename-buffer))

(defun navi-buffer-key (&optional buf)
  "Return the (current) buffer-name or string BUF as interned keyword-symbol"
  (let ((buf-name
         (file-name-sans-extension
          (car (split-string (or buf (buffer-name)) "[*]" 'OMIT-NULLS)))))
  (intern (concat ":" buf-name))))

(defun navi-marker-name (&optional buf)
  "Return marker-name by expansion of (current) buffer-name or string BUF."
  (let ((buf-name
         (file-name-sans-extension
          (car (split-string (or buf (buffer-name)) "[*]" 'OMIT-NULLS)))))
  (concat buf-name "-marker")))

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
           (navi-buffer-key)
           (set
            (intern
             (navi-marker-name
              (cadr (split-string (buffer-name) "[*:]" 'OMIT-NULLS))))
            (point-marker))))))

(add-to-list 'occur-hook 'navi-rename-buffer)

;; ** Commands

;; Convenience function copied from whom ??
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun navi-search-and-switch ()
  "Call `occur' and immediatley switch to `*Navi*' (modified `*Occur*')
buffer"
  (interactive)
  (let ((1st-level-headers
         (regexp-quote
          (outshine-calc-outline-string-at-level 1))))
    (put 'navi (navi-buffer-key (buffer-name))
         (set (intern (navi-marker-name)) (point-marker)))
    (occur 1st-level-headers)))


(defun navi-quit-and-switch ()
  "Quit `*Navi*' and immediatley switch back to original buffer"
  (interactive)
  (quit-window)
  (switch-to-buffer
   (marker-buffer original-buffer-marker))
  (goto-char
   (marker-position original-buffer-marker)) ; necessary?
  (set-marker navi-buffer-marker nil)
  (set-marker original-buffer-marker nil))


(defun navi-twin-marker ()
  "Returns the marker pointing to the twin of the current-buffer or nil.
If the current buffer is a navi-buffer, the marker that points to
the associated original-buffer is returned. If it is an
original-buffer with existing navi-buffer, a marker pointing to
the navi-buffer is returned. Otherwise, nil is returned."
  (interactive)
    (get navi (navi-buffer-key)))


(defun navi-twin-buffer ()
  "Returns the twin of the current-buffer or nil.
If the current buffer is a navi-buffer, the associated
original-buffer is returned. If it is an original-buffer with
existing navi-buffer, the associated navi-buffer is
returned. Otherwise, nil is returned."
  (interactive)
    (marker-buffer (navi-twin-marker)))


;; * Keybindings

;; Occur mode: new/better keybindings
(global-set-key (kbd "M-s n") 'navi-search-and-switch) 
(define-key occur-mode-map (kbd "d") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(define-key occur-mode-map (kbd "q") 'navi-quit-and-switch)
(define-key isearch-mode-map (kbd "M-s i") 'isearch-occur)

;; * Run Hooks and Provide

(run-hooks 'navi-hook)

(provide 'navi)

;; navi.el ends here

