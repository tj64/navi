- [navi-mode.el &#x2014; major-mode for easy buffer-navigation](#navi-mode.el-&#x2014;-major-mode-for-easy-buffer-navigation)
  - [MetaData](#metadata)
  - [Commentary](#commentary)
    - [About navi-mode](#about-navi-mode)
    - [Usage](#usage)
    - [Installation](#installation)
    - [Emacs Version](#emacs-version)
  - [ChangeLog](#changelog)



# navi-mode.el &#x2014; major-mode for easy buffer-navigation

Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
Version: 1.0
URL: <https://github.com/tj64/outshine>

## MetaData

    copyright: Thorsten Jolitz
    
    copyright-years: 2013+
    
    version: 1.0
    
    licence: GPL 2 or later (free software)
    
    licence-url: http://www.gnu.org/licenses/
    
    part-of-emacs: no
    
    author: Thorsten Jolitz
    
    author_email: tjolitz AT gmail DOT com
    
    git-repo: https://github.com/tj64/navi.git
    
    git-clone: git://github.com/tj64/navi.git
    
    inspiration: occur-mode org-mode
    
    keywords: emacs keymaps unbound

## Commentary

### About navi-mode

[NOTE: For the sake of adding this library to MELPA, headlines had to
be converted back from 'Org-mode style' to 'oldschool', and a few
extra lines of required information had to be added on top of the
MetaData section - just to comply with the required file
formatting. All outshine, outorg and navi-mode functionality still
works with this file. See my [iOrg](https://github.com/tj64/iorg) repository for examples of
Emacs-Lisp and PicoLisp files structured 'the outshine way'.]

This file implements extensions for occur-mode. You can think of a
navi-buffer as a kind of 'remote-control' for an (adecuately)
outline-structured original-buffer. It enables quick navigation and
basic structure editing in the original-buffer without (necessarily)
leaving the navi-buffer. When switching to the original-buffer and
coming back after some modifications, the navi-buffer is always
reverted (thus up-to-date).

Besides the fundamental outline-heading-searches (8 outline-levels)
and the 5 basic keyword-searches (:FUN, :VAR, :DB, :OBJ and :ALL), all
languages can have their own set of searches and keybindings (see
\`navi-key-mappings' and \`navi-keywords'). Heading-searches and
keyword-searches can be combined, offering a vast amount of possible
'views' on the original-buffer.

### Usage

For \`navi-mode' to work, the original-buffer must be outline-structured
'the outshine way', i.e. with the headlines being proper Org-mode
headlines, marked and outcommented with \`comment-region'. As an example, to
generate a 3rd level outshine-headline in an Emacs Lisp file, write down

    *** Third Level Header

mark the header line, and apply \`comment-region' on it:

    ;;;;; Third Level Header

In a LaTeX file, an adecuate header will look like this:

    % *** Third Level Header

and in a PicoLisp file like this (always depending of the major-mode specific
values of \`comment-start', \`comment-end', \`comment-add' and
\`comment-padding'):

    ## *** Third Level Header

The second assumption is that \`outline-minor-mode' is activated in the
original-buffer and \`outshine.el' loaded like described in its
installation instructions, i.e.

    (require 'outshine)
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)

When these pre-conditions are fullfilled (\`outorg.el' must be loaded
too), you can use 'M-s n' (\`navi-search-and-switch') to open a
navi-buffer and immediately switch to it. The new navi-buffer will
show the first-level headings of the original-buffer, with point at
the first entry.

You can then:

-   Show headlines (up-to) different levels:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">key</th>
<th scope="col" class="left">command</th>
<th scope="col" class="left">function-name</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">1 &#x2026; 8</td>
<td class="left">show levels 1 to 8</td>
<td class="left">navi-generic-command</td>
</tr>
</tbody>
</table>
-   Navigate up and down in the search results shown in the navi-buffer:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">key</th>
<th scope="col" class="left">command</th>
<th scope="col" class="left">function-name</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">p</td>
<td class="left">previous</td>
<td class="left">occur-prev</td>
</tr>


<tr>
<td class="left">n</td>
<td class="left">next</td>
<td class="left">occur-next</td>
</tr>


<tr>
<td class="left">DEL</td>
<td class="left">down page</td>
<td class="left">scroll-down-command</td>
</tr>


<tr>
<td class="left">SPC</td>
<td class="left">up page</td>
<td class="left">scroll-up-command</td>
</tr>
</tbody>
</table>
-   Revert the navi-buffer (seldom necessary), show help for the
    user-defined keyword-searches, and quit the navi-buffer and switch-back
    to the original-buffer:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">key</th>
<th scope="col" class="left">command</th>
<th scope="col" class="left">function-name</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">g</td>
<td class="left">revert buffer</td>
<td class="left">navi-revert-function</td>
</tr>


<tr>
<td class="left">h</td>
<td class="left">show help</td>
<td class="left">navi-show-help</td>
</tr>


<tr>
<td class="left">q</td>
<td class="left">quit navi-mode and switch</td>
<td class="left">navi-quit-and-switch</td>
</tr>
</tbody>
</table>
-   Switch to the original-buffer and back to the navi-buffer, display an
    occurence in the original-buffer or go to the occurence:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">key</th>
<th scope="col" class="left">command</th>
<th scope="col" class="left">function-name</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">M-s n</td>
<td class="left">launch navi-buffer</td>
<td class="left">navi-search-and-switch</td>
</tr>


<tr>
<td class="left">M-s s</td>
<td class="left">switch to other buffer</td>
<td class="left">navi-switch-to-twin-buffer</td>
</tr>


<tr>
<td class="left">M-s M-s</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">s</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">d</td>
<td class="left">display occurrence</td>
<td class="left">occur-mode-display-occurrence</td>
</tr>


<tr>
<td class="left">o</td>
<td class="left">goto occurrence</td>
<td class="left">navi-goto-occurrence-other-window</td>
</tr>
</tbody>
</table>
-   Structure editing on subtrees and visibility cycling

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">key</th>
<th scope="col" class="left">command</th>
<th scope="col" class="left">function-name</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">TAB</td>
<td class="left">cycle subtrees</td>
<td class="left">navi-cycle-subtree</td>
</tr>


<tr>
<td class="left"><backtab></td>
<td class="left">cycle buffer</td>
<td class="left">navi-cycle-buffer</td>
</tr>


<tr>
<td class="left">+</td>
<td class="left">Demote Subtree</td>
<td class="left">navi-demote-subtree</td>
</tr>


<tr>
<td class="left">-</td>
<td class="left">promote subtree</td>
<td class="left">navi-promote-subtree</td>
</tr>


<tr>
<td class="left">^</td>
<td class="left">move up subtree (same level)</td>
<td class="left">navi-move-up-subtree</td>
</tr>


<tr>
<td class="left"><</td>
<td class="left">move down subtree (same level)</td>
<td class="left">navi-move-down-subtree</td>
</tr>
</tbody>
</table>
-   Miscancellous actions on subtrees

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">key</th>
<th scope="col" class="left">command</th>
<th scope="col" class="left">function-name</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">m</td>
<td class="left">mark thing at point</td>
<td class="left">navi-mark-thing-at-point-and-switch</td>
</tr>


<tr>
<td class="left">c</td>
<td class="left">copy thing at point</td>
<td class="left">navi-copy-thing-at-point-to-register-s</td>
</tr>


<tr>
<td class="left">k</td>
<td class="left">kill thing at point</td>
<td class="left">navi-kill-thing-at-point</td>
</tr>


<tr>
<td class="left">y</td>
<td class="left">yank killed/copied thing</td>
<td class="left">navi-yank-thing-at-point-from-register-s</td>
</tr>


<tr>
<td class="left">u</td>
<td class="left">undo last change</td>
<td class="left">navi-undo</td>
</tr>


<tr>
<td class="left">r</td>
<td class="left">narrow to thing at point</td>
<td class="left">navi-narrow-to-thing-at-point</td>
</tr>


<tr>
<td class="left">w</td>
<td class="left">widen</td>
<td class="left">navi-widen</td>
</tr>


<tr>
<td class="left">l</td>
<td class="left">query-replace</td>
<td class="left">navi-query-replace</td>
</tr>


<tr>
<td class="left">i</td>
<td class="left">isearch</td>
<td class="left">navi-isearch</td>
</tr>


<tr>
<td class="left">e</td>
<td class="left">edit as org (outorg)</td>
<td class="left">navi-edit-as-org</td>
</tr>


<tr>
<td class="left">.</td>
<td class="left">call fun on thing at point</td>
<td class="left">navi-act-on-thing-at-point</td>
</tr>
</tbody>
</table>
-   Furthermore, there are five (semantically) predefined keyword-searches:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">key</th>
<th scope="col" class="left">keyword-symbol</th>
<th scope="col" class="left">searches for</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">f</td>
<td class="left">:FUN</td>
<td class="left">functions, macros etc.</td>
</tr>


<tr>
<td class="left">v</td>
<td class="left">:VAR</td>
<td class="left">vars, consts, customs etc.</td>
</tr>


<tr>
<td class="left">x</td>
<td class="left">:OBJ</td>
<td class="left">OOP (classes, methods etc)</td>
</tr>


<tr>
<td class="left">b</td>
<td class="left">:DB</td>
<td class="left">DB (store and select)</td>
</tr>


<tr>
<td class="left">a</td>
<td class="left">:ALL</td>
<td class="left">all</td>
</tr>
</tbody>
</table>
-   And (potentially) many more user-defined keyword-searches

(example Emacs Lisp):

    [KEY] : [SEARCH]
    ================
                            a : ALL
                            f : FUN
                            v : VAR
                            x : OBJ
                            b : DB
                            F : defun
                            V : defvar
                            C : defconst
                            G : defgroup
                            U : defcustom
                            A : defadvice
                            W : defalias
                            M : defmarcro
                            D : defface
                            S : defstruct
                            B : defsubst
                            L : defclass
                            I : define
                            J : declare
                            K : global-set-key
                            T : add-to-list
                            Q : setq
                            H : add-hook
                            O : hook
                            X : lambda
                            Z : ert
                            R : require
-   Headline-searches and keyword-searches can be combined, e.g.

    C-2 f

in an Emacs Lisp (outshine-)buffer shows all headlines up-to level 2 as
well as all function, macro and advice definitions in the original-buffer,

    C-5 a

shows all headlines up-to level 5 as well as all functions, variables,
classes, methods, objects, and database-related definitions. The exact
meaning of the standard keyword-searches 'f' and 'a' must be defined with a
regexp in the customizable variable \`navi-keywords' (just like the
user-defined keyword-searches).

When exploring a (potentially big) original buffer via navi-mode, a common
usage pattern is the following:

1.  type e.g '2'  and go to the relevant headline
2.  type 'r' and e.g. '3' in sequence to narrow buffers to the subtree at
    point and show one deeper level of headlines
3.  do your thing in the narrowed subtree
4.  type e.g. '2' and 'w' to first reduce the headline levels shown and
    then widen the buffers again.

### Installation

Download (or clone the github-repos of) the three required libraries

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">\`navi-mode.el'</td>
<td class="left">(<https://github.com/tj64/navi>)</td>
</tr>


<tr>
<td class="left">\`outshine.el'</td>
<td class="left">(<https://github.com/tj64/outshine>)</td>
</tr>


<tr>
<td class="left">\`outorg.el'</td>
<td class="left">(<https://github.com/tj64/outorg>)</td>
</tr>
</tbody>
</table>

and put them in a place where Emacs can find them (on the Emacs
'load-path'). Follow the installation instructions in \`outshine.el' and
\`outorg.el'.

Install \`navi-mode.el' by adding

    (require 'navi-mode)

to your .emacs file.

### Emacs Version

\`navi-mode.el' works with [GNU Emacs 24.2.1 (x86\_64-unknown-linux-gnu,
GTK+ Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing
with older versions or other types of Emacs have been made (yet).

## ChangeLog

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">date</th>
<th scope="col" class="left">author(s)</th>
<th scope="col" class="right">version</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2013-05-03 Fr&gt;</span></span></td>
<td class="left">Thorsten Jolitz</td>
<td class="right">1.0</td>
</tr>


<tr>
<td class="left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2013-03-11 Mo&gt;</span></span></td>
<td class="left">Thorsten Jolitz</td>
<td class="right">0.9</td>
</tr>
</tbody>
</table>
