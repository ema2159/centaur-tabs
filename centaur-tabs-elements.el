;;; centaur-tabs-elements.el --- centaur-tabs visual components and customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Emmanuel Bustos
;; Package-Requires: ((emacs "24.4") (powerline "2.4")  (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;; This file contains the visual components of centaur-tabs

;;; Code:
;;
;;; Requires
;;
(require 'color)
(require 'powerline)
;;; Faces
;;
(defface centaur-tabs-default
  '((t
     (:background "black" :foreground "black")))
  "Default face used in the tab bar."
  :group 'centaur-tabs)

(defface centaur-tabs-unselected
  '((t
     (:background "#3D3C3D" :foreground "grey50")))
  "Face used for unselected tabs."
  :group 'centaur-tabs)

(defface centaur-tabs-selected
  '((t (:background "#31343E" :foreground "white")))
  "Face used for the selected tab."
  :group 'centaur-tabs)

(defface centaur-tabs-unselected-modified
  '((t
     (:background "#3D3C3D" :foreground "grey50")))
  "Face used for unselected-modified tabs."
  :group 'centaur-tabs)

(defface centaur-tabs-selected-modified
  '((t (:background "#31343E" :foreground "white")))
  "Face used for the selected-modified tab."
  :group 'centaur-tabs)

(defface centaur-tabs-close-unselected
  '((t
     (:inherit centaur-tabs-unselected)))
  "Face used for unselected close button."
  :group 'centaur-tabs)

(defface centaur-tabs-close-selected
  '((t (:inherit centaur-tabs-selected)))
  "Face used for selected close button."
  :group 'centaur-tabs)

(defface centaur-tabs-close-mouse-face
  '((t (:inherit underline)))
  "Face used for close button when hovered with the mouse."
  :group 'centaur-tabs)

(defface centaur-tabs-modified-marker-selected
  `((t (:inherit centaur-tabs-selected)))
  "Face used for selected modified marker."
  :group 'centaur-tabs)

(defface centaur-tabs-modified-marker-unselected
  `((t (:inherit centaur-tabs-unselected)))
  "Face used for unselected modified marker."
  :group 'centaur-tabs)

(defface centaur-tabs-active-bar-face
  '((t (:background "cyan")))
  "Face used for selected tab bar."
  :group 'centaur-tabs)

;;; Tabs' display line
;;
(defvar centaur-tabs-display-line
  (if (boundp 'tab-line-format)
      'tab-line
    'header-line))

(defvar centaur-tabs-display-line-format
  (if (boundp 'tab-line-format)
      'tab-line-format
    'header-line-format))

;;; Tabs' characteristics
;;
(defcustom centaur-tabs-style "bar"
  "The style of tab."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-label-fixed-length 0
  "Fixed length of label.  Set to 0 if dynamic."
  :group 'centaur-tabs
  :type 'int)

(defcustom centaur-tabs-background-color (face-background 'centaur-tabs-default)
  "*Background color of the tab bar.
By default, use the background color specified for the
`centaur-tabs-default' face (or inherited from another face), or the
background color of the `default' face otherwise."
  :group 'centaur-tabs
  :type 'face)

(defcustom centaur-tabs-height 22
  "The height of tab."
  :group 'centaur-tabs
  :type 'int)

(defcustom centaur-tabs-bar-height (+ 8 centaur-tabs-height)
  "The height of bar."
  :group 'centaur-tabs
  :type 'int)

(defcustom centaur-tabs-mouse-pointer 'hand
  "Cursor to display when hovering the tabs.
Default is 'hand.  The following scopes are possible:
- arrow
- hand
- vdrag
- hdrag
- modeline
- hourglass"
  :group 'centaur-tabs
  :type 'variable)

;;; Icons
;;
(defcustom centaur-tabs-set-icons nil
  "When non nil, display an icon from all-the-icons alongside the tab name."
  :group 'centaur-tabs
  :type 'boolean)

(defvar centaur-tabs-icon-scale-factor
  1.0
  "The base scale factor for the `height' face property of tab icons.")

(defvar centaur-tabs-icon-v-adjust
  0.01
  "The vertical adjust for tab icons.")

(defcustom centaur-tabs-gray-out-icons nil
  "When non nil, enable gray icons for unselected buffer."
  :group 'centaur-tabs
  :type '(choice :tag "Gray out icons for unselected..."
		 (const :tag "Buffer" buffer)))

(defcustom centaur-tabs-plain-icons nil
  "When non nil, tab icons' color will be the same as tabs' foreground color."
  :group 'centaur-tabs
  :type 'boolean)

(defun centaur-tabs-icon (tab face selected)
  "Generate all-the-icons icon for TAB using FACE's background.
If icon gray out option enabled, gray out icon if not SELECTED."
  (if (featurep 'all-the-icons)
      (with-current-buffer (car tab)
	(let* ((icon
		(if (and (buffer-file-name)
			 (all-the-icons-auto-mode-match?))
		    (all-the-icons-icon-for-file
		     (file-name-nondirectory (buffer-file-name))
		     :v-adjust centaur-tabs-icon-v-adjust
		     :height centaur-tabs-icon-scale-factor)
		  (all-the-icons-icon-for-mode
		   major-mode
		   :v-adjust centaur-tabs-icon-v-adjust
		   :height centaur-tabs-icon-scale-factor)))
	       (background (face-background face))
	       (inactive (cond ((and (not selected)
				     (eq centaur-tabs-gray-out-icons 'buffer))
				(face-foreground 'mode-line-inactive))
			       (centaur-tabs-plain-icons
				(face-foreground 'centaur-tabs-selected))
			       (t 'unspecified)))
	       (underline (and (eq centaur-tabs-set-bar 'under)
			       (face-attribute face :underline)))
	       (overline (and (eq centaur-tabs-set-bar 'over)
			      (face-attribute face :overline))))
	  (if (stringp icon)
	      (progn
		(propertize icon 'face `(:inherit ,(get-text-property 0 'face icon)
						  :foreground ,inactive
						  :background ,background
						  :underline ,underline
						  :overline ,overline)))
	    "")))
    ""))

;;; Close buttons, modified marker and edges' margins
;;
(defcustom centaur-tabs-set-close-button t
  "When non nil, display a clickable close button on the right side of the tabs."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-set-left-close-button nil
  "When non nil, display a clickable close button on the left side of the tabs."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-close-button (make-string 1 #x00D7)
  "Display appearance of the close buttons, if enabled."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-set-modified-marker nil
  "When non nil, display a marker when the buffer is modified."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-modified-marker (make-string 1 #x23FA)
  "Display appearance of the modified marker, if enabled."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-left-edge-margin " "
  "Text to display at the left edge of the tabs, or nil for no added margin."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-right-edge-margin " "
  "Text to display at the right edge of the tabs, or nil for no added margin."
  :group 'centaur-tabs
  :type 'string)

;;; Selected tab bar
;;
(defcustom centaur-tabs-set-bar nil
  "When non nil, display a bar to show the currently selected tab.
There are three options:
- 'left: displays the bar at the left of the currently selected tab.
- 'under: displays the bar under the currently selected tab.
- 'over: displays the bar over the currently selected tab."
  :group 'centaur-tabs
  :type '(choice :tag "Display bar at..."
		 (const :tag "Put bar on the left" left)
		 (const :tag "Put bar as an underline" under)
		 (const :tag "Put bar as an overline" over)))

(defun centaur-tabs--make-xpm (face width height)
  "Create an XPM bitmap via FACE WIDTH and HEIGHT.
Taken from `doom-modeline'."
  (when (and (display-graphic-p)
	     (image-type-available-p 'xpm))
    (propertize
     " " 'display
     (let ((data (make-list height (make-list width 1)))
	   (color (or (face-background face nil t) "None")))
       (ignore-errors
	 (create-image
	  (concat
	   (format
	    "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
	    (length (car data)) (length data) color color)
	   (apply #'concat
		  (cl-loop with idx = 0
			   with len = (length data)
			   for dl in data
			   do (cl-incf idx)
			   collect
			   (concat
			    "\""
			    (cl-loop for d in dl
				     if (= d 0) collect (string-to-char " ")
				     else collect (string-to-char "."))
			    (if (eq idx len) "\"};" "\",\n")))))
	  'xpm t :ascent 'center))))))

(defvar centaur-tabs-active-bar
  (centaur-tabs--make-xpm 'centaur-tabs-active-bar-face
			  2
			  centaur-tabs-bar-height))

;;; Navigation buttons
;;
(defcustom centaur-tabs-show-navigation-buttons nil
  "When non-nil, show the buttons for backward/forward tabs."
  :group 'centaur-tabs
  :type 'boolean)

(defcustom centaur-tabs-down-tab-text " ▾ "
  "Text icon to show in the down button tab."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-backward-tab-text " ⏴ "
  "Text icon to show in the backward button tab."
  :group 'centaur-tabs
  :type 'string)

(defcustom centaur-tabs-forward-tab-text " ⏵ "
  "Text icon to show in the forward button tab."
  :group 'centaur-tabs
  :type 'string)

;;; Separators
;;
(defvar centaur-tabs-style-left nil)
(defvar centaur-tabs-style-right nil)

(defvar ns-use-srgb-colorspace)

(defvar centaur-tabs-image-apple-rgb
  (and (eq (window-system) 'ns)
       ns-use-srgb-colorspace
       (< 11
	  (string-to-number
	   (and (string-match "darwin\\([0-9]+\\)" system-configuration)
		(match-string-no-properties 1 system-configuration)))))
  "Boolean variable to determine whether to use Apple RGB colorspace.
used to render images.

t on macOS 10.7+ and `ns-use-srgb-colorspace' is t, nil otherwise.

This variable is automatically set, there's no need to modify it.")

(defun centaur-tabs-separator-interpolate (color1 color2)
  "Interpolate between COLOR1 and COLOR2.

COLOR1 and COLOR2 must be supplied as hex strings with a leading #."
  (let* ((c1 (color-name-to-rgb color1))
	 (c2 (color-name-to-rgb color2))
	 (red (/ (+ (nth 0 c1) (nth 0 c2)) 2))
	 (green (/ (+ (nth 1 c1) (nth 1 c2)) 2))
	 (blue (/ (+ (nth 2 c1) (nth 2 c2)) 2)))
    (color-rgb-to-hex red green blue)))

(defun centaur-tabs-separator-color-xyz-to-apple-rgb (X Y Z)
  "Convert CIE X Y Z colors to Apple RGB color space."
  (let ((r (+ (* 3.2404542 X) (* -1.5371385 Y) (* -0.4985314 Z)))
	(g (+ (* -0.9692660 X) (* 1.8760108 Y) (* 0.0415560 Z)))
	(b (+ (* 0.0556434 X) (* -0.2040259 Y) (* 1.0572252 Z))))
    (list (expt r (/ 1.8)) (expt g (/ 1.8)) (expt b (/ 1.8)))))

(defun centaur-tabs-separator-color-srgb-to-apple-rgb (red green blue)
  "Convert RED GREEN BLUE colors from sRGB color space to Apple RGB.
RED, GREEN and BLUE should be between 0.0 and 1.0, inclusive."
  (apply #'centaur-tabs-separator-color-xyz-to-apple-rgb (color-srgb-to-xyz red green blue)))

(defun centaur-tabs-separator-hex-color (color)
  "Get the hexadecimal value of COLOR."
  (when color
    (let ((srgb-color (color-name-to-rgb color)))
      (if centaur-tabs-image-apple-rgb
	  (apply #'color-rgb-to-hex (apply #'centaur-tabs-separator-color-srgb-to-apple-rgb srgb-color))
	(apply #'color-rgb-to-hex srgb-color)))))

(defun centaur-tabs-separator-pattern (lst)
  "Turn LST into an infinite pattern."
  (when lst
    (let ((pattern (cl-copy-list lst)))
      (setcdr (last pattern) pattern))))

(defun centaur-tabs-separator-pattern-to-string (pattern)
  "Convert a PATTERN into a string that can be used in an XPM."
  (concat "\"" (mapconcat #'number-to-string pattern "") "\","))

(defun centaur-tabs-separator-reverse-pattern (pattern)
  "Reverse each line in PATTERN."
  (cl-mapcar 'reverse pattern))

(defun centaur-tabs-separator-row-pattern (fill total &optional fade)
  "Make a list that has FILL 0s out of TOTAL 1s with FADE 2s to the right of the fill."
  (unless fade
    (setq fade 0))
  (let ((fill (min fill total))
	(fade (min fade (max (- total fill) 0))))
    (append (make-list fill 0)
	    (make-list fade 2)
	    (make-list (- total fill fade) 1))))

(defun centaur-tabs-separator-pattern-bindings-body (patterns height-exp pattern-height-sym
							      second-pattern-height-sym)
  "Create let-var bindings and a function body from PATTERNS.
The `car' and `cdr' parts of the result can be passed to the
function `centaur-tabs-separator-wrap-defun' as its `let-vars'
and `body' arguments,respectively.  HEIGHT-EXP is an expression
 calculating the image height and it should contain a free variable `height'.
PATTERN-HEIGHT-SYM and SECOND-PATTERN-HEIGHT-SYM are symbols used
for let-var binding variables."
  (let* ((pattern (centaur-tabs-separator-pattern (cl-mapcar 'centaur-tabs-separator-pattern-to-string (car patterns))))
	 (header (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 1 patterns)))
	 (footer (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 2 patterns)))
	 (second-pattern (centaur-tabs-separator-pattern (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 3 patterns))))
	 (center (cl-mapcar 'centaur-tabs-separator-pattern-to-string (nth 4 patterns)))
	 (reserve (+ (length header) (length footer) (length center))))
    (when pattern
      (cons `((,pattern-height-sym (max (- ,height-exp ,reserve) 0))
	      (,second-pattern-height-sym (/ ,pattern-height-sym 2))
	      (,pattern-height-sym ,(if second-pattern `(ceiling ,pattern-height-sym 2) `,pattern-height-sym)))
	    (list (when header `(mapconcat 'identity ',header ""))
		  `(mapconcat 'identity
			      (cl-subseq ',pattern 0 ,pattern-height-sym) "")
		  (when center `(mapconcat 'identity ',center ""))
		  (when second-pattern
		    `(mapconcat 'identity
				(cl-subseq ',second-pattern
					   0 ,second-pattern-height-sym) ""))
		  (when footer `(mapconcat 'identity ',footer "")))))))

(defun centaur-tabs-separator-pattern-defun (name dir width &rest patterns)
  "Create a powerline function of NAME in DIR with WIDTH for PATTERNS.

PATTERNS is of the form (PATTERN HEADER FOOTER SECOND-PATTERN CENTER
PATTERN-2X HEADER-2X FOOTER-2X SECOND-PATTERN-2X CENTER-2X).
PATTERN is required, all other components are optional.
The first 5 components are for the standard resolution image.
The remaining ones are for the high resolution image where both
width and height are doubled.  If PATTERN-2X is nil or not given,
then the remaining components are ignored and the standard
resolution image with magnification and interpolation will be
used in high resolution environments

All generated functions generate the form:
HEADER
PATTERN ...
CENTER
SECOND-PATTERN ...
FOOTER

PATTERN and SECOND-PATTERN repeat infinitely to fill the space needed to
generate a full height XPM.

PATTERN, HEADER, FOOTER, SECOND-PATTERN, CENTER are of the form
\((COLOR ...) (COLOR ...) ...).

COLOR can be one of 0, 1, or 2, where 0 is the source color, 1 is the
destination color, and 2 is the interpolated color between 0 and 1."
  (when (eq dir 'right)
    (setq patterns (cl-mapcar 'centaur-tabs-separator-reverse-pattern patterns)))
  (let ((bindings-body (centaur-tabs-separator-pattern-bindings-body patterns
								     'height
								     'pattern-height
								     'second-pattern-height))
	(bindings-body-2x (centaur-tabs-separator-pattern-bindings-body (nthcdr 5 patterns)
									'(* height 2)
									'pattern-height-2x
									'second-pattern-height-2x)))
    (centaur-tabs-separator-wrap-defun name dir width
				       (append (car bindings-body) (car bindings-body-2x))
				       (cdr bindings-body) (cdr bindings-body-2x))))

(defun centaur-tabs-separator-background-color (face)
  "Set the separator background color using FACE."
  (face-attribute face
		  (if (face-attribute face :inverse-video nil 'default)
		      :foreground
		    :background)
		  nil
		  'default))

(defun centaur-tabs-separator-wrap-defun (name dir width let-vars body &optional body-2x)
  "Generate a powerline function of name NAME in dir DIR.
This is made with WIDTH using LET-VARS and BODY.
BODY-2X is an optional argument."
  (let* ((src-face (if (eq dir 'left) 'face1 'face2))
	 (dst-face (if (eq dir 'left) 'face2 'face1)))
    `(defun ,(intern (format "powerline-%s-%s" name (symbol-name dir)))
	 (face1 face2 &optional height)
       (when window-system
	 (unless height (setq height centaur-tabs-height))
	 (let* ,(append `((color1 (when ,src-face
				    (centaur-tabs-separator-hex-color (centaur-tabs-separator-background-color ,src-face))))
			  (color2 (when ,dst-face
				    (centaur-tabs-separator-hex-color (centaur-tabs-separator-background-color ,dst-face))))
			  (colori (when (and color1 color2) (centaur-tabs-separator-interpolate color1 color2)))
			  (color1 (or color1 "None"))
			  (color2 (or color2 "None"))
			  (colori (or colori "None")))
			let-vars)
	   (apply #'create-image
		  ,(append `(concat (format "/* XPM */ static char * %s_%s[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
					    ,(replace-regexp-in-string "-" "_" name)
					    (symbol-name ',dir)
					    ,width
					    height
					    color1
					    color2
					    colori))
			   body
			   '("};"))
		  'xpm t
		  :ascent 'center
		  :face (when (and face1 face2)
			  ,dst-face)
		  ,(and body-2x
			`(and (featurep 'mac)
			      (list :data-2x
				    ,(append `(concat (format "/* XPM */ static char * %s_%s_2x[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
							      ,(replace-regexp-in-string "-" "_" name)
							      (symbol-name ',dir)
							      (* ,width 2)
							      (* height 2)
							      color1
							      color2
							      colori))
					     body-2x
					     '("};")))))))))))

(defun centaur-tabs-separator-alternate (dir)
  "Generate an alternating pattern XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "alternate" dir 4
					'((2 2 1 1)
					  (0 0 2 2))
					nil nil nil nil
					;; 2x
					'((2 2 2 2 1 1 1 1)
					  (2 2 2 2 1 1 1 1)
					  (0 0 0 0 2 2 2 2)
					  (0 0 0 0 2 2 2 2))))

(defun centaur-tabs-separator-bar (dir)
  "Generate a bar XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "bar" dir 2
					'((2 2))))

(defun centaur-tabs-separator-box (dir)
  "Generate a box XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "box" dir 2
					'((0 0)
					  (0 0)
					  (1 1)
					  (1 1))
					nil nil nil nil
					;; 2x
					'((0 0 0 0)
					  (0 0 0 0)
					  (0 0 0 0)
					  (0 0 0 0)
					  (1 1 1 1)
					  (1 1 1 1)
					  (1 1 1 1)
					  (1 1 1 1))))

(defun centaur-tabs-separator-chamfer (dir)
  "Generate a chamfer XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "chamfer" dir 3
					'((0 0 0))
					'((1 1 1)
					  (0 1 1)
					  (0 0 1))
					nil nil nil
					;; 2x
					'((0 0 0 0 0 0))
					'((1 1 1 1 1 1)
					  (0 1 1 1 1 1)
					  (0 0 1 1 1 1)
					  (0 0 0 1 1 1)
					  (0 0 0 0 1 1)
					  (0 0 0 0 0 1))))

(defun centaur-tabs-separator-rounded (dir)
  "Generate a rounded XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "rounded" dir 6
					'((0 0 0 0 0 0))
					'((2 1 1 1 1 1)
					  (0 0 2 1 1 1)
					  (0 0 0 0 1 1)
					  (0 0 0 0 2 1)
					  (0 0 0 0 0 1)
					  (0 0 0 0 0 2))
					nil nil nil
					;; 2x
					'((0 0 0 0 0 0 0 0 0 0 0 0))
					'((1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 2 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 2 1 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1)
					  (0 0 0 0 0 0 0 0 0 0 2 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1))))

(defun centaur-tabs-separator-slant (dir)
  "Generate a slant XPM function for DIR."
  (let* ((row-modifier (if (eq dir 'left) 'identity 'reverse)))
    (centaur-tabs-separator-wrap-defun "slant" dir 'width
				       '((width (1- (ceiling height 2))))
				       `((cl-loop for i from 0 to (1- height)
						  concat (centaur-tabs-separator-pattern-to-string (,row-modifier (centaur-tabs-separator-row-pattern (/ i 2) width)))))
				       `((cl-loop for i from 0 to (1- (* height 2))
						  concat (centaur-tabs-separator-pattern-to-string (,row-modifier (centaur-tabs-separator-row-pattern (/ i 2) (* width 2)))))))))

(defun centaur-tabs-separator-wave (dir)
  "Generate a wave XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "wave" dir 11
					'((0 0 0 0 0 0 1 1 1 1 1))
					'((2 1 1 1 1 1 1 1 1 1 1)
					  (0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 2 1 1 1 1 1 1 1)
					  (0 0 0 0 1 1 1 1 1 1 1)
					  (0 0 0 0 2 1 1 1 1 1 1)
					  (0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 2 1 1 1 1 1))
					'((0 0 0 0 0 0 2 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1)
					  (0 0 0 0 0 0 0 2 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1)
					  (0 0 0 0 0 0 0 0 2 1 1)
					  (0 0 0 0 0 0 0 0 0 0 2))
					nil nil
					;; 2x
					'((0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1))
					'((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1))
					'((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 1 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
					  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defun centaur-tabs-separator-zigzag (dir)
  "Generate a zigzag pattern XPM function for DIR."
  (centaur-tabs-separator-pattern-defun "zigzag" dir 3
					'((1 1 1)
					  (0 1 1)
					  (0 0 1)
					  (0 0 0)
					  (0 0 1)
					  (0 1 1))
					nil nil nil nil
					;; 2x
					'((1 1 1 1 1 1)
					  (0 1 1 1 1 1)
					  (0 0 1 1 1 1)
					  (0 0 0 1 1 1)
					  (0 0 0 0 1 1)
					  (0 0 0 0 0 1)
					  (0 0 0 0 0 0)
					  (0 0 0 0 0 1)
					  (0 0 0 0 1 1)
					  (0 0 0 1 1 1)
					  (0 0 1 1 1 1)
					  (0 1 1 1 1 1))))

(defun centaur-tabs-separator-memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the memoized function over
the original function.  Use frame-local memoization."
  (cl-typecase func
    (symbol (fset func (centaur-tabs-separator-memoize-wrap-frame-local (symbol-function func))) func)
    (function (centaur-tabs-separator-memoize-wrap-frame-local func))))

(defun centaur-tabs-separator-memoize-wrap-frame-local (func)
  "Return the memoized version of FUNC.
The memoization cache is frame-local."
  (let ((funcid (cl-gensym)))
    `(lambda (&rest args)
       ,(concat (documentation func) (format "\n(memoized function %s)" funcid))
       (let* ((cache (centaur-tabs-separator-create-or-get-cache))
	      (key (cons ',funcid args))
	      (val (gethash key cache)))
	 (if val
	     val
	   (puthash key (apply ,func args) cache))))))

(defun centaur-tabs-separator-create-or-get-cache ()
  "Return a frame-local hash table that acts as a memoization cache.
The cache is for the powerline.
Create one if the frame doesn't have one yet."
  (let ((table (frame-parameter nil 'powerline-cache)))
    (if (hash-table-p table) table (centaur-tabs-separator-reset-cache))))

(defun centaur-tabs-separator-reset-cache ()
  "Reset and return the frame-local hash table used for a memoization cache."
  (let ((table (make-hash-table :test 'equal)))
    ;; Store it as a frame-local variable
    (modify-frame-parameters nil `((powerline-cache . ,table)))
    table))

(centaur-tabs-separator-memoize (centaur-tabs-separator-alternate 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-alternate 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-bar 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-bar 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-box 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-box 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-chamfer 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-chamfer 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-rounded 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-rounded 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-slant 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-slant 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-wave 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-wave 'right))
(centaur-tabs-separator-memoize (centaur-tabs-separator-zigzag 'left))
(centaur-tabs-separator-memoize (centaur-tabs-separator-zigzag 'right))

(defun centaur-tabs-select-separator-style (tab-style)
  "Set the separator style to TAB-STYLE."
  (setq centaur-tabs-style-left (funcall (intern (format "powerline-%s-right" tab-style)) 'centaur-tabs-default nil centaur-tabs-height))
  (setq centaur-tabs-style-right (funcall (intern (format "powerline-%s-left" tab-style)) nil 'centaur-tabs-default centaur-tabs-height)))

(provide 'centaur-tabs-elements)

;;; centaur-tabs-elements.el ends here
