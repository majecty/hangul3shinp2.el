;;; hangul3shinp2.el --- Hangul shin sebeol p2 input method  -*- coding: utf-8 -*-

;; Copyright (c) 2021 majecty
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Version: 0.1
;; Author: majecty
;; Keywords: hangul input-method
;; URL: https://github.com/majecty/emacs-3shinp2

;;; Commentary:
;; This plugin adds Hangul Shin Sebeol p2 input method.
;; This plugin uses internal functions and vars defined in the leim/quail/hangul.el
;; It will be better to remove the dependency.

;; In this code, key is a value that passed from Emacs input.
;; char or character is a value that is mapped by hangul3shinp2-keymap.
;; Key has richer information that code.
;; Different keys can be the same characters.

;;; Code:
(require 'hangul)

;; 87 ~ 90: (- 86) cho map to 1~4 ㄱ ㄲ ㄱㅅ ㄴ
;; 97 ~ 122: (- 92) cho map to 5~30
;; 65 ~ 85: (- 34) jung map to 31~51
;; 0 ~ 30: jong map to 0~30
;; The character values are the same as hangul390-keymap in the leim/quail/hangul.el
;; This maps printable ascii values to arbitary numeric code
;; for hangul chosung, jungsung and jongsung.
(defconst hangul3shinp2-keymap
  (vector 33	       ;; !
	  47	       ;; " to /
	  35	       ;; #
	  36	       ;; $
	  37	       ;; %
	  38	       ;; &
	  (+ 28 92)   ;; ' to ㅌ(초)
	  40	       ;; (
	  41	       ;; )
	  42	       ;; *
	  43	       ;; +
	  44	       ;; ,
	  45	       ;; -
	  46	       ;; .
	  (+ 27 92)   ;; / to ㅋ(초)
	  48	       ;; 0
	  49	       ;; 1
	  50	       ;; 2
	  51	       ;; 3
	  52	       ;; 4
	  53	       ;; 5
	  54	       ;; 6
	  55	       ;; 7
	  56	       ;; 8
	  57	       ;; 9
	  58	       ;; :
	  (+ 18 92)   ;; ; to ㅂ(초)
	  60	       ;; <
	  61	       ;; =
	  62	       ;; >
	  63	       ;; ? to ㅗ
	  64	       ;; @
	  (+ 48 34)   ;; A to ㅠ
	  (+ 44 34)   ;; B to ㅜ
	  (+ 36 34)   ;; C to ㅔ
	  (+ 51 34)   ;; D to ㅣ
	  (+ 32 34)   ;; E to ㅐ
	  (+ 31 34)   ;; F to ㅏ
	  (+ 49 34)   ;; G to ㅡ
	  (+ 4 86)    ;; H to ㄴ(초성) TODO
	  (+ 17 92)   ;; I to ㅁ(초성) TODO
	  39	       ;; J to '
	  34	       ;; K to "
	  (+ 24 92)   ;; L to ㅈ(초성) TODO
	  (+ 30 92)   ;; M to ㅎ(초성) TODO
	  (+ 21 92)   ;; N to ㅅ(초성) TODO
	  (+ 26 92)   ;; O to ㅊ(초성) TODO
	  59	      ;; P to ;
	  (+ 34 34)   ;; Q to ㅒ
	  (+ 35 34)   ;; R to ㅓ
	  (+ 38 34)   ;; S to ㅖ
	  (+ 37 34)   ;; T to ㅕ
	  (+ 7 92)    ;; U to ㄷ(초성) TODO
	  (+ 39 34)   ;; V to ㅗ
	  (+ 33 34)   ;; W to ㅑ
	  (+ 43 34)   ;; X to ㅛ
	  (+ 9 92)    ;; Y to ㄹ(초성) TODO
	  17	       ;; Z to ㅁ(종성) TODO
	  91	       ;; [
	  92	       ;; \
	  93	       ;; ]
	  94	       ;; ^
	  95	       ;; _
	  96	       ;; `
	  23	       ;; a to ㅇ(종성)
	  26	       ;; b to ㅊ(종성)
	  1	       ;; c to ㄱ(종성)
	  30	       ;; d to ㅎ(종성)
	  18	       ;; e to ㅂ(종성)
	  29	       ;; f to ㅍ(종성)
	  7	       ;; g to ㄷ(종성)
	  (+ 4 86)    ;; h to ㄴ(초성)
	  (+ 17 92)   ;; i to ㅁ(초성)
	  (+ 23 92)   ;; j to ㅇ(초성)
	  (+ 1 86)    ;; k to ㄱ(초성)
	  (+ 24 92)   ;; l to ㅈ(초성)
	  (+ 30 92)   ;; m to ㅎ(초성)
	  (+ 21 92)   ;; n to ㅅ(초성)
	  (+ 26 92)   ;; o to ㅊ(초성)
	  (+ 29 92)   ;; p to ㅍ(초성)
	  21	       ;; q to ㅅ(종성)
	  28	       ;; r to ㅌ(종성)
	  4	       ;; s to ㄴ(종성)
	  27	       ;; t to ㅋ(종성)
	  (+ 7 92)    ;; u to ㄷ(초성)
	  24	       ;; v to ㅈ(종성)
	  9	       ;; w to ㄹ(종성)
	  22	       ;; x to ㅆ(종성)
	  (+ 9 92)     ;; y to ㄹ(초성)
	  17	       ;; z to ㅁ(종성)
	  123	       ;; {
	  124	       ;; |
	  125	       ;; }
	  126	       ;; ~
	  ))

(defun hangul3shinp2--only-cho ()
  "Check that hangul-queue only contain chosung."
  (and (notzerop (aref hangul-queue 0))
       (zerop (aref hangul-queue 2))
       (zerop (aref hangul-queue 3))
       (zerop (aref hangul-queue 4))))

(defun hangul3shinp2--empty ()
  "Check that hangule-queue is empty."
  (and (zerop (aref hangul-queue 0))
       (zerop (aref hangul-queue 1))
       (zerop (aref hangul-queue 2))
       (zerop (aref hangul-queue 3))
       (zerop (aref hangul-queue 4))))

(defun hangul3shinp2--only-cho-single-mo ()
  "Check that hangule-queue only contain chosung and single jungsung."
  (and (notzerop (aref hangul-queue 0))
       (notzerop (aref hangul-queue 2))
       (zerop (aref hangul-queue 3))
       (zerop (aref hangul-queue 4))))

(defun hangul3shinp2--is-galma-cho (char)
  "Check the CHAR is available for galmadeuli."
  (or (eq char (+ 17 92)) ;; ㅁ -> ㅡ
      (eq char (+ 26 92)) ;; ㅊ -> ㅜ
      (eq char (+ 27 92)))) ;; ㅋ -> ㅗ

(defun hangul3shinp2--galma-cho-to-jung (char)
  "Translate chosung(CHAR) to jungsung."
  (cond ((eq char (+ 17 92))     ; ㅁ
	 (+ 49 34))        ; ㅡ
	((eq char (+ 26 92))      ; ㅊ
	 (+ 44 34))        ; ㅜ
	((eq char (+ 27 92))      ; ㅋ
	 (+ 39 34))        ; ㅗ
	(t
	 char)))

(defun hangul3shinp2--is-jong (char)
  "Check the CHAR is jongsung or nog."
  (< char 31))

;; jong: lower case. ex) a: 97
;; jung: upper case. ex) A: 65
(defun hangul3shinp2--galma-jong-to-jung (key char)
  "Translate the KEY(CHAR) jongsung to jungsung."
  (when (hangul3shinp2--is-jong char)
    (let* ((upper-case (- key 32))
	   (from-bang (- upper-case 33)))
     (aref hangul3shinp2-keymap from-bang))))

;; Original keycode that is typed previously.
;; This is used to check the double-jamo like ㅘ
(defvar hangul3shinp2--prev-key
  nil)

(defun hangul3shinp2--prev-key-o-u-eu ()
  "Check the prev key is o, u, or eu."
  (or (eq hangul3shinp2--prev-key 47)  ; /
      (eq hangul3shinp2--prev-key 105)  ; i
      (eq hangul3shinp2--prev-key 111)))  ; o

(defun hangul3shinp2--is-double-mo (key char)
  "Check the KEY(CHAR) can be merged to the previous key."
  (cond ((and (eq hangul3shinp2--prev-key 47))  ; /,ㅗ
         (or (eq key 102)           ; ㅏ
	     (eq key 101)           ; ㅐ
	     (eq key 100))         ; ㅣ
	 t)               ; ㅘ ㅙ ㅚ
	((and (eq hangul3shinp2--prev-key 105) ; i,ㅡ
              (eq key 100))    ; ㅣ
	 t)        ; ㅢ
	((and (eq hangul3shinp2--prev-key 111) ; o,ㅜ
              (or (eq key 114)    ; ㅓ
		  (eq key 99)    ; ㅔ
		  (eq key 100)))  ; ㅣ
	 t)        ; ㅝ ㅞ ㅟ
	(t
	 nil)))

(defun hangul3shinp2--update-galma (key char)
  "Translate CHAR to another character with KEY."
  (cond ((and (hangul3shinp2--only-cho)
              (hangul3shinp2--is-galma-cho char))
         (hangul3shinp2--galma-cho-to-jung char))
	((and (hangul3shinp2--only-cho)
              (hangul3shinp2--is-jong char))
	 (hangul3shinp2--galma-jong-to-jung key char))
	((and (hangul3shinp2--is-double-mo key char)
              (hangul3shinp2--only-cho-single-mo))
	 (hangul3shinp2--galma-jong-to-jung key char))
	(t
	 char)))

(defun hangul3shinp2-input-method-internal (key)
  "Receive a KEY from Emacs and handle it."
  (let* ((char (aref hangul3shinp2-keymap (- key 33)))
         (char3 (hangul3shinp2--update-galma key char)))
    (cond ((or (and (> char3 86) (< char3 91))
               (and (> char3 96) (< char3 123)))
           (hangul3-input-method-cho (- char3 (if (< char3 97) 86 92))))
          ((and (> char3 64) (< char3 86))
           (hangul3-input-method-jung (- char3 34)))
          ((< char3 31)
           (hangul3-input-method-jong char3))
          (t
           (setq hangul-queue (make-vector 6 0))
           (insert (decode-char 'ucs char3))
           (move-overlay quail-overlay (point) (point))))))

(defun hangul3shinp2-input-method-internal-with-prev (key)
  "Call (intput-method-interval KEY) while updating the prev-key."
  (let ((char (hangul3shinp2-input-method-internal key)))
    (setq hangul3shinp2--prev-key key)
    char))

(defun hangul3shinp2-input-method (key)
  "The main function. KEY is the first input key from Emacs."
  (if (or buffer-read-only (< key 33) (>= key 127))
      (list key)
    (quail-setup-overlays nil)
    (let ((input-method-function nil)
          (echo-keystrokes 0)
          (help-char nil))
      (setq hangul-queue (make-vector 6 0))
      (hangul3shinp2-input-method-internal-with-prev key)
      (unwind-protect
          (catch 'exit-input-loop
            (while t
              (let* ((seq (read-key-sequence nil))
                     (cmd (lookup-key hangul-im-keymap seq))
                     key)
                (cond ((and (stringp seq)
                            (= 1 (length seq))
                            (setq key (aref seq 0))
                            (and (>= key 33) (< key 127)))
                       (hangul3shinp2-input-method-internal-with-prev key))
                      ((commandp cmd)
                       (call-interactively cmd))
                      (t
                       (setq unread-command-events
                             (nconc (listify-key-sequence seq)
                                    unread-command-events))
                       (throw 'exit-input-loop nil))))))
	(quail-delete-overlays)))))

(register-input-method
 "korean-hangul3shinp2"
 "UTF-8"
 'hangul-input-method-activate
 "한3신p2"
 "Hangul 3-Bulsik shin p2 Input"
 'hangul3shinp2-input-method
 "Input method: korean-hangul3shinp2 (mode line indicator:한3신p2)\n\nHangul 3-Bulsik shin p2 input method.")

(provide 'hangul3shinp2)

;;; hangul3shinp2.el ends here
