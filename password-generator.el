;;; password-generator.el --- Password generator for humans. Good, Bad, Phonetic passwords included.

;; Copyright (C) 2015 Zargener

;; Author: Zargener <zargener@gmail.com>
;; URL: http://github.com/zargener/emacs-password-genarator
;; Version: 1.0

;;; Commentary:
     
;; Generate a password and insert it in-place. Such functions provided:
;; pass-get-simple   - simple password for most websites
;; pass-get-phonetic - easy to remeber password
;; pass-get-strong   - strong password and still suitable for most web sites with strange password requirements to used special chars.

;; Use C-u <length> pass-get-simple to specify length of generated password

;; See full docs here: http://github.com/zargener/emacs-password-genarator

;;; Code:

(defun get-random-string-char (string)
  "You pass here string. You get random character from it."
  (let* ((random-symbol 0)) 
    (progn
      (setq random-symbol (random (length string)))
      (substring string random-symbol (+ 1 random-symbol)))))



(defun pass-generate-internal (symbols-for-pass pass-length)
  "Generates the password with given vocabulary and length"
  (let* 
      ((iter 0)
       (password ""))
    (while (< iter pass-length) 
      (progn
        (setq 
         password 
         (concat password (get-random-string-char symbols-for-pass)))
         (setq iter (+ 1 iter))))
    password))


(defun pass-get-simple (&optional pre-len return)
  "Minimal viable password for most of web systems. It is not secure but allows to register."
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 8))
         (symbols-for-pass "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (setq password (pass-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


(defun pass-get-strong (&optional pre-len return)
  "The best password you can get. Some symbols does not included to make you free from problems which occurs when your shell tries interpolate $, \\ and others."
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 12))
         (symbols-for-pass "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ_@!.^%,&-"))
    (setq password (pass-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


(defun pass-get-paranoid (&optional pre-len return)
  "Good thing to use if you really care about bruteforce. Not all applications handle special characters presented in such password properly. Be ready to escape special characters if you will pass such password via ssh command or so."
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 20))
         (symbols-for-pass "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-+=/?,.><[]{}~"))
    (setq password (pass-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


(defun pass-get-numeric (&optional pre-len return)
  "Yeah, there are sill reasons to use numeric passwords like credit card PIN-code"
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 4))
         (symbols-for-pass "0123456789"))
    (setq password (pass-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


(defun pass-get-phonetic (&optional pre-len return)
  "It will be easy to remeber, because of fonetic, but not so secure..."
  (interactive)
  (let*
      ((password "")
       (letters-A "eoai")
       (letters-B "rtpsdfghjkzxcvbnm")
       (letters-N "123456789")
       (pass-length (or pre-len current-prefix-arg 8))
       (iter 0) 
       (max-iter (+ 1 (/ pass-length 3)))
       (password ""))
    (while (< iter max-iter)
      (progn 
        (setq password (concat password
                               (get-random-string-char letters-B)
                               (get-random-string-char letters-A)
                               (get-random-string-char letters-N)))
        (setq iter (+ 1 iter))))
    (setq password (substring password 0 pass-length))
    (cond ((equal nil return) (insert password)) (t password))))

(provide 'password-generator)

;;; password-generator.el  ends here
