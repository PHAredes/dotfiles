;;; modules/tools/config.el -*- lexical-binding: t; -*-

(after! eev
  (defun find-angg (fname &rest rest)
    (apply 'find-wgeta (format "http://anggtwu.net/%s" fname) rest))
  
  (defun find-es (fname &rest rest)
    (apply 'find-wgeta (format "http://anggtwu.net/e/%s.e" fname) rest))
  
  (defun find-chrome (url)
    (find-bgprocess `("google-chrome-stable" ,url)))
  
  (defun find-eww2 (url &rest comments) 
    (find-2a nil `(find-eww ,url)))
  
  (code-brurl 'find-eww2 :remote 'breww2 :local 'breww2l :dired 'breww2d))
