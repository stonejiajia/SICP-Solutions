(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define (square x) (* x x))



 
(defconst stone-org-packages
  '()
  )

;;* Babel
(add-to-list 'load-path "~/.emacs.d/elpa/ess-20170227.802/lisp")

;;; packages.el ends here
;;; ob-ipython 
(defun ipython-notebook/init-ob-ipython ()
  (use-package ob-ipython
    :defer t
    :init
    (org-babel-do-load-languages 'org-babel-load-languages '((ipython . t)))))





(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)


(eval-after-load 'org
'(progn
     (add-to-list 'org-structure-template-alist
                  '("rr" "#+BEGIN_SRC R :exports both :results graphics :file ./fig_1?.png\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))

     (add-to-list 'org-structure-template-alist
                  '("sr" "#+BEGIN_SRC R :exports both :session \n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))

     (add-to-list 'org-structure-template-alist
                  '("si" "#+BEGIN_SRC ipython :session :results output  :exports both  \n\n#+END_SRC"))

     (add-to-list 'org-structure-template-alist
                  '("ss" "#+BEGIN_SRC ipython :session :exports both  \n\n#+END_SRC"))

     (add-to-list 'org-structure-template-alist
                  '("sif" "#+BEGIN_SRC ipython :session :exports both :file ./figure/fig_1?.png\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>")
                 )

     ))



(setq org-agenda-files (list "/Users/stone20091652/org-notes/"))

(setq deft-extensions '("org" "md" "txt"))

(setq image-file-name-extensions
   (quote
    ("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg" "pdf" "bmp")))

(setq org-image-actual-width 600)

(setq org-imagemagick-display-command "convert -density 600 \"%s\" -thumbnail \"%sx%s>\" \"%s\"")
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This
can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (unless refresh
    (org-remove-inline-images)
    (if (fboundp 'clear-image-cache) (clear-image-cache)))
  (save-excursion
    (save-restriction
      (widen)
      (setq beg (or beg (point-min)) end (or end (point-max)))
      (goto-char beg)
      (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
                        (substring (org-image-file-name-regexp) 0 -2)
                        "\\)\\]" (if include-linked "" "\\]")))
            old file ov img)
        (while (re-search-forward re end t)
          (setq old (get-char-property-and-overlay (match-beginning 1)
                                                   'org-image-overlay)
        file (expand-file-name
                      (concat (or (org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   (ipython . t)
   (sh . t)
   (dot . t)
   (js . t)
   (latex .#+BEGIN_SRC python :results output org drawer

#+END_SRCt)
   (python . t)
   (emacs-lisp . t)
   (plantuml . t)
   (C . t)
   (ditaa . t)))(match-string 3) "") (match-string 4))))
          (when (file-exists-p file)
            (let ((file-thumb (format "%s%s_thumb.png" (file-name-directory file) (file-name-base file))))
              (if (file-exists-p file-thumb)
                  (let ((thumb-time (nth 5 (file-attributes file-thumb 'string)))
                        (file-time (nth 5 (file-attributes file 'string))))
                    (if (time-less-p thumb-time file-time)
            (shell-command (format org-imagemagick-display-command
                           file org-image-actual-width org-image-actual-width file-thumb) nil nil)))
                (shell-command (format org-imagemagick-display-command
                                         file org-image-actual-width org-image-actual-width file-thumb) nil nil))
              (if (and (car-safe old) refresh)
                  (image-refresh (overlay-get (cdr old) 'display))
                (setq img (save-match-data (create-image file-thumb)))
                (when img
                  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                  (overlay-put ov 'display img)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put ov 'modification-hooks
                               (list 'org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))
