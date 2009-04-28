;;; bike.el
;
; version 0.2 05-July-1999
;
; Calculate bicycle gearing.  Yes, there are a million of these programs
; around, but none for emacs.  This one is bare bones and just gives you
; an ugly tabular output in the buffer "*Gear Inches*".  There is an
; advantage to this.  You can go to this buffer, then evaluate
;
;  (calc-grab-rectangle (goto-char (point-min)) (goto-char point-max))
;
; to drop the output into an Emacs Calculator array, after which you can do
; all the postprocessing you ever dreamed about, including plotting.
; The same data file can be used, with some processing, for your
; favorite plotting program such as Gnuplot.
;
; LaTeX output is now an option.  It is an either/or proposition, either
; the ugly but useful text table or LaTeX source.  LaTeX output more or
; less assumes you have the auctex package installed, or at least
; something that gives you a LaTeX mode (lacking which, this program
; will fail unless you comment out the (latex-mode) line down below).
;
; Usage of this program is really self-explanatory.  It should be byte-
; compiled and loaded prior to invoking with M-x bike-gears.  The only
; weirdness is that output of chainwheel and cassette calculations are
; in the opposite order from the input order.
;
; Ideas, fixes, and bug reports are welcome at
;
;  bnewell@alum.mit.edu
;
; There is no guarantee of support.  Use of this program is at your own
; risk.  The author assumes no liability for anything whatsoever.
;
; This program is released into the public domain.

(defun bike-gears () "Calculate bicycle gearing" (interactive)

(setq cw (string-to-list (read-string
      "Enter chainwheel teeth separated by a space: ")))
(setq cas (string-to-list (read-string
      "Enter cassette teeth separated by a space: ")))
(setq wh (float (string-to-number (read-string "Enter wheel size: "))))
(setq latex-or-table (yes-or-no-p "LaTeX output? "))

; No reuse.  Save in-between if you want intermediate results.
(if (get-buffer "*Gear Inches*")
    (kill-buffer "*Gear Inches*"))
(if (get-buffer "bikegear.tex")
    (kill-buffer "bikegear.tex"))

(setq gear-buffer (get-buffer-create "*Gear Inches*"))
; Preamble if LaTeX.
(cond ( latex-or-table
        (setq title (read-string "Enter title for table: "))
        (save-excursion
          (set-buffer gear-buffer)
; LaTeX headers
          (insert "\\documentclass[12pt]{article}\n")
          (insert "\\begin{document}\n")
          (insert "\\thispagestyle{empty}\n")
          (insert "\\begin{center}\n")
          (insert "{\\bf\\Large Bicycle Gearing Chart} \\\\ \n")
          (insert "{\\bf\\Large ")
          (insert title)
          (insert "} \\\\ \n")
          (insert "\\vspace{.5in}\n")
          (insert "{\\large Wheel Size ")
          (insert (format "%.2f" wh))
          (insert "} \\\\ \n")
          (insert "\\vspace{.1in}\n")
; Now the cassette stuff and the table setup.  Ugga-bugga.
          (insert "\\begin{tabular}{|r|")    ; column for chainwheels
          (setq castemp cas)
          (while
              (setq casnow (car castemp))
              (setq castemp (cdr castemp))
              (insert "r|")
          )
          (insert "}\n\\hline \n")
          (setq castemp cas)
          (while
              (setq casnow (car castemp))
              (setq castemp (cdr castemp))
              (insert " & ")
              (insert casnow)
          )
          (insert " \\\\ \\hline\n")
        )        
))

(while
    (setq cwnow (car cw))
    (setq cw (cdr cw))
    (setq castemp cas)
; Start of chainwheel group, begin line.
    (cond (latex-or-table
           (save-excursion
             (set-buffer gear-buffer)
             (goto-char (point-max))
             (insert cwnow)
    )))
    (while
        (setq casnow (car castemp))
        (setq castemp (cdr castemp))
        (setq ginches (* wh (/ (float (string-to-number cwnow))
              (float (string-to-number casnow)))))
        (save-excursion
          (set-buffer gear-buffer)
          (goto-char (point-max))
          (cond ( (not latex-or-table)
                  (insert cwnow)
                  (insert " ")
                  (insert casnow)
                  (insert " ")
                  (insert (format "%.2f" ginches))
                  (insert "\n")
                )
                (t
                  (insert " & ")
                  (insert (format "%.2f" ginches))  
                 ))
         )
    )
; End of chainwheel group, close out line.
    (cond ( latex-or-table
           (save-excursion
             (set-buffer gear-buffer)
             (goto-char (point-max))
             (insert " \\\\ \\hline \n")
    )))
)

; Postamble if LaTeX
(cond ( latex-or-table
        (save-excursion
          (set-buffer gear-buffer)
          (goto-char (point-max))
          (insert "\\end{tabular}\n")
          (insert "\\end{center}\n")
          (insert "\\end{document}\n")
          (goto-char (point-min))
          (latex-mode)
          (write-file "bikegear.tex")
)))
(switch-to-buffer gear-buffer)
)

(defun string-to-list (intext) "parse blank separated tokens from string"
; This is really ugly, and I can't believe I'm unable to locate stock code
; for this type of parsing.  Can anyone help?
; UPDATE: I've received a replacement from a kind person in Germany but
; have yet to install it...
(setq outlist (list ()))
(setq pointer 0)
(setq token " ")
(setq doing 0)
(while
  (< pointer (length intext))
  (cond
    ( (= doing 0)
      (cond
       ( (not (eq " " (substring intext pointer (1+ pointer))))
         (setq doing 1)
         (setq token (substring intext pointer (1+ pointer))))))
    (t    
      (cond
       ( (not (string= " " (substring intext pointer (1+ pointer))))
         (setq token (concat token
              (substring intext pointer (1+ pointer)))))
       (t  
         (setq outlist (cons token outlist ))
         (setq doing 0)
         (setq token " "))))
   )
  (setq pointer (1+ pointer))
)
(if (not (string= token " "))
     (setq outlist (cons token outlist))))

;;; end of bike.el
