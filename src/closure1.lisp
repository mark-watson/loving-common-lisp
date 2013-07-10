(let* ((fortunes
	'("You will become a great Lisp Programmer"
	  "The force will not be with you"
	  "Take time for meditation"))
       (len (length fortunes))
       (index 0))
  (defun fortune ()
    (let ((new-fortune (nth index fortunes)))
      (setq index (1+ index))
      (if (>= index len) (setq index 0))
      new-fortune)))

