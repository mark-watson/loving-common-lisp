;; Perform a heuristic A* search between the start and goal nodes:
;;
;; Copyright 1990, 2017 by Mark Watson

(defun A*search (nodes paths start goal &aux possible-paths best)

  (defun Y-coord (x) (truncate (cadr x)))
  (defun X-coord (x) (truncate (car x)))

  (defun dist-between-points (point1 point2)
    (let ((x-dif (- (X-coord point2) (X-coord point1)))
          (y-dif (- (Y-coord point2) (Y-coord point1))))
      (sqrt (+ (* x-dif x-dif)  (* y-dif y-dif)))))

  (setq possible-paths
        (list
         (list
          (dist-between-points
           (eval start)
           (eval goal))
          0
          (list start))))

  (defun init-network ()
    (setq paths (init-lengths paths))
    (init-path-list nodes paths))

  (defun init-lengths (pathlist)
    (let (new-path-list pathlength path-with-length)
      (dolist (path pathlist)
        (setq pathlength (slow-path-length path))
        (setq path-with-length (append path (list pathlength)))
        (setq new-path-list (cons path-with-length new-path-list)))
      new-path-list))

  (defun init-path-list (nodes paths)
    (dolist (node nodes)
      (setf
       (get node 'path-list)
       ;; let returns all paths connected to node:
       (let (path-list)
         (dolist (path paths)
           (if (equal node (start-node-name path))
               (setq path-list
                     (cons (list (end-node-name path)
                                 (path-length path))
                           path-list))
               (if (equal node (end-node-name path))
                   (setq path-list (cons (list (start-node-name path)
                                               (path-length path))
                                         path-list)))))
         path-list ))))

  (defun slow-path-length (path)
    (dist-between-points (start-node path) (end-node path)))

  (defun path-length (x) (caddr x))

  (defun start-node (path) (eval (car path)))
  (defun end-node (path) (eval (cadr path)))
  (defun start-node-name (x) (car x))
  (defun end-node-name (x) (cadr x))
  (defun first-on-path (x) (caddr x))
  (defun goal-node (x) (car x))
  (defun distance-to-that-node (x) (cadr x))

  (defun enumerate-children (node goal)
    (let* (
           (start-to-lead-node-dist (cadr node)) ;; distance already calculated
           (path (caddr node))
           (lead-node (car path)))
      (if (get-stored-path lead-node goal)
          (consider-best-path lead-node goal path start-to-lead-node-dist)
          (consider-all-nodes lead-node goal path start-to-lead-node-dist))))

  (defun consider-best-path (lead-node goal path distance-to-here)
    (let* (
           (first-node (get-first-node-in-path lead-node goal))
           (dist-to-first (+ distance-to-here
                             (get-stored-dist lead-node first-node)))
           (total-estimate (+ distance-to-here
                              (get-stored-dist lead-node goal)))
           (new-path (cons first-node path)))
      (list (list total-estimate dist-to-first new-path))))

  (defun get-stored-path (start goal)
    (if (equal start goal)
        (list start 0)
        (assoc goal (get start 'path-list))))

  (defun node-not-in-path (node path)
    (if (null path)
        t
        (if (equal node (car path))
            nil
            (node-not-in-path node (cdr path)))))

  (defun consider-all-nodes (lead-node goal path start-to-lead-node-dist)
    (let (dist-to-first total-estimate new-path new-nodes)
      (dolist (node (collect-linked-nodes lead-node))
        (if (node-not-in-path node path)
            (let ()
              (setq dist-to-first (+ start-to-lead-node-dist
                                     (get-stored-dist lead-node node)))
              (setq total-estimate (+ dist-to-first
                                      (dist-between-points
                                       (eval node)
                                       (eval  goal))))
              (setq new-path (cons node path))
              (setq new-nodes (cons (list total-estimate
                                          dist-to-first
                                          new-path)
                                    new-nodes)))))
      new-nodes))

  (defun collect-linked-nodes (node)
    (let (links)
      (dolist (link (get node 'path-list))
        (if (null (first-on-path link))
            (setq links (cons (goal-node link) links))))
      links))

  (defun get-stored-dist (node1 node2)
    (distance-to-that-node (get-stored-path node1 node2)))

  (defun collect-ascending-search-list-order (a l)
    (if (null l)
        (list a)
        (if (< (car a) (caar l))
            (cons a l)
            (cons (car l) (Collect-ascending-search-list-order a (cdr l))))))

  (defun get-first-node-in-path (start goal)
    (let (first-node)
      (setq first-node  (first-on-path (get-stored-path start goal)))
      (if first-node first-node goal)))

  (defun a*-helper ()
    (if possible-paths
        (let ()
          (setq best (car possible-paths))
          (setq possible-paths (cdr possible-paths))
          (if (equal (first (caddr best)) goal)
              best
              (let ()
                (dolist (child (enumerate-children best goal))
                  (setq possible-paths
                        (collect-ascending-search-list-order
                         child possible-paths)))
                (a*-helper))))))
  (init-network)
  (reverse (caddr (a*-helper))))

;;      test code:

(defun test ()
  (defvar n1 '(30 201))
  (defvar  n2 '(25 140))
  (defvar  n3 '(55 30))
  (defvar  n4 '(105 190))
  (defvar  n5 '(95 110))
  (defvar  n6 '(140 22))
  (defvar  n7 '(160 150))
  (defvar  n8 '(170 202))
  (defvar  n9 '(189 130))
  (defvar  n10 '(200 55))
  (defvar  n11 '(205 201))

 (print (A*search
         '(n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11) ;; nodes
         '((n1 n2) (n2 n3) (n3 n5) (n3 n6) (n6 n10) ;; paths
           (n9 n10) (n7 n9) (n1 n4) (n4 n2) (n5 n8)
           (n8 n4) (n7 n11))
         'n1 'n11))) ;; starting and goal nodes
