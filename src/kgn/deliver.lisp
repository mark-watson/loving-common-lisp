;; LispWorks, to be run from command line (not multiprocessing):
;;
;;  lispworks -build deliver.lisp

(in-package "CL-USER")

(load-all-patches)
(load "~/quicklisp/setup.lisp")
(ql:quickload "kgn")

#|

(deliver 'kgn:kgn 
         #+:cocoa 
         (create-macos-application-bundle
          "~/KnowledgeGraphNavigator.app"
          ;; Do not copy file associations...
          :document-types nil
          ;; ...or CFBundleIdentifier from the LispWorks bundle
          :identifier "com.markwatson.kgn"
          )
         #-:cocoa "~/hello" 
         0 
         :interface :capi
         :startup-bitmap-file nil
         ;;:template-bundle (pathname-location
         ;;                  (current-pathname "templates/KnowledgeGraphNavigator.app/"))
         :KEEP-PRETTY-PRINTER t
         :split :resources)



|#

(defvar *target-application-path* "~/KnowledgeGraphNavigator.app")

(deliver 'kgn:kgn
         (create-macos-application-bundle
          *target-application-path*
          :template-bundle (pathname-location
                            (current-pathname "templates/KnowledgeGraphNavigator.app/")))
         0
         :interface :capi
         :KEEP-PRETTY-PRINTER t
         :split :resources
         :startup-bitmap-file nil
)

