This is the stub README.txt for the "kgn" project.

try:

        (ql:quickload "kgn")
        (kgn:kgn)

create application using LispWorks:

must use non-multiprocessing command line:

    /Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin -build deliver.lisp


(deliver 'kgn:kgn 
         #+:cocoa 
         (create-macos-application-bundle
          "~/KGN.app"
          ;; Do not copy file associations...
          :document-types nil
          ;; ...or CFBundleIdentifier from the LispWorks bundle
          :identifier "com.example.Hello"
          )
         #-:cocoa "~/hello" 
         0 
         :interface :capi
         :KEEP-PRETTY-PRINTER t
         :split t)

CREDIT for app ICON: Deleket (Jojo Mendoza) https://findicons.com/icon/25039/network_mac_01

For matching example: who is <name>, <name>, etc. -> (entity_lookup EntityTypes.PERSON <name>)

reference for Pattern Matching and destructuring: https://lispcookbook.github.io/cl-cookbook/pattern_matching.html
  use: (ql:quickload "trivia") https://github.com/guicho271828/trivia/wiki/What-is-pattern-matching%3F-Benefits%3F
