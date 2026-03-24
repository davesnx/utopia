  $ mkdir -p pages/lib _utopia
  $ touch _utopia/dune
  $ printf "let page = Utils.value\n" > pages/Home.re
  $ printf "let value = 1\n" > pages/lib/Utils.re
  $ utopia.compiler > /dev/null
  $ cat _utopia/dune
  (rule
   (deps ../pages/Home.re)
   (targets Home_melange.re Home_native.re)
   (action
    (progn
     (run cp %{deps} Home_melange.re)
     (run cp %{deps} Home_native.re))))
  
  (rule
   (deps ../pages/lib/Utils.re)
   (targets Lib__Utils_melange.re Lib__Utils_native.re)
   (action
    (progn
     (run cp %{deps} Lib__Utils_melange.re)
     (run cp %{deps} Lib__Utils_native.re))))
  
  (rule
   (target Lib_melange.re)
   (action
    (write-file %{target} "module Utils = Lib__Utils_melange")))
  
  (rule
   (target Lib_native.re)
   (action
    (write-file %{target} "module Utils = Lib__Utils_native")))
  
  (melange.emit
   (target target)
   (modules Lib_melange Lib__Utils_melange Home_melange)
   (libraries reason-react)
   (flags (:standard -open Lib_melange))
   (preprocess
    (pps reason-react-ppx)))
  
  (library
   (name pages)
   (modules Lib_native Lib__Utils_native Home_native)
   (public_name utopia)
   (libraries server-reason-react.react server-reason-react.reactDom)
   (flags (:standard -open Lib_native))
   (preprocess
    (pps server-reason-react.ppx)))
  
  $ cat _utopia/routes.manifest
  home	code	pages/Home.re	home		
