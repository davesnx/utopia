  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "# Hello\n" > pages/Guide.md
  $ utopia.compiler > /dev/null
  $ cat _utopia/dune
  (rule
   (deps ../pages/Home.re)
   (targets Home_melange.re Home_native.re)
   (action
    (progn
     (run cp %{deps} Home_melange.re)
     (run cp %{deps} Home_native.re))))
  
  (melange.emit
   (target target)
   (modules Home_melange)
   (libraries reason-react)
   (preprocess
    (pps reason-react-ppx)))
  
  (rule
   (deps ../pages/Guide.md)
   (target Guide.html)
   (action
    (with-stdout-to %{target}
    (with-stdin-from %{deps}
     (run %{bin:utopia.markdown})))))
  
  (library
   (name pages)
   (modules Home_native)
   (public_name utopia)
   (libraries server-reason-react.react server-reason-react.reactDom)
   (preprocess
    (pps server-reason-react.ppx)))
  
