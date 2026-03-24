  $ mkdir -p pages/client _utopia
  $ touch _utopia/dune
  $ printf "/* @utopia.script ./client/home_client.re */\nlet page = ()\n" > pages/Home.re
  $ printf "let run = ()\n" > pages/client/home_client.re
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
   (deps ../pages/client/home_client.re)
   (target Script__pages_client_home_client.re)
   (action
    (run cp %{deps} Script__pages_client_home_client.re)))
  
  (melange.emit
   (target target)
   (modules Home_melange Script__pages_client_home_client)
   (libraries reason-react)
   (preprocess
    (pps reason-react-ppx)))
  
  (library
   (name pages)
   (modules Home_native)
   (public_name utopia)
   (libraries server-reason-react.react server-reason-react.reactDom)
   (preprocess
    (pps server-reason-react.ppx)))
  
  $ cat _utopia/scripts.manifest
  home	target/Script__pages_client_home_client.js
