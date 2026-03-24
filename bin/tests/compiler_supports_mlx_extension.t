  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.mlx
  $ utopia.compiler > /dev/null
  $ cat _utopia/dune
  (rule
   (deps ../pages/Home.mlx)
   (targets Home_melange.mlx Home_native.mlx)
   (action
    (progn
     (run cp %{deps} Home_melange.mlx)
     (run cp %{deps} Home_native.mlx))))
  
  (melange.emit
   (target target)
   (modules Home_melange)
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
  
