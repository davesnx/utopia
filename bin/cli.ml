(* open Cmdliner

   let dev_term = Term.(const dev)

   let info =
     Cmd.info "query-json" ~doc:"Run operations on JSON"
       ~man:
         [
           `S Manpage.s_description;
           `P "query-json '.dependencies' package.json";
           `S Manpage.s_bugs;
         ]

   let cmd = Cmd.v info dev_term
   let () = Stdlib.exit (Cmd.eval cmd)
*)
