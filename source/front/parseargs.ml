let dualArgs dualSpecList =
  let seperate (key1, key2, spec, doc) =
    [(key1, spec, doc) ; (key2, spec, doc)]
  in
    List.flatten (List.map seperate dualSpecList)
    
let printVersion () =
  print_string "version number:... \n"

let versionspec =
  ("-v", "--version", Arg.Unit printVersion, " Return the system version")

let getModName name =
  try
    Filename.chop_extension name
  with
    | Invalid_argument _ -> name

let error str =
  prerr_endline ("Error : " ^ str) ;
  exit 1
