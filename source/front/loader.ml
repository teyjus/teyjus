let heapSize = ref 0 
let path  = ref "./"

(** Use the Teyjus loader to load the bytecode file
    and set up the simulator state. 
    Followed from simulatorfront main.*)
let load_module name =
  Front.systemInit  !heapSize ;
  Module.setPath    !path;
  Module.moduleLoad name;
  Front.simulatorInit () ;
  Module.moduleInstall name ;
  Module.initModuleContext () ;
  Module.getCurrentModule ()

(** Read the mapping between LF and LP constants
    from the metadata file. *)
let load_metadata name =
  let metadata = ref Metadata.empty in
  let in_chann = open_in (name ^ ".md") in
  let _ =
    try
      while true do
        let line = input_line in_chann in
        match Str.split (Str.regexp " ") line  with
            [fst; snd] ->
              metadata := Metadata.set_mapping (!metadata) (Symb.symbol fst) (Symbol.symbol snd)
          | _ -> ()
      done
    with End_of_file -> close_in in_chann
  in !metadata

let load name =
  (load_module name, load_metadata name)
  
