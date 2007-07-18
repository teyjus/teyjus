external systemInit : int -> unit = "c_systemInit"
external simulatorInit : unit -> unit = "c_simulatorInit"
external topModuleInstall : unit -> unit = "c_topModuleInstall"
external moduleInstall : int -> unit = "c_moduleInstall"

external test : unit -> unit = "c_test"
