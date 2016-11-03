let default = print_string
let messageFunc = ref (default)
let setMessageFunc f = messageFunc := f
let message s = (!messageFunc) s
