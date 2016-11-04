module type FGN_OPN = sig
  type csid = int
  type rep = exn
  type arg
  type result

  type func = rep -> arg -> result

  val install : csid * func -> unit

  val apply : csid * rep -> arg -> result
end

			
module FgnOpnTable (M : sig
			  type arg
			  type result
			end) : (FGN_OPN with type arg = M.arg
				with type result = M.result) =
struct
  type csid = int
  type rep = exn
  type arg = M.arg
  type result = M.result
  type func = (rep -> arg -> result)

  type table = func array 

  exception CSfunNotInstalled of csid
		    
  let initializeTable tbl =   
    let maxCSid = (*Global.maxCSid*) 50 in
    let unimplemented csid = fun _ -> raise (CSfunNotInstalled csid) in
    Array.init (maxCSid +1)  unimplemented

  let table : table = initializeTable ()

  let install (csid, f) = Array.set table csid f

  let apply (csid, rep) = (Array.get table csid) rep
end
