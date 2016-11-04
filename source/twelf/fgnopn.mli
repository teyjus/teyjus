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
				        with type result = M.result)
