
type intref    = int ref
type aconstant = Absyn.aconstant
type akind     = Absyn.akind
	
val wordSize : int


val rSize : int
val eSize : int
val nSize : int
val i1Size : int
val ceSize : int
val segSize : int
val cSize : int
val kSize : int
val sSize : int
val mtSize : int
val itSize : int
val htSize : int
val bvtSize : int
val opcodeSize : int




type rtype = int
type etype = int
type ntype = int
type i1type = int
type cetype = int
type segtype = int
type ctype = aconstant
type ktype = akind
type ltype = intref
type itype = int
type ftype = float
type stype = int
type mttype = int
type ittype = int
type httype = int
type bvttype = int


type inscatRX = rtype
type inscatEX = etype
type inscatI1X = i1type
type inscatCX = ctype
type inscatKX = ktype
type inscatIX = itype
type inscatFX = ftype
type inscatSX = stype
type inscatMTX = mttype
type inscatLX = ltype
type inscatRRX = rtype * rtype
type inscatERX = etype * rtype
type inscatRCX = rtype * ctype
type inscatRIX = rtype * itype
type inscatRFX = rtype * ftype
type inscatRSX = rtype * stype
type inscatRI1X = rtype * i1type
type inscatRCEX = rtype * cetype
type inscatECEX = etype * cetype
type inscatCLX = ctype * ltype
type inscatRKX = rtype * ktype
type inscatECX = etype * ctype
type inscatI1ITX = i1type * ittype
type inscatI1LX = i1type * ltype
type inscatSEGLX = segtype * ltype
type inscatI1LWPX = i1type * ltype
type inscatI1NX = i1type * ntype
type inscatI1HTX = i1type * httype
type inscatI1BVTX = i1type * bvttype
type inscatCWPX = ctype
type inscatI1WPX = i1type
type inscatRRI1X = rtype * rtype * i1type
type inscatRCLX = rtype * ctype * ltype
type inscatRCI1X = rtype * ctype * i1type
type inscatSEGI1LX = segtype * i1type * ltype
type inscatI1LLX = i1type * ltype * ltype
type inscatNLLX = ntype * ltype * ltype
type inscatLLLLX = ltype * ltype * ltype * ltype
type inscatI1CWPX = i1type * ctype
type inscatI1I1WPX = i1type * i1type


type instruction = Ins_put_variable_t of inscatRRX
  | Ins_put_variable_p of inscatERX
  | Ins_put_value_t of inscatRRX
  | Ins_put_value_p of inscatERX
  | Ins_put_unsafe_value of inscatERX
  | Ins_copy_value of inscatERX
  | Ins_put_m_const of inscatRCX
  | Ins_put_p_const of inscatRCX
  | Ins_put_nil of inscatRX
  | Ins_put_integer of inscatRIX
  | Ins_put_float of inscatRFX
  | Ins_put_string of inscatRSX
  | Ins_put_index of inscatRI1X
  | Ins_put_app of inscatRRI1X
  | Ins_put_list of inscatRX
  | Ins_put_lambda of inscatRRI1X
  | Ins_set_variable_t of inscatRX
  | Ins_set_variable_te of inscatRX
  | Ins_set_variable_p of inscatEX
  | Ins_set_value_t of inscatRX
  | Ins_set_value_p of inscatEX
  | Ins_globalize_pt of inscatERX
  | Ins_globalize_t of inscatRX
  | Ins_set_m_const of inscatCX
  | Ins_set_p_const of inscatCX
  | Ins_set_nil
  | Ins_set_integer of inscatIX
  | Ins_set_float of inscatFX
  | Ins_set_string of inscatSX
  | Ins_set_index of inscatI1X
  | Ins_set_void of inscatI1X
  | Ins_deref of inscatRX
  | Ins_set_lambda of inscatRI1X
  | Ins_get_variable_t of inscatRRX
  | Ins_get_variable_p of inscatERX
  | Ins_init_variable_t of inscatRCEX
  | Ins_init_variable_p of inscatECEX
  | Ins_get_m_constant of inscatRCX
  | Ins_get_p_constant of inscatRCLX
  | Ins_get_integer of inscatRIX
  | Ins_get_float of inscatRFX
  | Ins_get_string of inscatRSX
  | Ins_get_nil of inscatRX
  | Ins_get_m_structure of inscatRCI1X
  | Ins_get_p_structure of inscatRCI1X
  | Ins_get_list of inscatRX
  | Ins_unify_variable_t of inscatRX
  | Ins_unify_variable_p of inscatEX
  | Ins_unify_value_t of inscatRX
  | Ins_unify_value_p of inscatEX
  | Ins_unify_local_value_t of inscatRX
  | Ins_unify_local_value_p of inscatEX
  | Ins_unify_m_constant of inscatCX
  | Ins_unify_p_constant of inscatCLX
  | Ins_unify_integer of inscatIX
  | Ins_unify_float of inscatFX
  | Ins_unify_string of inscatSX
  | Ins_unify_nil
  | Ins_unify_void of inscatI1X
  | Ins_put_type_variable_t of inscatRRX
  | Ins_put_type_variable_p of inscatERX
  | Ins_put_type_value_t of inscatRRX
  | Ins_put_type_value_p of inscatERX
  | Ins_put_type_unsafe_value of inscatERX
  | Ins_put_type_const of inscatRKX
  | Ins_put_type_structure of inscatRKX
  | Ins_put_type_arrow of inscatRX
  | Ins_set_type_variable_t of inscatRX
  | Ins_set_type_variable_p of inscatEX
  | Ins_set_type_value_t of inscatRX
  | Ins_set_type_value_p of inscatEX
  | Ins_set_type_local_value_t of inscatRX
  | Ins_set_type_local_value_p of inscatEX
  | Ins_set_type_constant of inscatKX
  | Ins_get_type_variable_t of inscatRRX
  | Ins_get_type_variable_p of inscatERX
  | Ins_init_type_variable_t of inscatRCEX
  | Ins_init_type_variable_p of inscatECEX
  | Ins_get_type_value_t of inscatRRX
  | Ins_get_type_value_p of inscatERX
  | Ins_get_type_constant of inscatRKX
  | Ins_get_type_structure of inscatRKX
  | Ins_get_type_arrow of inscatRX
  | Ins_unify_type_variable_t of inscatRX
  | Ins_unify_type_variable_p of inscatEX
  | Ins_unify_type_value_t of inscatRX
  | Ins_unify_type_value_p of inscatEX
  | Ins_unify_envty_value_t of inscatRX
  | Ins_unify_envty_value_p of inscatEX
  | Ins_unify_type_local_value_t of inscatRX
  | Ins_unify_type_local_value_p of inscatEX
  | Ins_unify_envty_local_value_t of inscatRX
  | Ins_unify_envty_local_value_p of inscatEX
  | Ins_unify_type_constant of inscatKX
  | Ins_pattern_unify_t of inscatRRX
  | Ins_pattern_unify_p of inscatERX
  | Ins_finish_unify
  | Ins_head_normalize_t of inscatRX
  | Ins_head_normalize_p of inscatEX
  | Ins_incr_universe
  | Ins_decr_universe
  | Ins_set_univ_tag of inscatECX
  | Ins_tag_exists_t of inscatRX
  | Ins_tag_exists_p of inscatEX
  | Ins_tag_variable of inscatEX
  | Ins_push_impl_point of inscatI1ITX
  | Ins_pop_impl_point
  | Ins_add_imports of inscatSEGI1LX
  | Ins_remove_imports of inscatSEGLX
  | Ins_push_import of inscatMTX
  | Ins_pop_imports of inscatI1X
  | Ins_allocate of inscatI1X
  | Ins_deallocate
  | Ins_call of inscatI1LX
  | Ins_call_name of inscatI1CWPX
  | Ins_execute of inscatLX
  | Ins_execute_name of inscatCWPX
  | Ins_proceed
  | Ins_try_me_else of inscatI1LX
  | Ins_retry_me_else of inscatI1LX
  | Ins_trust_me of inscatI1WPX
  | Ins_try of inscatI1LX
  | Ins_retry of inscatI1LX
  | Ins_trust of inscatI1LWPX
  | Ins_trust_ext of inscatI1NX
  | Ins_try_else of inscatI1LLX
  | Ins_retry_else of inscatI1LLX
  | Ins_branch of inscatLX
  | Ins_switch_on_term of inscatLLLLX
  | Ins_switch_on_constant of inscatI1HTX
  | Ins_switch_on_bvar of inscatI1BVTX
  | Ins_switch_on_reg of inscatNLLX
  | Ins_neck_cut
  | Ins_get_level of inscatEX
  | Ins_put_level of inscatEX
  | Ins_cut of inscatEX
  | Ins_call_builtin of inscatI1I1WPX
  | Ins_builtin of inscatI1X
  | Ins_stop
  | Ins_halt
  | Ins_fail
  | Ins_create_type_variable of inscatEX

val getSize_put_variable_t : int
val getSize_put_variable_p : int
val getSize_put_value_t : int
val getSize_put_value_p : int
val getSize_put_unsafe_value : int
val getSize_copy_value : int
val getSize_put_m_const : int
val getSize_put_p_const : int
val getSize_put_nil : int
val getSize_put_integer : int
val getSize_put_float : int
val getSize_put_string : int
val getSize_put_index : int
val getSize_put_app : int
val getSize_put_list : int
val getSize_put_lambda : int
val getSize_set_variable_t : int
val getSize_set_variable_te : int
val getSize_set_variable_p : int
val getSize_set_value_t : int
val getSize_set_value_p : int
val getSize_globalize_pt : int
val getSize_globalize_t : int
val getSize_set_m_const : int
val getSize_set_p_const : int
val getSize_set_nil : int
val getSize_set_integer : int
val getSize_set_float : int
val getSize_set_string : int
val getSize_set_index : int
val getSize_set_void : int
val getSize_deref : int
val getSize_set_lambda : int
val getSize_get_variable_t : int
val getSize_get_variable_p : int
val getSize_init_variable_t : int
val getSize_init_variable_p : int
val getSize_get_m_constant : int
val getSize_get_p_constant : int
val getSize_get_integer : int
val getSize_get_float : int
val getSize_get_string : int
val getSize_get_nil : int
val getSize_get_m_structure : int
val getSize_get_p_structure : int
val getSize_get_list : int
val getSize_unify_variable_t : int
val getSize_unify_variable_p : int
val getSize_unify_value_t : int
val getSize_unify_value_p : int
val getSize_unify_local_value_t : int
val getSize_unify_local_value_p : int
val getSize_unify_m_constant : int
val getSize_unify_p_constant : int
val getSize_unify_integer : int
val getSize_unify_float : int
val getSize_unify_string : int
val getSize_unify_nil : int
val getSize_unify_void : int
val getSize_put_type_variable_t : int
val getSize_put_type_variable_p : int
val getSize_put_type_value_t : int
val getSize_put_type_value_p : int
val getSize_put_type_unsafe_value : int
val getSize_put_type_const : int
val getSize_put_type_structure : int
val getSize_put_type_arrow : int
val getSize_set_type_variable_t : int
val getSize_set_type_variable_p : int
val getSize_set_type_value_t : int
val getSize_set_type_value_p : int
val getSize_set_type_local_value_t : int
val getSize_set_type_local_value_p : int
val getSize_set_type_constant : int
val getSize_get_type_variable_t : int
val getSize_get_type_variable_p : int
val getSize_init_type_variable_t : int
val getSize_init_type_variable_p : int
val getSize_get_type_value_t : int
val getSize_get_type_value_p : int
val getSize_get_type_constant : int
val getSize_get_type_structure : int
val getSize_get_type_arrow : int
val getSize_unify_type_variable_t : int
val getSize_unify_type_variable_p : int
val getSize_unify_type_value_t : int
val getSize_unify_type_value_p : int
val getSize_unify_envty_value_t : int
val getSize_unify_envty_value_p : int
val getSize_unify_type_local_value_t : int
val getSize_unify_type_local_value_p : int
val getSize_unify_envty_local_value_t : int
val getSize_unify_envty_local_value_p : int
val getSize_unify_type_constant : int
val getSize_pattern_unify_t : int
val getSize_pattern_unify_p : int
val getSize_finish_unify : int
val getSize_head_normalize_t : int
val getSize_head_normalize_p : int
val getSize_incr_universe : int
val getSize_decr_universe : int
val getSize_set_univ_tag : int
val getSize_tag_exists_t : int
val getSize_tag_exists_p : int
val getSize_tag_variable : int
val getSize_push_impl_point : int
val getSize_pop_impl_point : int
val getSize_add_imports : int
val getSize_remove_imports : int
val getSize_push_import : int
val getSize_pop_imports : int
val getSize_allocate : int
val getSize_deallocate : int
val getSize_call : int
val getSize_call_name : int
val getSize_execute : int
val getSize_execute_name : int
val getSize_proceed : int
val getSize_try_me_else : int
val getSize_retry_me_else : int
val getSize_trust_me : int
val getSize_try : int
val getSize_retry : int
val getSize_trust : int
val getSize_trust_ext : int
val getSize_try_else : int
val getSize_retry_else : int
val getSize_branch : int
val getSize_switch_on_term : int
val getSize_switch_on_constant : int
val getSize_switch_on_bvar : int
val getSize_switch_on_reg : int
val getSize_neck_cut : int
val getSize_get_level : int
val getSize_put_level : int
val getSize_cut : int
val getSize_call_builtin : int
val getSize_builtin : int
val getSize_stop : int
val getSize_halt : int
val getSize_fail : int
val getSize_create_type_variable : int


val writeInstruction : instruction -> unit


