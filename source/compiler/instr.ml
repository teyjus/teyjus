let wordSize = 8


let rSize = 255
let eSize = 255
let nSize = 255
let i1Size = 255
let ceSize = 255
let segSize = 255
let cSize = 65535
let kSize = 65535
let sSize = 65535
let mtSize = 65535
let itSize = 65535
let htSize = 65535
let bvtSize = 65535
let opcodeSize = 255




let writeR arg = Writeutil.writeint1 arg
let writeE arg = Writeutil.writeint1 arg
let writeN arg = Writeutil.writeint1 arg
let writeI1 arg = Writeutil.writeint1 arg
let writeCE arg = Writeutil.writeint1 arg
let writeSEG arg = Writeutil.writeint1 arg
let writeC arg = Writeutil.writeint2 arg
let writeK arg = Writeutil.writeint2 arg
let writeL arg = Writeutil.writeintref8 arg
let writeI arg = Writeutil.writeint4 arg
let writeF arg = Writeutil.writefloat4 arg
let writeS arg = Writeutil.writeint2 arg
let writeMT arg = Writeutil.writeint2 arg
let writeIT arg = Writeutil.writeint2 arg
let writeHT arg = Writeutil.writeint2 arg
let writeBVT arg = Writeutil.writeint2 arg
let writeopcode arg = Writeutil.writeint1 arg




type intref = int ref
type rtype = int
type etype = int
type ntype = int
type i1type = int
type cetype = int
type segtype = int
type ctype = int
type ktype = int
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



let writeRX (arg1) = writeR arg1
let writeEX (arg1) = writeE arg1
let writeI1X (arg1) = writeI1 arg1
let writeCX (arg1) = writeC arg1
let writeKX (arg1) = writeK arg1
let writeIX (arg1) = writeI arg1
let writeFX (arg1) = writeF arg1
let writeSX (arg1) = writeS arg1
let writeMTX (arg1) = writeMT arg1
let writeLX (arg1) = writeL arg1
let writeRRX (arg1, arg2) = writeR arg1; writeR arg2
let writeERX (arg1, arg2) = writeE arg1; writeR arg2
let writeRCX (arg1, arg2) = writeR arg1; writeC arg2
let writeRIX (arg1, arg2) = writeR arg1; writeI arg2
let writeRFX (arg1, arg2) = writeR arg1; writeF arg2
let writeRSX (arg1, arg2) = writeR arg1; writeS arg2
let writeRI1X (arg1, arg2) = writeR arg1; writeI1 arg2
let writeRCEX (arg1, arg2) = writeR arg1; writeCE arg2
let writeECEX (arg1, arg2) = writeE arg1; writeCE arg2
let writeCLX (arg1, arg2) = writeC arg1; writeL arg2
let writeRKX (arg1, arg2) = writeR arg1; writeK arg2
let writeECX (arg1, arg2) = writeE arg1; writeC arg2
let writeI1ITX (arg1, arg2) = writeI1 arg1; writeIT arg2
let writeI1LX (arg1, arg2) = writeI1 arg1; writeL arg2
let writeSEGLX (arg1, arg2) = writeSEG arg1; writeL arg2
let writeI1LWPX (arg1, arg2) = writeI1 arg1; writeL arg2
let writeI1NX (arg1, arg2) = writeI1 arg1; writeN arg2
let writeI1HTX (arg1, arg2) = writeI1 arg1; writeHT arg2
let writeI1BVTX (arg1, arg2) = writeI1 arg1; writeBVT arg2
let writeCWPX (arg1) = writeC arg1
let writeI1WPX (arg1) = writeI1 arg1
let writeRRI1X (arg1, arg2, arg3) = writeR arg1; writeR arg2; writeI1 arg3
let writeRCLX (arg1, arg2, arg3) = writeR arg1; writeC arg2; writeL arg3
let writeRCI1X (arg1, arg2, arg3) = writeR arg1; writeC arg2; writeI1 arg3
let writeSEGI1LX (arg1, arg2, arg3) = writeSEG arg1; writeI1 arg2; writeL arg3
let writeI1LLX (arg1, arg2, arg3) = writeI1 arg1; writeL arg2; writeL arg3
let writeNLLX (arg1, arg2, arg3) = writeN arg1; writeL arg2; writeL arg3
let writeLLLLX (arg1, arg2, arg3, arg4) = writeL arg1; writeL arg2; writeL arg3; writeL arg4
let writeI1CWPX (arg1, arg2) = writeI1 arg1; writeC arg2
let writeI1I1WPX (arg1, arg2) = writeI1 arg1; writeI1 arg2


let inscatX_LEN = 8
let inscatRX_LEN = 8
let inscatEX_LEN = 8
let inscatI1X_LEN = 8
let inscatCX_LEN = 8
let inscatKX_LEN = 8
let inscatIX_LEN = 16
let inscatFX_LEN = 16
let inscatSX_LEN = 16
let inscatMTX_LEN = 16
let inscatLX_LEN = 16
let inscatRRX_LEN = 8
let inscatERX_LEN = 8
let inscatRCX_LEN = 8
let inscatRIX_LEN = 16
let inscatRFX_LEN = 16
let inscatRSX_LEN = 16
let inscatRI1X_LEN = 8
let inscatRCEX_LEN = 8
let inscatECEX_LEN = 8
let inscatCLX_LEN = 16
let inscatRKX_LEN = 8
let inscatECX_LEN = 8
let inscatI1ITX_LEN = 16
let inscatI1LX_LEN = 16
let inscatSEGLX_LEN = 16
let inscatI1LWPX_LEN = 24
let inscatI1NX_LEN = 8
let inscatI1HTX_LEN = 16
let inscatI1BVTX_LEN = 16
let inscatCWPX_LEN = 16
let inscatI1WPX_LEN = 16
let inscatRRI1X_LEN = 8
let inscatRCLX_LEN = 16
let inscatRCI1X_LEN = 8
let inscatSEGI1LX_LEN = 16
let inscatI1LLX_LEN = 24
let inscatNLLX_LEN = 24
let inscatLLLLX_LEN = 40
let inscatI1CWPX_LEN = 16
let inscatI1I1WPX_LEN = 16


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

let getSize_put_variable_t = inscatRRX_LEN
let getSize_put_variable_p = inscatERX_LEN
let getSize_put_value_t = inscatRRX_LEN
let getSize_put_value_p = inscatERX_LEN
let getSize_put_unsafe_value = inscatERX_LEN
let getSize_copy_value = inscatERX_LEN
let getSize_put_m_const = inscatRCX_LEN
let getSize_put_p_const = inscatRCX_LEN
let getSize_put_nil = inscatRX_LEN
let getSize_put_integer = inscatRIX_LEN
let getSize_put_float = inscatRFX_LEN
let getSize_put_string = inscatRSX_LEN
let getSize_put_index = inscatRI1X_LEN
let getSize_put_app = inscatRRI1X_LEN
let getSize_put_list = inscatRX_LEN
let getSize_put_lambda = inscatRRI1X_LEN
let getSize_set_variable_t = inscatRX_LEN
let getSize_set_variable_te = inscatRX_LEN
let getSize_set_variable_p = inscatEX_LEN
let getSize_set_value_t = inscatRX_LEN
let getSize_set_value_p = inscatEX_LEN
let getSize_globalize_pt = inscatERX_LEN
let getSize_globalize_t = inscatRX_LEN
let getSize_set_m_const = inscatCX_LEN
let getSize_set_p_const = inscatCX_LEN
let getSize_set_nil = inscatX_LEN
let getSize_set_integer = inscatIX_LEN
let getSize_set_float = inscatFX_LEN
let getSize_set_string = inscatSX_LEN
let getSize_set_index = inscatI1X_LEN
let getSize_set_void = inscatI1X_LEN
let getSize_deref = inscatRX_LEN
let getSize_set_lambda = inscatRI1X_LEN
let getSize_get_variable_t = inscatRRX_LEN
let getSize_get_variable_p = inscatERX_LEN
let getSize_init_variable_t = inscatRCEX_LEN
let getSize_init_variable_p = inscatECEX_LEN
let getSize_get_m_constant = inscatRCX_LEN
let getSize_get_p_constant = inscatRCLX_LEN
let getSize_get_integer = inscatRIX_LEN
let getSize_get_float = inscatRFX_LEN
let getSize_get_string = inscatRSX_LEN
let getSize_get_nil = inscatRX_LEN
let getSize_get_m_structure = inscatRCI1X_LEN
let getSize_get_p_structure = inscatRCI1X_LEN
let getSize_get_list = inscatRX_LEN
let getSize_unify_variable_t = inscatRX_LEN
let getSize_unify_variable_p = inscatEX_LEN
let getSize_unify_value_t = inscatRX_LEN
let getSize_unify_value_p = inscatEX_LEN
let getSize_unify_local_value_t = inscatRX_LEN
let getSize_unify_local_value_p = inscatEX_LEN
let getSize_unify_m_constant = inscatCX_LEN
let getSize_unify_p_constant = inscatCLX_LEN
let getSize_unify_integer = inscatIX_LEN
let getSize_unify_float = inscatFX_LEN
let getSize_unify_string = inscatSX_LEN
let getSize_unify_nil = inscatX_LEN
let getSize_unify_void = inscatI1X_LEN
let getSize_put_type_variable_t = inscatRRX_LEN
let getSize_put_type_variable_p = inscatERX_LEN
let getSize_put_type_value_t = inscatRRX_LEN
let getSize_put_type_value_p = inscatERX_LEN
let getSize_put_type_unsafe_value = inscatERX_LEN
let getSize_put_type_const = inscatRKX_LEN
let getSize_put_type_structure = inscatRKX_LEN
let getSize_put_type_arrow = inscatRX_LEN
let getSize_set_type_variable_t = inscatRX_LEN
let getSize_set_type_variable_p = inscatEX_LEN
let getSize_set_type_value_t = inscatRX_LEN
let getSize_set_type_value_p = inscatEX_LEN
let getSize_set_type_local_value_t = inscatRX_LEN
let getSize_set_type_local_value_p = inscatEX_LEN
let getSize_set_type_constant = inscatKX_LEN
let getSize_get_type_variable_t = inscatRRX_LEN
let getSize_get_type_variable_p = inscatERX_LEN
let getSize_init_type_variable_t = inscatRCEX_LEN
let getSize_init_type_variable_p = inscatECEX_LEN
let getSize_get_type_value_t = inscatRRX_LEN
let getSize_get_type_value_p = inscatERX_LEN
let getSize_get_type_constant = inscatRKX_LEN
let getSize_get_type_structure = inscatRKX_LEN
let getSize_get_type_arrow = inscatRX_LEN
let getSize_unify_type_variable_t = inscatRX_LEN
let getSize_unify_type_variable_p = inscatEX_LEN
let getSize_unify_type_value_t = inscatRX_LEN
let getSize_unify_type_value_p = inscatEX_LEN
let getSize_unify_envty_value_t = inscatRX_LEN
let getSize_unify_envty_value_p = inscatEX_LEN
let getSize_unify_type_local_value_t = inscatRX_LEN
let getSize_unify_type_local_value_p = inscatEX_LEN
let getSize_unify_envty_local_value_t = inscatRX_LEN
let getSize_unify_envty_local_value_p = inscatEX_LEN
let getSize_unify_type_constant = inscatKX_LEN
let getSize_pattern_unify_t = inscatRRX_LEN
let getSize_pattern_unify_p = inscatERX_LEN
let getSize_finish_unify = inscatX_LEN
let getSize_head_normalize_t = inscatRX_LEN
let getSize_head_normalize_p = inscatEX_LEN
let getSize_incr_universe = inscatX_LEN
let getSize_decr_universe = inscatX_LEN
let getSize_set_univ_tag = inscatECX_LEN
let getSize_tag_exists_t = inscatRX_LEN
let getSize_tag_exists_p = inscatEX_LEN
let getSize_tag_variable = inscatEX_LEN
let getSize_push_impl_point = inscatI1ITX_LEN
let getSize_pop_impl_point = inscatX_LEN
let getSize_add_imports = inscatSEGI1LX_LEN
let getSize_remove_imports = inscatSEGLX_LEN
let getSize_push_import = inscatMTX_LEN
let getSize_pop_imports = inscatI1X_LEN
let getSize_allocate = inscatI1X_LEN
let getSize_deallocate = inscatX_LEN
let getSize_call = inscatI1LX_LEN
let getSize_call_name = inscatI1CWPX_LEN
let getSize_execute = inscatLX_LEN
let getSize_execute_name = inscatCWPX_LEN
let getSize_proceed = inscatX_LEN
let getSize_try_me_else = inscatI1LX_LEN
let getSize_retry_me_else = inscatI1LX_LEN
let getSize_trust_me = inscatI1WPX_LEN
let getSize_try = inscatI1LX_LEN
let getSize_retry = inscatI1LX_LEN
let getSize_trust = inscatI1LWPX_LEN
let getSize_trust_ext = inscatI1NX_LEN
let getSize_try_else = inscatI1LLX_LEN
let getSize_retry_else = inscatI1LLX_LEN
let getSize_branch = inscatLX_LEN
let getSize_switch_on_term = inscatLLLLX_LEN
let getSize_switch_on_constant = inscatI1HTX_LEN
let getSize_switch_on_bvar = inscatI1BVTX_LEN
let getSize_switch_on_reg = inscatNLLX_LEN
let getSize_neck_cut = inscatX_LEN
let getSize_get_level = inscatEX_LEN
let getSize_put_level = inscatEX_LEN
let getSize_cut = inscatEX_LEN
let getSize_call_builtin = inscatI1I1WPX_LEN
let getSize_builtin = inscatI1X_LEN
let getSize_stop = inscatX_LEN
let getSize_halt = inscatX_LEN
let getSize_fail = inscatX_LEN


let writeInstruction inst =
  match inst with
    Ins_put_variable_t(arg) -> writeopcode 0; writeRRX arg
  | Ins_put_variable_p(arg) -> writeopcode 1; writeERX arg
  | Ins_put_value_t(arg) -> writeopcode 2; writeRRX arg
  | Ins_put_value_p(arg) -> writeopcode 3; writeERX arg
  | Ins_put_unsafe_value(arg) -> writeopcode 4; writeERX arg
  | Ins_copy_value(arg) -> writeopcode 5; writeERX arg
  | Ins_put_m_const(arg) -> writeopcode 6; writeRCX arg
  | Ins_put_p_const(arg) -> writeopcode 7; writeRCX arg
  | Ins_put_nil(arg) -> writeopcode 8; writeRX arg
  | Ins_put_integer(arg) -> writeopcode 9; writeRIX arg
  | Ins_put_float(arg) -> writeopcode 10; writeRFX arg
  | Ins_put_string(arg) -> writeopcode 11; writeRSX arg
  | Ins_put_index(arg) -> writeopcode 12; writeRI1X arg
  | Ins_put_app(arg) -> writeopcode 13; writeRRI1X arg
  | Ins_put_list(arg) -> writeopcode 14; writeRX arg
  | Ins_put_lambda(arg) -> writeopcode 15; writeRRI1X arg
  | Ins_set_variable_t(arg) -> writeopcode 16; writeRX arg
  | Ins_set_variable_te(arg) -> writeopcode 17; writeRX arg
  | Ins_set_variable_p(arg) -> writeopcode 18; writeEX arg
  | Ins_set_value_t(arg) -> writeopcode 19; writeRX arg
  | Ins_set_value_p(arg) -> writeopcode 20; writeEX arg
  | Ins_globalize_pt(arg) -> writeopcode 21; writeERX arg
  | Ins_globalize_t(arg) -> writeopcode 22; writeRX arg
  | Ins_set_m_const(arg) -> writeopcode 23; writeCX arg
  | Ins_set_p_const(arg) -> writeopcode 24; writeCX arg
  | Ins_set_nil -> writeopcode 25
  | Ins_set_integer(arg) -> writeopcode 26; writeIX arg
  | Ins_set_float(arg) -> writeopcode 27; writeFX arg
  | Ins_set_string(arg) -> writeopcode 28; writeSX arg
  | Ins_set_index(arg) -> writeopcode 29; writeI1X arg
  | Ins_set_void(arg) -> writeopcode 30; writeI1X arg
  | Ins_deref(arg) -> writeopcode 31; writeRX arg
  | Ins_set_lambda(arg) -> writeopcode 32; writeRI1X arg
  | Ins_get_variable_t(arg) -> writeopcode 33; writeRRX arg
  | Ins_get_variable_p(arg) -> writeopcode 34; writeERX arg
  | Ins_init_variable_t(arg) -> writeopcode 35; writeRCEX arg
  | Ins_init_variable_p(arg) -> writeopcode 36; writeECEX arg
  | Ins_get_m_constant(arg) -> writeopcode 37; writeRCX arg
  | Ins_get_p_constant(arg) -> writeopcode 38; writeRCLX arg
  | Ins_get_integer(arg) -> writeopcode 39; writeRIX arg
  | Ins_get_float(arg) -> writeopcode 40; writeRFX arg
  | Ins_get_string(arg) -> writeopcode 41; writeRSX arg
  | Ins_get_nil(arg) -> writeopcode 42; writeRX arg
  | Ins_get_m_structure(arg) -> writeopcode 43; writeRCI1X arg
  | Ins_get_p_structure(arg) -> writeopcode 44; writeRCI1X arg
  | Ins_get_list(arg) -> writeopcode 45; writeRX arg
  | Ins_unify_variable_t(arg) -> writeopcode 46; writeRX arg
  | Ins_unify_variable_p(arg) -> writeopcode 47; writeEX arg
  | Ins_unify_value_t(arg) -> writeopcode 48; writeRX arg
  | Ins_unify_value_p(arg) -> writeopcode 49; writeEX arg
  | Ins_unify_local_value_t(arg) -> writeopcode 50; writeRX arg
  | Ins_unify_local_value_p(arg) -> writeopcode 51; writeEX arg
  | Ins_unify_m_constant(arg) -> writeopcode 52; writeCX arg
  | Ins_unify_p_constant(arg) -> writeopcode 53; writeCLX arg
  | Ins_unify_integer(arg) -> writeopcode 54; writeIX arg
  | Ins_unify_float(arg) -> writeopcode 55; writeFX arg
  | Ins_unify_string(arg) -> writeopcode 56; writeSX arg
  | Ins_unify_nil -> writeopcode 57
  | Ins_unify_void(arg) -> writeopcode 58; writeI1X arg
  | Ins_put_type_variable_t(arg) -> writeopcode 59; writeRRX arg
  | Ins_put_type_variable_p(arg) -> writeopcode 60; writeERX arg
  | Ins_put_type_value_t(arg) -> writeopcode 61; writeRRX arg
  | Ins_put_type_value_p(arg) -> writeopcode 62; writeERX arg
  | Ins_put_type_unsafe_value(arg) -> writeopcode 63; writeERX arg
  | Ins_put_type_const(arg) -> writeopcode 64; writeRKX arg
  | Ins_put_type_structure(arg) -> writeopcode 65; writeRKX arg
  | Ins_put_type_arrow(arg) -> writeopcode 66; writeRX arg
  | Ins_set_type_variable_t(arg) -> writeopcode 67; writeRX arg
  | Ins_set_type_variable_p(arg) -> writeopcode 68; writeEX arg
  | Ins_set_type_value_t(arg) -> writeopcode 69; writeRX arg
  | Ins_set_type_value_p(arg) -> writeopcode 70; writeEX arg
  | Ins_set_type_local_value_t(arg) -> writeopcode 71; writeRX arg
  | Ins_set_type_local_value_p(arg) -> writeopcode 72; writeEX arg
  | Ins_set_type_constant(arg) -> writeopcode 73; writeKX arg
  | Ins_get_type_variable_t(arg) -> writeopcode 74; writeRRX arg
  | Ins_get_type_variable_p(arg) -> writeopcode 75; writeERX arg
  | Ins_init_type_variable_t(arg) -> writeopcode 76; writeRCEX arg
  | Ins_init_type_variable_p(arg) -> writeopcode 77; writeECEX arg
  | Ins_get_type_value_t(arg) -> writeopcode 78; writeRRX arg
  | Ins_get_type_value_p(arg) -> writeopcode 79; writeERX arg
  | Ins_get_type_constant(arg) -> writeopcode 80; writeRKX arg
  | Ins_get_type_structure(arg) -> writeopcode 81; writeRKX arg
  | Ins_get_type_arrow(arg) -> writeopcode 82; writeRX arg
  | Ins_unify_type_variable_t(arg) -> writeopcode 83; writeRX arg
  | Ins_unify_type_variable_p(arg) -> writeopcode 84; writeEX arg
  | Ins_unify_type_value_t(arg) -> writeopcode 85; writeRX arg
  | Ins_unify_type_value_p(arg) -> writeopcode 86; writeEX arg
  | Ins_unify_envty_value_t(arg) -> writeopcode 87; writeRX arg
  | Ins_unify_envty_value_p(arg) -> writeopcode 88; writeEX arg
  | Ins_unify_type_local_value_t(arg) -> writeopcode 89; writeRX arg
  | Ins_unify_type_local_value_p(arg) -> writeopcode 90; writeEX arg
  | Ins_unify_envty_local_value_t(arg) -> writeopcode 91; writeRX arg
  | Ins_unify_envty_local_value_p(arg) -> writeopcode 92; writeEX arg
  | Ins_unify_type_constant(arg) -> writeopcode 93; writeKX arg
  | Ins_pattern_unify_t(arg) -> writeopcode 94; writeRRX arg
  | Ins_pattern_unify_p(arg) -> writeopcode 95; writeERX arg
  | Ins_finish_unify -> writeopcode 96
  | Ins_head_normalize_t(arg) -> writeopcode 97; writeRX arg
  | Ins_head_normalize_p(arg) -> writeopcode 98; writeEX arg
  | Ins_incr_universe -> writeopcode 99
  | Ins_decr_universe -> writeopcode 100
  | Ins_set_univ_tag(arg) -> writeopcode 101; writeECX arg
  | Ins_tag_exists_t(arg) -> writeopcode 102; writeRX arg
  | Ins_tag_exists_p(arg) -> writeopcode 103; writeEX arg
  | Ins_tag_variable(arg) -> writeopcode 104; writeEX arg
  | Ins_push_impl_point(arg) -> writeopcode 105; writeI1ITX arg
  | Ins_pop_impl_point -> writeopcode 106
  | Ins_add_imports(arg) -> writeopcode 107; writeSEGI1LX arg
  | Ins_remove_imports(arg) -> writeopcode 108; writeSEGLX arg
  | Ins_push_import(arg) -> writeopcode 109; writeMTX arg
  | Ins_pop_imports(arg) -> writeopcode 110; writeI1X arg
  | Ins_allocate(arg) -> writeopcode 111; writeI1X arg
  | Ins_deallocate -> writeopcode 112
  | Ins_call(arg) -> writeopcode 113; writeI1LX arg
  | Ins_call_name(arg) -> writeopcode 114; writeI1CWPX arg
  | Ins_execute(arg) -> writeopcode 115; writeLX arg
  | Ins_execute_name(arg) -> writeopcode 116; writeCWPX arg
  | Ins_proceed -> writeopcode 117
  | Ins_try_me_else(arg) -> writeopcode 118; writeI1LX arg
  | Ins_retry_me_else(arg) -> writeopcode 119; writeI1LX arg
  | Ins_trust_me(arg) -> writeopcode 120; writeI1WPX arg
  | Ins_try(arg) -> writeopcode 121; writeI1LX arg
  | Ins_retry(arg) -> writeopcode 122; writeI1LX arg
  | Ins_trust(arg) -> writeopcode 123; writeI1LWPX arg
  | Ins_trust_ext(arg) -> writeopcode 124; writeI1NX arg
  | Ins_try_else(arg) -> writeopcode 125; writeI1LLX arg
  | Ins_retry_else(arg) -> writeopcode 126; writeI1LLX arg
  | Ins_branch(arg) -> writeopcode 127; writeLX arg
  | Ins_switch_on_term(arg) -> writeopcode 128; writeLLLLX arg
  | Ins_switch_on_constant(arg) -> writeopcode 129; writeI1HTX arg
  | Ins_switch_on_bvar(arg) -> writeopcode 130; writeI1BVTX arg
  | Ins_switch_on_reg(arg) -> writeopcode 131; writeNLLX arg
  | Ins_neck_cut -> writeopcode 132
  | Ins_get_level(arg) -> writeopcode 133; writeEX arg
  | Ins_put_level(arg) -> writeopcode 134; writeEX arg
  | Ins_cut(arg) -> writeopcode 135; writeEX arg
  | Ins_call_builtin(arg) -> writeopcode 136; writeI1I1WPX arg
  | Ins_builtin(arg) -> writeopcode 137; writeI1X arg
  | Ins_stop -> writeopcode 138
  | Ins_halt -> writeopcode 139
  | Ins_fail -> writeopcode 140