
type intref    = int ref
type aconstant = Absyn.aconstant
type akind     = Absyn.akind	

let wordSize = 4


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




let writeR arg = Bytecode.writeint1 arg
let writeE arg = Bytecode.writeint1 arg
let writeN arg = Bytecode.writeint1 arg
let writeI1 arg = Bytecode.writeint1 arg
let writeCE arg = Bytecode.writeint1 arg
let writeSEG arg = Bytecode.writeint1 arg
let writeC arg = Bytecode.writeaconstant2 arg
let writeK arg = Bytecode.writeakind2 arg
let writeL arg = Bytecode.writeintref4 arg
let writeI arg = Bytecode.writeint4 arg
let writeF arg = Bytecode.writefloat4 arg
let writeS arg = Bytecode.writeint2 arg
let writeMT arg = Bytecode.writeint2 arg
let writeIT arg = Bytecode.writeint2 arg
let writeHT arg = Bytecode.writeint2 arg
let writeBVT arg = Bytecode.writeint2 arg
let writeopcode arg = Bytecode.writeint1 arg




let readR () = Bytecode.readint1 ()
let readE () = Bytecode.readint1 ()
let readN () = Bytecode.readint1 ()
let readI1 () = Bytecode.readint1 ()
let readCE () = Bytecode.readint1 ()
let readSEG () = Bytecode.readint1 ()
let readC () = Bytecode.readaconstant2 ()
let readK () = Bytecode.readakind2 ()
let readL () = Bytecode.readintref4 ()
let readI () = Bytecode.readint4 ()
let readF () = Bytecode.readfloat4 ()
let readS () = Bytecode.readint2 ()
let readMT () = Bytecode.readint2 ()
let readIT () = Bytecode.readint2 ()
let readHT () = Bytecode.readint2 ()
let readBVT () = Bytecode.readint2 ()
let readopcode () = Bytecode.readint1 ()




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


let readRX () = 
  let arg1 = readR () in
  (arg1)
let readEX () = 
  let arg1 = readE () in
  (arg1)
let readI1X () = 
  let arg1 = readI1 () in
  (arg1)
let readCX () = 
  let arg1 = readC () in
  (arg1)
let readKX () = 
  let arg1 = readK () in
  (arg1)
let readIX () = 
  let arg1 = readI () in
  (arg1)
let readFX () = 
  let arg1 = readF () in
  (arg1)
let readSX () = 
  let arg1 = readS () in
  (arg1)
let readMTX () = 
  let arg1 = readMT () in
  (arg1)
let readLX () = 
  let arg1 = readL () in
  (arg1)
let readRRX () = 
  let arg1 = readR () in
  let arg2 = readR () in
  (arg1, arg2)
let readERX () = 
  let arg1 = readE () in
  let arg2 = readR () in
  (arg1, arg2)
let readRCX () = 
  let arg1 = readR () in
  let arg2 = readC () in
  (arg1, arg2)
let readRIX () = 
  let arg1 = readR () in
  let arg2 = readI () in
  (arg1, arg2)
let readRFX () = 
  let arg1 = readR () in
  let arg2 = readF () in
  (arg1, arg2)
let readRSX () = 
  let arg1 = readR () in
  let arg2 = readS () in
  (arg1, arg2)
let readRI1X () = 
  let arg1 = readR () in
  let arg2 = readI1 () in
  (arg1, arg2)
let readRCEX () = 
  let arg1 = readR () in
  let arg2 = readCE () in
  (arg1, arg2)
let readECEX () = 
  let arg1 = readE () in
  let arg2 = readCE () in
  (arg1, arg2)
let readCLX () = 
  let arg1 = readC () in
  let arg2 = readL () in
  (arg1, arg2)
let readRKX () = 
  let arg1 = readR () in
  let arg2 = readK () in
  (arg1, arg2)
let readECX () = 
  let arg1 = readE () in
  let arg2 = readC () in
  (arg1, arg2)
let readI1ITX () = 
  let arg1 = readI1 () in
  let arg2 = readIT () in
  (arg1, arg2)
let readI1LX () = 
  let arg1 = readI1 () in
  let arg2 = readL () in
  (arg1, arg2)
let readSEGLX () = 
  let arg1 = readSEG () in
  let arg2 = readL () in
  (arg1, arg2)
let readI1LWPX () = 
  let arg1 = readI1 () in
  let arg2 = readL () in
  (arg1, arg2)
let readI1NX () = 
  let arg1 = readI1 () in
  let arg2 = readN () in
  (arg1, arg2)
let readI1HTX () = 
  let arg1 = readI1 () in
  let arg2 = readHT () in
  (arg1, arg2)
let readI1BVTX () = 
  let arg1 = readI1 () in
  let arg2 = readBVT () in
  (arg1, arg2)
let readCWPX () = 
  let arg1 = readC () in
  (arg1)
let readI1WPX () = 
  let arg1 = readI1 () in
  (arg1)
let readRRI1X () = 
  let arg1 = readR () in
  let arg2 = readR () in
  let arg3 = readI1 () in
  (arg1, arg2, arg3)
let readRCLX () = 
  let arg1 = readR () in
  let arg2 = readC () in
  let arg3 = readL () in
  (arg1, arg2, arg3)
let readRCI1X () = 
  let arg1 = readR () in
  let arg2 = readC () in
  let arg3 = readI1 () in
  (arg1, arg2, arg3)
let readSEGI1LX () = 
  let arg1 = readSEG () in
  let arg2 = readI1 () in
  let arg3 = readL () in
  (arg1, arg2, arg3)
let readI1LLX () = 
  let arg1 = readI1 () in
  let arg2 = readL () in
  let arg3 = readL () in
  (arg1, arg2, arg3)
let readNLLX () = 
  let arg1 = readN () in
  let arg2 = readL () in
  let arg3 = readL () in
  (arg1, arg2, arg3)
let readLLLLX () = 
  let arg1 = readL () in
  let arg2 = readL () in
  let arg3 = readL () in
  let arg4 = readL () in
  (arg1, arg2, arg3, arg4)
let readI1CWPX () = 
  let arg1 = readI1 () in
  let arg2 = readC () in
  (arg1, arg2)
let readI1I1WPX () = 
  let arg1 = readI1 () in
  let arg2 = readI1 () in
  (arg1, arg2)


let displayRX (arg1) = Bytecode.displayR arg1
let displayEX (arg1) = Bytecode.displayE arg1
let displayI1X (arg1) = Bytecode.displayI1 arg1
let displayCX (arg1) = Bytecode.displayC arg1
let displayKX (arg1) = Bytecode.displayK arg1
let displayIX (arg1) = Bytecode.displayI arg1
let displayFX (arg1) = Bytecode.displayF arg1
let displaySX (arg1) = Bytecode.displayS arg1
let displayMTX (arg1) = Bytecode.displayMT arg1
let displayLX (arg1) = Bytecode.displayL arg1
let displayRRX (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayR arg2)
let displayERX (arg1, arg2) = (Bytecode.displayE arg1) ^ ", " ^ (Bytecode.displayR arg2)
let displayRCX (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayC arg2)
let displayRIX (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayI arg2)
let displayRFX (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayF arg2)
let displayRSX (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayS arg2)
let displayRI1X (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayI1 arg2)
let displayRCEX (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayCE arg2)
let displayECEX (arg1, arg2) = (Bytecode.displayE arg1) ^ ", " ^ (Bytecode.displayCE arg2)
let displayCLX (arg1, arg2) = (Bytecode.displayC arg1) ^ ", " ^ (Bytecode.displayL arg2)
let displayRKX (arg1, arg2) = (Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayK arg2)
let displayECX (arg1, arg2) = (Bytecode.displayE arg1) ^ ", " ^ (Bytecode.displayC arg2)
let displayI1ITX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayIT arg2)
let displayI1LX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayL arg2)
let displaySEGLX (arg1, arg2) = (Bytecode.displaySEG arg1) ^ ", " ^ (Bytecode.displayL arg2)
let displayI1LWPX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayL arg2)
let displayI1NX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayN arg2)
let displayI1HTX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayHT arg2)
let displayI1BVTX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayBVT arg2)
let displayCWPX (arg1) = Bytecode.displayC arg1
let displayI1WPX (arg1) = Bytecode.displayI1 arg1
let displayRRI1X (arg1, arg2, arg3) = ((Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayR arg2)) ^ ", " ^ (Bytecode.displayI1 arg3)
let displayRCLX (arg1, arg2, arg3) = ((Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayC arg2)) ^ ", " ^ (Bytecode.displayL arg3)
let displayRCI1X (arg1, arg2, arg3) = ((Bytecode.displayR arg1) ^ ", " ^ (Bytecode.displayC arg2)) ^ ", " ^ (Bytecode.displayI1 arg3)
let displaySEGI1LX (arg1, arg2, arg3) = ((Bytecode.displaySEG arg1) ^ ", " ^ (Bytecode.displayI1 arg2)) ^ ", " ^ (Bytecode.displayL arg3)
let displayI1LLX (arg1, arg2, arg3) = ((Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayL arg2)) ^ ", " ^ (Bytecode.displayL arg3)
let displayNLLX (arg1, arg2, arg3) = ((Bytecode.displayN arg1) ^ ", " ^ (Bytecode.displayL arg2)) ^ ", " ^ (Bytecode.displayL arg3)
let displayLLLLX (arg1, arg2, arg3, arg4) = (((Bytecode.displayL arg1) ^ ", " ^ (Bytecode.displayL arg2)) ^ ", " ^ (Bytecode.displayL arg3)) ^ ", " ^ (Bytecode.displayL arg4)
let displayI1CWPX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayC arg2)
let displayI1I1WPX (arg1, arg2) = (Bytecode.displayI1 arg1) ^ ", " ^ (Bytecode.displayI1 arg2)


let inscatX_LEN = 4
let inscatRX_LEN = 4
let inscatEX_LEN = 4
let inscatI1X_LEN = 4
let inscatCX_LEN = 4
let inscatKX_LEN = 4
let inscatIX_LEN = 8
let inscatFX_LEN = 8
let inscatSX_LEN = 8
let inscatMTX_LEN = 8
let inscatLX_LEN = 8
let inscatRRX_LEN = 4
let inscatERX_LEN = 4
let inscatRCX_LEN = 4
let inscatRIX_LEN = 8
let inscatRFX_LEN = 8
let inscatRSX_LEN = 8
let inscatRI1X_LEN = 4
let inscatRCEX_LEN = 4
let inscatECEX_LEN = 4
let inscatCLX_LEN = 8
let inscatRKX_LEN = 4
let inscatECX_LEN = 4
let inscatI1ITX_LEN = 8
let inscatI1LX_LEN = 8
let inscatSEGLX_LEN = 8
let inscatI1LWPX_LEN = 12
let inscatI1NX_LEN = 4
let inscatI1HTX_LEN = 8
let inscatI1BVTX_LEN = 8
let inscatCWPX_LEN = 8
let inscatI1WPX_LEN = 8
let inscatRRI1X_LEN = 4
let inscatRCLX_LEN = 8
let inscatRCI1X_LEN = 8
let inscatSEGI1LX_LEN = 8
let inscatI1LLX_LEN = 12
let inscatNLLX_LEN = 12
let inscatLLLLX_LEN = 20
let inscatI1CWPX_LEN = 8
let inscatI1I1WPX_LEN = 8


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
let getSize_create_type_variable = inscatEX_LEN


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
  | Ins_create_type_variable(arg) -> writeopcode 141; writeEX arg

let readInstruction getKindFunc getConstantFunc =                             
  Bytecode.setGetKindFn getKindFunc;                                           
  Bytecode.setGetConstantFn getConstantFunc;                                   
  let opcode = readopcode () in
  if opcode = 0 then (Ins_put_variable_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 1 then (Ins_put_variable_p (readERX ()), inscatERX_LEN)
  else  if opcode = 2 then (Ins_put_value_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 3 then (Ins_put_value_p (readERX ()), inscatERX_LEN)
  else  if opcode = 4 then (Ins_put_unsafe_value (readERX ()), inscatERX_LEN)
  else  if opcode = 5 then (Ins_copy_value (readERX ()), inscatERX_LEN)
  else  if opcode = 6 then (Ins_put_m_const (readRCX ()), inscatRCX_LEN)
  else  if opcode = 7 then (Ins_put_p_const (readRCX ()), inscatRCX_LEN)
  else  if opcode = 8 then (Ins_put_nil (readRX ()), inscatRX_LEN)
  else  if opcode = 9 then (Ins_put_integer (readRIX ()), inscatRIX_LEN)
  else  if opcode = 10 then (Ins_put_float (readRFX ()), inscatRFX_LEN)
  else  if opcode = 11 then (Ins_put_string (readRSX ()), inscatRSX_LEN)
  else  if opcode = 12 then (Ins_put_index (readRI1X ()), inscatRI1X_LEN)
  else  if opcode = 13 then (Ins_put_app (readRRI1X ()), inscatRRI1X_LEN)
  else  if opcode = 14 then (Ins_put_list (readRX ()), inscatRX_LEN)
  else  if opcode = 15 then (Ins_put_lambda (readRRI1X ()), inscatRRI1X_LEN)
  else  if opcode = 16 then (Ins_set_variable_t (readRX ()), inscatRX_LEN)
  else  if opcode = 17 then (Ins_set_variable_te (readRX ()), inscatRX_LEN)
  else  if opcode = 18 then (Ins_set_variable_p (readEX ()), inscatEX_LEN)
  else  if opcode = 19 then (Ins_set_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 20 then (Ins_set_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 21 then (Ins_globalize_pt (readERX ()), inscatERX_LEN)
  else  if opcode = 22 then (Ins_globalize_t (readRX ()), inscatRX_LEN)
  else  if opcode = 23 then (Ins_set_m_const (readCX ()), inscatCX_LEN)
  else  if opcode = 24 then (Ins_set_p_const (readCX ()), inscatCX_LEN)
  else  if opcode = 25 then (Ins_set_nil, inscatX_LEN)
  else  if opcode = 26 then (Ins_set_integer (readIX ()), inscatIX_LEN)
  else  if opcode = 27 then (Ins_set_float (readFX ()), inscatFX_LEN)
  else  if opcode = 28 then (Ins_set_string (readSX ()), inscatSX_LEN)
  else  if opcode = 29 then (Ins_set_index (readI1X ()), inscatI1X_LEN)
  else  if opcode = 30 then (Ins_set_void (readI1X ()), inscatI1X_LEN)
  else  if opcode = 31 then (Ins_deref (readRX ()), inscatRX_LEN)
  else  if opcode = 32 then (Ins_set_lambda (readRI1X ()), inscatRI1X_LEN)
  else  if opcode = 33 then (Ins_get_variable_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 34 then (Ins_get_variable_p (readERX ()), inscatERX_LEN)
  else  if opcode = 35 then (Ins_init_variable_t (readRCEX ()), inscatRCEX_LEN)
  else  if opcode = 36 then (Ins_init_variable_p (readECEX ()), inscatECEX_LEN)
  else  if opcode = 37 then (Ins_get_m_constant (readRCX ()), inscatRCX_LEN)
  else  if opcode = 38 then (Ins_get_p_constant (readRCLX ()), inscatRCLX_LEN)
  else  if opcode = 39 then (Ins_get_integer (readRIX ()), inscatRIX_LEN)
  else  if opcode = 40 then (Ins_get_float (readRFX ()), inscatRFX_LEN)
  else  if opcode = 41 then (Ins_get_string (readRSX ()), inscatRSX_LEN)
  else  if opcode = 42 then (Ins_get_nil (readRX ()), inscatRX_LEN)
  else  if opcode = 43 then (Ins_get_m_structure (readRCI1X ()), inscatRCI1X_LEN)
  else  if opcode = 44 then (Ins_get_p_structure (readRCI1X ()), inscatRCI1X_LEN)
  else  if opcode = 45 then (Ins_get_list (readRX ()), inscatRX_LEN)
  else  if opcode = 46 then (Ins_unify_variable_t (readRX ()), inscatRX_LEN)
  else  if opcode = 47 then (Ins_unify_variable_p (readEX ()), inscatEX_LEN)
  else  if opcode = 48 then (Ins_unify_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 49 then (Ins_unify_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 50 then (Ins_unify_local_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 51 then (Ins_unify_local_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 52 then (Ins_unify_m_constant (readCX ()), inscatCX_LEN)
  else  if opcode = 53 then (Ins_unify_p_constant (readCLX ()), inscatCLX_LEN)
  else  if opcode = 54 then (Ins_unify_integer (readIX ()), inscatIX_LEN)
  else  if opcode = 55 then (Ins_unify_float (readFX ()), inscatFX_LEN)
  else  if opcode = 56 then (Ins_unify_string (readSX ()), inscatSX_LEN)
  else  if opcode = 57 then (Ins_unify_nil, inscatX_LEN)
  else  if opcode = 58 then (Ins_unify_void (readI1X ()), inscatI1X_LEN)
  else  if opcode = 59 then (Ins_put_type_variable_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 60 then (Ins_put_type_variable_p (readERX ()), inscatERX_LEN)
  else  if opcode = 61 then (Ins_put_type_value_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 62 then (Ins_put_type_value_p (readERX ()), inscatERX_LEN)
  else  if opcode = 63 then (Ins_put_type_unsafe_value (readERX ()), inscatERX_LEN)
  else  if opcode = 64 then (Ins_put_type_const (readRKX ()), inscatRKX_LEN)
  else  if opcode = 65 then (Ins_put_type_structure (readRKX ()), inscatRKX_LEN)
  else  if opcode = 66 then (Ins_put_type_arrow (readRX ()), inscatRX_LEN)
  else  if opcode = 67 then (Ins_set_type_variable_t (readRX ()), inscatRX_LEN)
  else  if opcode = 68 then (Ins_set_type_variable_p (readEX ()), inscatEX_LEN)
  else  if opcode = 69 then (Ins_set_type_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 70 then (Ins_set_type_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 71 then (Ins_set_type_local_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 72 then (Ins_set_type_local_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 73 then (Ins_set_type_constant (readKX ()), inscatKX_LEN)
  else  if opcode = 74 then (Ins_get_type_variable_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 75 then (Ins_get_type_variable_p (readERX ()), inscatERX_LEN)
  else  if opcode = 76 then (Ins_init_type_variable_t (readRCEX ()), inscatRCEX_LEN)
  else  if opcode = 77 then (Ins_init_type_variable_p (readECEX ()), inscatECEX_LEN)
  else  if opcode = 78 then (Ins_get_type_value_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 79 then (Ins_get_type_value_p (readERX ()), inscatERX_LEN)
  else  if opcode = 80 then (Ins_get_type_constant (readRKX ()), inscatRKX_LEN)
  else  if opcode = 81 then (Ins_get_type_structure (readRKX ()), inscatRKX_LEN)
  else  if opcode = 82 then (Ins_get_type_arrow (readRX ()), inscatRX_LEN)
  else  if opcode = 83 then (Ins_unify_type_variable_t (readRX ()), inscatRX_LEN)
  else  if opcode = 84 then (Ins_unify_type_variable_p (readEX ()), inscatEX_LEN)
  else  if opcode = 85 then (Ins_unify_type_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 86 then (Ins_unify_type_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 87 then (Ins_unify_envty_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 88 then (Ins_unify_envty_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 89 then (Ins_unify_type_local_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 90 then (Ins_unify_type_local_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 91 then (Ins_unify_envty_local_value_t (readRX ()), inscatRX_LEN)
  else  if opcode = 92 then (Ins_unify_envty_local_value_p (readEX ()), inscatEX_LEN)
  else  if opcode = 93 then (Ins_unify_type_constant (readKX ()), inscatKX_LEN)
  else  if opcode = 94 then (Ins_pattern_unify_t (readRRX ()), inscatRRX_LEN)
  else  if opcode = 95 then (Ins_pattern_unify_p (readERX ()), inscatERX_LEN)
  else  if opcode = 96 then (Ins_finish_unify, inscatX_LEN)
  else  if opcode = 97 then (Ins_head_normalize_t (readRX ()), inscatRX_LEN)
  else  if opcode = 98 then (Ins_head_normalize_p (readEX ()), inscatEX_LEN)
  else  if opcode = 99 then (Ins_incr_universe, inscatX_LEN)
  else  if opcode = 100 then (Ins_decr_universe, inscatX_LEN)
  else  if opcode = 101 then (Ins_set_univ_tag (readECX ()), inscatECX_LEN)
  else  if opcode = 102 then (Ins_tag_exists_t (readRX ()), inscatRX_LEN)
  else  if opcode = 103 then (Ins_tag_exists_p (readEX ()), inscatEX_LEN)
  else  if opcode = 104 then (Ins_tag_variable (readEX ()), inscatEX_LEN)
  else  if opcode = 105 then (Ins_push_impl_point (readI1ITX ()), inscatI1ITX_LEN)
  else  if opcode = 106 then (Ins_pop_impl_point, inscatX_LEN)
  else  if opcode = 107 then (Ins_add_imports (readSEGI1LX ()), inscatSEGI1LX_LEN)
  else  if opcode = 108 then (Ins_remove_imports (readSEGLX ()), inscatSEGLX_LEN)
  else  if opcode = 109 then (Ins_push_import (readMTX ()), inscatMTX_LEN)
  else  if opcode = 110 then (Ins_pop_imports (readI1X ()), inscatI1X_LEN)
  else  if opcode = 111 then (Ins_allocate (readI1X ()), inscatI1X_LEN)
  else  if opcode = 112 then (Ins_deallocate, inscatX_LEN)
  else  if opcode = 113 then (Ins_call (readI1LX ()), inscatI1LX_LEN)
  else  if opcode = 114 then (Ins_call_name (readI1CWPX ()), inscatI1CWPX_LEN)
  else  if opcode = 115 then (Ins_execute (readLX ()), inscatLX_LEN)
  else  if opcode = 116 then (Ins_execute_name (readCWPX ()), inscatCWPX_LEN)
  else  if opcode = 117 then (Ins_proceed, inscatX_LEN)
  else  if opcode = 118 then (Ins_try_me_else (readI1LX ()), inscatI1LX_LEN)
  else  if opcode = 119 then (Ins_retry_me_else (readI1LX ()), inscatI1LX_LEN)
  else  if opcode = 120 then (Ins_trust_me (readI1WPX ()), inscatI1WPX_LEN)
  else  if opcode = 121 then (Ins_try (readI1LX ()), inscatI1LX_LEN)
  else  if opcode = 122 then (Ins_retry (readI1LX ()), inscatI1LX_LEN)
  else  if opcode = 123 then (Ins_trust (readI1LWPX ()), inscatI1LWPX_LEN)
  else  if opcode = 124 then (Ins_trust_ext (readI1NX ()), inscatI1NX_LEN)
  else  if opcode = 125 then (Ins_try_else (readI1LLX ()), inscatI1LLX_LEN)
  else  if opcode = 126 then (Ins_retry_else (readI1LLX ()), inscatI1LLX_LEN)
  else  if opcode = 127 then (Ins_branch (readLX ()), inscatLX_LEN)
  else  if opcode = 128 then (Ins_switch_on_term (readLLLLX ()), inscatLLLLX_LEN)
  else  if opcode = 129 then (Ins_switch_on_constant (readI1HTX ()), inscatI1HTX_LEN)
  else  if opcode = 130 then (Ins_switch_on_bvar (readI1BVTX ()), inscatI1BVTX_LEN)
  else  if opcode = 131 then (Ins_switch_on_reg (readNLLX ()), inscatNLLX_LEN)
  else  if opcode = 132 then (Ins_neck_cut, inscatX_LEN)
  else  if opcode = 133 then (Ins_get_level (readEX ()), inscatEX_LEN)
  else  if opcode = 134 then (Ins_put_level (readEX ()), inscatEX_LEN)
  else  if opcode = 135 then (Ins_cut (readEX ()), inscatEX_LEN)
  else  if opcode = 136 then (Ins_call_builtin (readI1I1WPX ()), inscatI1I1WPX_LEN)
  else  if opcode = 137 then (Ins_builtin (readI1X ()), inscatI1X_LEN)
  else  if opcode = 138 then (Ins_stop, inscatX_LEN)
  else  if opcode = 139 then (Ins_halt, inscatX_LEN)
  else  if opcode = 140 then (Ins_fail, inscatX_LEN)
  else (Ins_create_type_variable (readEX ()), inscatEX_LEN)

let displayInstruction inst =
 match inst with
    Ins_put_variable_t(arg) -> ("put_variable_t           "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_put_variable_p(arg) -> ("put_variable_p           "^ (displayERX arg), inscatERX_LEN)
  | Ins_put_value_t(arg) -> ("put_value_t              "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_put_value_p(arg) -> ("put_value_p              "^ (displayERX arg), inscatERX_LEN)
  | Ins_put_unsafe_value(arg) -> ("put_unsafe_value         "^ (displayERX arg), inscatERX_LEN)
  | Ins_copy_value(arg) -> ("copy_value               "^ (displayERX arg), inscatERX_LEN)
  | Ins_put_m_const(arg) -> ("put_m_const              "^ (displayRCX arg), inscatRCX_LEN)
  | Ins_put_p_const(arg) -> ("put_p_const              "^ (displayRCX arg), inscatRCX_LEN)
  | Ins_put_nil(arg) -> ("put_nil                  "^ (displayRX arg), inscatRX_LEN)
  | Ins_put_integer(arg) -> ("put_integer              "^ (displayRIX arg), inscatRIX_LEN)
  | Ins_put_float(arg) -> ("put_float                "^ (displayRFX arg), inscatRFX_LEN)
  | Ins_put_string(arg) -> ("put_string               "^ (displayRSX arg), inscatRSX_LEN)
  | Ins_put_index(arg) -> ("put_index                "^ (displayRI1X arg), inscatRI1X_LEN)
  | Ins_put_app(arg) -> ("put_app                  "^ (displayRRI1X arg), inscatRRI1X_LEN)
  | Ins_put_list(arg) -> ("put_list                 "^ (displayRX arg), inscatRX_LEN)
  | Ins_put_lambda(arg) -> ("put_lambda               "^ (displayRRI1X arg), inscatRRI1X_LEN)
  | Ins_set_variable_t(arg) -> ("set_variable_t           "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_variable_te(arg) -> ("set_variable_te          "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_variable_p(arg) -> ("set_variable_p           "^ (displayEX arg), inscatEX_LEN)
  | Ins_set_value_t(arg) -> ("set_value_t              "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_value_p(arg) -> ("set_value_p              "^ (displayEX arg), inscatEX_LEN)
  | Ins_globalize_pt(arg) -> ("globalize_pt             "^ (displayERX arg), inscatERX_LEN)
  | Ins_globalize_t(arg) -> ("globalize_t              "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_m_const(arg) -> ("set_m_const              "^ (displayCX arg), inscatCX_LEN)
  | Ins_set_p_const(arg) -> ("set_p_const              "^ (displayCX arg), inscatCX_LEN)
  | Ins_set_nil -> ("set_nil                  ", inscatX_LEN)
  | Ins_set_integer(arg) -> ("set_integer              "^ (displayIX arg), inscatIX_LEN)
  | Ins_set_float(arg) -> ("set_float                "^ (displayFX arg), inscatFX_LEN)
  | Ins_set_string(arg) -> ("set_string               "^ (displaySX arg), inscatSX_LEN)
  | Ins_set_index(arg) -> ("set_index                "^ (displayI1X arg), inscatI1X_LEN)
  | Ins_set_void(arg) -> ("set_void                 "^ (displayI1X arg), inscatI1X_LEN)
  | Ins_deref(arg) -> ("deref                    "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_lambda(arg) -> ("set_lambda               "^ (displayRI1X arg), inscatRI1X_LEN)
  | Ins_get_variable_t(arg) -> ("get_variable_t           "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_get_variable_p(arg) -> ("get_variable_p           "^ (displayERX arg), inscatERX_LEN)
  | Ins_init_variable_t(arg) -> ("init_variable_t          "^ (displayRCEX arg), inscatRCEX_LEN)
  | Ins_init_variable_p(arg) -> ("init_variable_p          "^ (displayECEX arg), inscatECEX_LEN)
  | Ins_get_m_constant(arg) -> ("get_m_constant           "^ (displayRCX arg), inscatRCX_LEN)
  | Ins_get_p_constant(arg) -> ("get_p_constant           "^ (displayRCLX arg), inscatRCLX_LEN)
  | Ins_get_integer(arg) -> ("get_integer              "^ (displayRIX arg), inscatRIX_LEN)
  | Ins_get_float(arg) -> ("get_float                "^ (displayRFX arg), inscatRFX_LEN)
  | Ins_get_string(arg) -> ("get_string               "^ (displayRSX arg), inscatRSX_LEN)
  | Ins_get_nil(arg) -> ("get_nil                  "^ (displayRX arg), inscatRX_LEN)
  | Ins_get_m_structure(arg) -> ("get_m_structure          "^ (displayRCI1X arg), inscatRCI1X_LEN)
  | Ins_get_p_structure(arg) -> ("get_p_structure          "^ (displayRCI1X arg), inscatRCI1X_LEN)
  | Ins_get_list(arg) -> ("get_list                 "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_variable_t(arg) -> ("unify_variable_t         "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_variable_p(arg) -> ("unify_variable_p         "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_value_t(arg) -> ("unify_value_t            "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_value_p(arg) -> ("unify_value_p            "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_local_value_t(arg) -> ("unify_local_value_t      "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_local_value_p(arg) -> ("unify_local_value_p      "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_m_constant(arg) -> ("unify_m_constant         "^ (displayCX arg), inscatCX_LEN)
  | Ins_unify_p_constant(arg) -> ("unify_p_constant         "^ (displayCLX arg), inscatCLX_LEN)
  | Ins_unify_integer(arg) -> ("unify_integer            "^ (displayIX arg), inscatIX_LEN)
  | Ins_unify_float(arg) -> ("unify_float              "^ (displayFX arg), inscatFX_LEN)
  | Ins_unify_string(arg) -> ("unify_string             "^ (displaySX arg), inscatSX_LEN)
  | Ins_unify_nil -> ("unify_nil                ", inscatX_LEN)
  | Ins_unify_void(arg) -> ("unify_void               "^ (displayI1X arg), inscatI1X_LEN)
  | Ins_put_type_variable_t(arg) -> ("put_type_variable_t      "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_put_type_variable_p(arg) -> ("put_type_variable_p      "^ (displayERX arg), inscatERX_LEN)
  | Ins_put_type_value_t(arg) -> ("put_type_value_t         "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_put_type_value_p(arg) -> ("put_type_value_p         "^ (displayERX arg), inscatERX_LEN)
  | Ins_put_type_unsafe_value(arg) -> ("put_type_unsafe_value    "^ (displayERX arg), inscatERX_LEN)
  | Ins_put_type_const(arg) -> ("put_type_const           "^ (displayRKX arg), inscatRKX_LEN)
  | Ins_put_type_structure(arg) -> ("put_type_structure       "^ (displayRKX arg), inscatRKX_LEN)
  | Ins_put_type_arrow(arg) -> ("put_type_arrow           "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_type_variable_t(arg) -> ("set_type_variable_t      "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_type_variable_p(arg) -> ("set_type_variable_p      "^ (displayEX arg), inscatEX_LEN)
  | Ins_set_type_value_t(arg) -> ("set_type_value_t         "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_type_value_p(arg) -> ("set_type_value_p         "^ (displayEX arg), inscatEX_LEN)
  | Ins_set_type_local_value_t(arg) -> ("set_type_local_value_t   "^ (displayRX arg), inscatRX_LEN)
  | Ins_set_type_local_value_p(arg) -> ("set_type_local_value_p   "^ (displayEX arg), inscatEX_LEN)
  | Ins_set_type_constant(arg) -> ("set_type_constant        "^ (displayKX arg), inscatKX_LEN)
  | Ins_get_type_variable_t(arg) -> ("get_type_variable_t      "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_get_type_variable_p(arg) -> ("get_type_variable_p      "^ (displayERX arg), inscatERX_LEN)
  | Ins_init_type_variable_t(arg) -> ("init_type_variable_t     "^ (displayRCEX arg), inscatRCEX_LEN)
  | Ins_init_type_variable_p(arg) -> ("init_type_variable_p     "^ (displayECEX arg), inscatECEX_LEN)
  | Ins_get_type_value_t(arg) -> ("get_type_value_t         "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_get_type_value_p(arg) -> ("get_type_value_p         "^ (displayERX arg), inscatERX_LEN)
  | Ins_get_type_constant(arg) -> ("get_type_constant        "^ (displayRKX arg), inscatRKX_LEN)
  | Ins_get_type_structure(arg) -> ("get_type_structure       "^ (displayRKX arg), inscatRKX_LEN)
  | Ins_get_type_arrow(arg) -> ("get_type_arrow           "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_type_variable_t(arg) -> ("unify_type_variable_t    "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_type_variable_p(arg) -> ("unify_type_variable_p    "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_type_value_t(arg) -> ("unify_type_value_t       "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_type_value_p(arg) -> ("unify_type_value_p       "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_envty_value_t(arg) -> ("unify_envty_value_t      "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_envty_value_p(arg) -> ("unify_envty_value_p      "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_type_local_value_t(arg) -> ("unify_type_local_value_t "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_type_local_value_p(arg) -> ("unify_type_local_value_p "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_envty_local_value_t(arg) -> ("unify_envty_local_value_t "^ (displayRX arg), inscatRX_LEN)
  | Ins_unify_envty_local_value_p(arg) -> ("unify_envty_local_value_p "^ (displayEX arg), inscatEX_LEN)
  | Ins_unify_type_constant(arg) -> ("unify_type_constant      "^ (displayKX arg), inscatKX_LEN)
  | Ins_pattern_unify_t(arg) -> ("pattern_unify_t          "^ (displayRRX arg), inscatRRX_LEN)
  | Ins_pattern_unify_p(arg) -> ("pattern_unify_p          "^ (displayERX arg), inscatERX_LEN)
  | Ins_finish_unify -> ("finish_unify             ", inscatX_LEN)
  | Ins_head_normalize_t(arg) -> ("head_normalize_t         "^ (displayRX arg), inscatRX_LEN)
  | Ins_head_normalize_p(arg) -> ("head_normalize_p         "^ (displayEX arg), inscatEX_LEN)
  | Ins_incr_universe -> ("incr_universe            ", inscatX_LEN)
  | Ins_decr_universe -> ("decr_universe            ", inscatX_LEN)
  | Ins_set_univ_tag(arg) -> ("set_univ_tag             "^ (displayECX arg), inscatECX_LEN)
  | Ins_tag_exists_t(arg) -> ("tag_exists_t             "^ (displayRX arg), inscatRX_LEN)
  | Ins_tag_exists_p(arg) -> ("tag_exists_p             "^ (displayEX arg), inscatEX_LEN)
  | Ins_tag_variable(arg) -> ("tag_variable             "^ (displayEX arg), inscatEX_LEN)
  | Ins_push_impl_point(arg) -> ("push_impl_point          "^ (displayI1ITX arg), inscatI1ITX_LEN)
  | Ins_pop_impl_point -> ("pop_impl_point           ", inscatX_LEN)
  | Ins_add_imports(arg) -> ("add_imports              "^ (displaySEGI1LX arg), inscatSEGI1LX_LEN)
  | Ins_remove_imports(arg) -> ("remove_imports           "^ (displaySEGLX arg), inscatSEGLX_LEN)
  | Ins_push_import(arg) -> ("push_import              "^ (displayMTX arg), inscatMTX_LEN)
  | Ins_pop_imports(arg) -> ("pop_imports              "^ (displayI1X arg), inscatI1X_LEN)
  | Ins_allocate(arg) -> ("allocate                 "^ (displayI1X arg), inscatI1X_LEN)
  | Ins_deallocate -> ("deallocate               ", inscatX_LEN)
  | Ins_call(arg) -> ("call                     "^ (displayI1LX arg), inscatI1LX_LEN)
  | Ins_call_name(arg) -> ("call_name                "^ (displayI1CWPX arg), inscatI1CWPX_LEN)
  | Ins_execute(arg) -> ("execute                  "^ (displayLX arg), inscatLX_LEN)
  | Ins_execute_name(arg) -> ("execute_name             "^ (displayCWPX arg), inscatCWPX_LEN)
  | Ins_proceed -> ("proceed                  ", inscatX_LEN)
  | Ins_try_me_else(arg) -> ("try_me_else              "^ (displayI1LX arg), inscatI1LX_LEN)
  | Ins_retry_me_else(arg) -> ("retry_me_else            "^ (displayI1LX arg), inscatI1LX_LEN)
  | Ins_trust_me(arg) -> ("trust_me                 "^ (displayI1WPX arg), inscatI1WPX_LEN)
  | Ins_try(arg) -> ("try                      "^ (displayI1LX arg), inscatI1LX_LEN)
  | Ins_retry(arg) -> ("retry                    "^ (displayI1LX arg), inscatI1LX_LEN)
  | Ins_trust(arg) -> ("trust                    "^ (displayI1LWPX arg), inscatI1LWPX_LEN)
  | Ins_trust_ext(arg) -> ("trust_ext                "^ (displayI1NX arg), inscatI1NX_LEN)
  | Ins_try_else(arg) -> ("try_else                 "^ (displayI1LLX arg), inscatI1LLX_LEN)
  | Ins_retry_else(arg) -> ("retry_else               "^ (displayI1LLX arg), inscatI1LLX_LEN)
  | Ins_branch(arg) -> ("branch                   "^ (displayLX arg), inscatLX_LEN)
  | Ins_switch_on_term(arg) -> ("switch_on_term           "^ (displayLLLLX arg), inscatLLLLX_LEN)
  | Ins_switch_on_constant(arg) -> ("switch_on_constant       "^ (displayI1HTX arg), inscatI1HTX_LEN)
  | Ins_switch_on_bvar(arg) -> ("switch_on_bvar           "^ (displayI1BVTX arg), inscatI1BVTX_LEN)
  | Ins_switch_on_reg(arg) -> ("switch_on_reg            "^ (displayNLLX arg), inscatNLLX_LEN)
  | Ins_neck_cut -> ("neck_cut                 ", inscatX_LEN)
  | Ins_get_level(arg) -> ("get_level                "^ (displayEX arg), inscatEX_LEN)
  | Ins_put_level(arg) -> ("put_level                "^ (displayEX arg), inscatEX_LEN)
  | Ins_cut(arg) -> ("cut                      "^ (displayEX arg), inscatEX_LEN)
  | Ins_call_builtin(arg) -> ("call_builtin             "^ (displayI1I1WPX arg), inscatI1I1WPX_LEN)
  | Ins_builtin(arg) -> ("builtin                  "^ (displayI1X arg), inscatI1X_LEN)
  | Ins_stop -> ("stop                     ", inscatX_LEN)
  | Ins_halt -> ("halt                     ", inscatX_LEN)
  | Ins_fail -> ("fail                     ", inscatX_LEN)
  | Ins_create_type_variable(arg) -> ("create_type_variable     "^ (displayEX arg), inscatEX_LEN)