val kint : Absyn.akind
val kreal : Absyn.akind
val kbool : Absyn.akind
val kstring : Absyn.akind
val klist : Absyn.akind
val kinstream : Absyn.akind
val koutstream : Absyn.akind


val numberPervasiveKinds : int

val iskint : Absyn.akind -> bool
val iskreal : Absyn.akind -> bool
val iskbool : Absyn.akind -> bool
val iskstring : Absyn.akind -> bool
val isklist : Absyn.akind -> bool
val iskinstream : Absyn.akind -> bool
val iskoutstream : Absyn.akind -> bool


val univConstant : Absyn.aconstant
val nilConstant : Absyn.aconstant
val consConstant : Absyn.aconstant
val intcConstant : Absyn.aconstant
val realcConstant : Absyn.aconstant
val strcConstant : Absyn.aconstant
val trueConstant : Absyn.aconstant
val cutConstant : Absyn.aconstant
val failConstant : Absyn.aconstant
val haltConstant : Absyn.aconstant
val stopConstant : Absyn.aconstant
val intuminusConstant : Absyn.aconstant
val modConstant : Absyn.aconstant
val iabsConstant : Absyn.aconstant
val intplusConstant : Absyn.aconstant
val intminusConstant : Absyn.aconstant
val intmultConstant : Absyn.aconstant
val intdivConstant : Absyn.aconstant
val intlssConstant : Absyn.aconstant
val intgrtConstant : Absyn.aconstant
val intleqConstant : Absyn.aconstant
val intgeqConstant : Absyn.aconstant
val timeConstant : Absyn.aconstant
val itorConstant : Absyn.aconstant
val floorConstant : Absyn.aconstant
val ceilConstant : Absyn.aconstant
val truncConstant : Absyn.aconstant
val realuminusConstant : Absyn.aconstant
val sqrtConstant : Absyn.aconstant
val sinConstant : Absyn.aconstant
val cosConstant : Absyn.aconstant
val arctanConstant : Absyn.aconstant
val logConstant : Absyn.aconstant
val rabsConstant : Absyn.aconstant
val rtosConstant : Absyn.aconstant
val realplusConstant : Absyn.aconstant
val realminusConstant : Absyn.aconstant
val realmultConstant : Absyn.aconstant
val realdivConstant : Absyn.aconstant
val reallssConstant : Absyn.aconstant
val realgrtConstant : Absyn.aconstant
val realleqConstant : Absyn.aconstant
val realgeqConstant : Absyn.aconstant
val slenConstant : Absyn.aconstant
val stoiConstant : Absyn.aconstant
val itochrConstant : Absyn.aconstant
val itostrConstant : Absyn.aconstant
val scatConstant : Absyn.aconstant
val strlssConstant : Absyn.aconstant
val strgrtConstant : Absyn.aconstant
val strleqConstant : Absyn.aconstant
val strgeqConstant : Absyn.aconstant
val getenvConstant : Absyn.aconstant
val substrConstant : Absyn.aconstant
val andConstant : Absyn.aconstant
val orConstant : Absyn.aconstant
val ampandConstant : Absyn.aconstant
val colondashConstant : Absyn.aconstant
val implConstant : Absyn.aconstant
val someConstant : Absyn.aconstant
val allConstant : Absyn.aconstant
val isConstant : Absyn.aconstant
val eqConstant : Absyn.aconstant
val stdinConstant : Absyn.aconstant
val stdoutConstant : Absyn.aconstant
val stderrConstant : Absyn.aconstant
val openinConstant : Absyn.aconstant
val openstrConstant : Absyn.aconstant
val openoutConstant : Absyn.aconstant
val openappConstant : Absyn.aconstant
val closeinConstant : Absyn.aconstant
val eofConstant : Absyn.aconstant
val closeoutConstant : Absyn.aconstant
val flushConstant : Absyn.aconstant
val termtostrConstant : Absyn.aconstant
val strtotermConstant : Absyn.aconstant
val outputConstant : Absyn.aconstant
val inputConstant : Absyn.aconstant
val inputlineConstant : Absyn.aconstant
val lookaheadConstant : Absyn.aconstant
val printConstant : Absyn.aconstant
val readConstant : Absyn.aconstant
val printtermConstant : Absyn.aconstant
val readtermConstant : Absyn.aconstant
val solveConstant : Absyn.aconstant
val notConstant : Absyn.aconstant
val opensocketConstant : Absyn.aconstant
val genericApplyConstant : Absyn.aconstant
val overloadUMinusConstant : Absyn.aconstant
val overloadAbsConstant : Absyn.aconstant
val overloadPlusConstant : Absyn.aconstant
val overloadMinusConstant : Absyn.aconstant
val overloadTimeConstant : Absyn.aconstant
val overloadLTConstant : Absyn.aconstant
val overloadGTConstant : Absyn.aconstant
val overloadLEConstant : Absyn.aconstant
val overloadGEConstant : Absyn.aconstant


val numberPervasiveConstants : int

val isunivConstant : Absyn.aconstant -> bool
val isnilConstant : Absyn.aconstant -> bool
val isconsConstant : Absyn.aconstant -> bool
val isintcConstant : Absyn.aconstant -> bool
val isrealcConstant : Absyn.aconstant -> bool
val isstrcConstant : Absyn.aconstant -> bool
val istrueConstant : Absyn.aconstant -> bool
val iscutConstant : Absyn.aconstant -> bool
val isfailConstant : Absyn.aconstant -> bool
val ishaltConstant : Absyn.aconstant -> bool
val isstopConstant : Absyn.aconstant -> bool
val isintuminusConstant : Absyn.aconstant -> bool
val ismodConstant : Absyn.aconstant -> bool
val isiabsConstant : Absyn.aconstant -> bool
val isintplusConstant : Absyn.aconstant -> bool
val isintminusConstant : Absyn.aconstant -> bool
val isintmultConstant : Absyn.aconstant -> bool
val isintdivConstant : Absyn.aconstant -> bool
val isintlssConstant : Absyn.aconstant -> bool
val isintgrtConstant : Absyn.aconstant -> bool
val isintleqConstant : Absyn.aconstant -> bool
val isintgeqConstant : Absyn.aconstant -> bool
val istimeConstant : Absyn.aconstant -> bool
val isitorConstant : Absyn.aconstant -> bool
val isfloorConstant : Absyn.aconstant -> bool
val isceilConstant : Absyn.aconstant -> bool
val istruncConstant : Absyn.aconstant -> bool
val isrealuminusConstant : Absyn.aconstant -> bool
val issqrtConstant : Absyn.aconstant -> bool
val issinConstant : Absyn.aconstant -> bool
val iscosConstant : Absyn.aconstant -> bool
val isarctanConstant : Absyn.aconstant -> bool
val islogConstant : Absyn.aconstant -> bool
val israbsConstant : Absyn.aconstant -> bool
val isrtosConstant : Absyn.aconstant -> bool
val isrealplusConstant : Absyn.aconstant -> bool
val isrealminusConstant : Absyn.aconstant -> bool
val isrealmultConstant : Absyn.aconstant -> bool
val isrealdivConstant : Absyn.aconstant -> bool
val isreallssConstant : Absyn.aconstant -> bool
val isrealgrtConstant : Absyn.aconstant -> bool
val isrealleqConstant : Absyn.aconstant -> bool
val isrealgeqConstant : Absyn.aconstant -> bool
val isslenConstant : Absyn.aconstant -> bool
val isstoiConstant : Absyn.aconstant -> bool
val isitochrConstant : Absyn.aconstant -> bool
val isitostrConstant : Absyn.aconstant -> bool
val isscatConstant : Absyn.aconstant -> bool
val isstrlssConstant : Absyn.aconstant -> bool
val isstrgrtConstant : Absyn.aconstant -> bool
val isstrleqConstant : Absyn.aconstant -> bool
val isstrgeqConstant : Absyn.aconstant -> bool
val isgetenvConstant : Absyn.aconstant -> bool
val issubstrConstant : Absyn.aconstant -> bool
val isandConstant : Absyn.aconstant -> bool
val isorConstant : Absyn.aconstant -> bool
val isampandConstant : Absyn.aconstant -> bool
val iscolondashConstant : Absyn.aconstant -> bool
val isimplConstant : Absyn.aconstant -> bool
val issomeConstant : Absyn.aconstant -> bool
val isallConstant : Absyn.aconstant -> bool
val isisConstant : Absyn.aconstant -> bool
val iseqConstant : Absyn.aconstant -> bool
val isstdinConstant : Absyn.aconstant -> bool
val isstdoutConstant : Absyn.aconstant -> bool
val isstderrConstant : Absyn.aconstant -> bool
val isopeninConstant : Absyn.aconstant -> bool
val isopenstrConstant : Absyn.aconstant -> bool
val isopenoutConstant : Absyn.aconstant -> bool
val isopenappConstant : Absyn.aconstant -> bool
val iscloseinConstant : Absyn.aconstant -> bool
val iseofConstant : Absyn.aconstant -> bool
val iscloseoutConstant : Absyn.aconstant -> bool
val isflushConstant : Absyn.aconstant -> bool
val istermtostrConstant : Absyn.aconstant -> bool
val isstrtotermConstant : Absyn.aconstant -> bool
val isoutputConstant : Absyn.aconstant -> bool
val isinputConstant : Absyn.aconstant -> bool
val isinputlineConstant : Absyn.aconstant -> bool
val islookaheadConstant : Absyn.aconstant -> bool
val isprintConstant : Absyn.aconstant -> bool
val isreadConstant : Absyn.aconstant -> bool
val isprinttermConstant : Absyn.aconstant -> bool
val isreadtermConstant : Absyn.aconstant -> bool
val issolveConstant : Absyn.aconstant -> bool
val isnotConstant : Absyn.aconstant -> bool
val isopensocketConstant : Absyn.aconstant -> bool
val isgenericApplyConstant : Absyn.aconstant -> bool
val isoverloadUMinusConstant : Absyn.aconstant -> bool
val isoverloadAbsConstant : Absyn.aconstant -> bool
val isoverloadPlusConstant : Absyn.aconstant -> bool
val isoverloadMinusConstant : Absyn.aconstant -> bool
val isoverloadTimeConstant : Absyn.aconstant -> bool
val isoverloadLTConstant : Absyn.aconstant -> bool
val isoverloadGTConstant : Absyn.aconstant -> bool
val isoverloadLEConstant : Absyn.aconstant -> bool
val isoverloadGEConstant : Absyn.aconstant -> bool


val pervasiveKinds : Absyn.akind Table.SymbolTable.t

val pervasiveConstants : Absyn.aconstant Table.SymbolTable.t

val pervasiveTypeAbbrevs : Absyn.atypeabbrev Table.SymbolTable.t


val implicationTerm : Absyn.aterm
val andTerm : Absyn.aterm

val isPerv : Absyn.aconstant -> bool                                          
val regClobberingPerv : Absyn.aconstant -> bool                                
val backtrackablePerv : Absyn.aconstant -> bool



