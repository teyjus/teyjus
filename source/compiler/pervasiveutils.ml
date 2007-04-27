let isOverloaded c =
  (Pervasive.isoverloadUMinusConstant c) ||
  (Pervasive.isoverloadAbsConstant c) ||
  (Pervasive.isoverloadPlusConstant c) ||
  (Pervasive.isoverloadMinusConstant c) ||
  (Pervasive.isoverloadTimeConstant c) ||
  (Pervasive.isoverloadLTConstant c) ||
  (Pervasive.isoverloadGTConstant c) ||
  (Pervasive.isoverloadLEConstant c) ||
  (Pervasive.isoverloadGEConstant c)

let getIntOverload c =
  if Pervasive.isoverloadUMinusConstant c then
    Pervasive.intuminusConstant
  else if Pervasive.isoverloadPlusConstant c then
    Pervasive.intplusConstant
  else if Pervasive.isoverloadMinusConstant c then
    Pervasive.intminusConstant
  else if Pervasive.isoverloadTimeConstant c then
    Pervasive.intmultConstant
  else if Pervasive.isoverloadAbsConstant c then
    Pervasive.iabsConstant
  else if Pervasive.isoverloadLTConstant c then
    Pervasive.intlssConstant
  else if Pervasive.isoverloadGTConstant c then
    Pervasive.intgrtConstant
  else if Pervasive.isoverloadLEConstant c then
    Pervasive.intleqConstant
  else if Pervasive.isoverloadGEConstant c then
    Pervasive.intgeqConstant
  else
    (Errormsg.impossible Errormsg.none "PervasiveUtils.getIntOverload: invalid constant")

let getRealOverload c =
  if Pervasive.isoverloadUMinusConstant c then
    Pervasive.realuminusConstant
  else if Pervasive.isoverloadPlusConstant c then
    Pervasive.realplusConstant
  else if Pervasive.isoverloadMinusConstant c then
    Pervasive.realminusConstant
  else if Pervasive.isoverloadTimeConstant c then
    Pervasive.realmultConstant
  else if Pervasive.isoverloadAbsConstant c then
    Pervasive.iabsConstant
  else if Pervasive.isoverloadLTConstant c then
    Pervasive.reallssConstant
  else if Pervasive.isoverloadGTConstant c then
    Pervasive.realgrtConstant
  else if Pervasive.isoverloadLEConstant c then
    Pervasive.realleqConstant
  else if Pervasive.isoverloadGEConstant c then
    Pervasive.realgeqConstant
  else
    (Errormsg.impossible Errormsg.none "PervasiveUtils.getRealOverload: invalid constant")

let getStringOverload c =
  if Pervasive.isoverloadLTConstant c then
    Pervasive.strlssConstant
  else if Pervasive.isoverloadGTConstant c then
    Pervasive.strgrtConstant
  else if Pervasive.isoverloadLEConstant c then
    Pervasive.strleqConstant
  else if Pervasive.isoverloadGEConstant c then
    Pervasive.strgeqConstant
  else
    (Errormsg.impossible Errormsg.none "PervasiveUtils.getStringOverload: invalid constant")

let getOverload k c =
  if Pervasive.iskint k then
    getIntOverload c
  else if Pervasive.iskreal k then
    getRealOverload c
  else if Pervasive.iskstring k then
    getStringOverload c
  else
    c

let maxSkeletonIndex = 256


let cutFailTerm = 
  Absyn.ApplicationTerm(Absyn.FirstOrderApplication(
						Absyn.ConstantTerm(Pervasive.andConstant, [], false, Errormsg.none),
						[Absyn.ConstantTerm(Pervasive.cutConstant, [], false, Errormsg.none) ;
						 Absyn.ConstantTerm(Pervasive.failConstant, [], false, Errormsg.none)], 2),
						false, Errormsg.none)
