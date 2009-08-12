sig mlt.


% from mltunify:

kind poly type.
kind substitution type.
kind difference type.
type emp substitution.
type sub poly -> poly -> substitution -> substitution.
type unify1 (list poly) -> (list poly) -> poly -> poly -> substitution -> o.
type unify (list poly) -> poly -> poly -> substitution -> o.
type sub_apply poly -> substitution -> poly -> o.
type diff poly -> poly -> difference.
type rigid poly -> o.
type vrd poly -> poly -> o.
type var poly -> o.
type distinct-vars poly -> poly -> o.
type occur-check poly -> poly -> o.
type transform (list difference) -> (list difference) -> substitution -> substitution -> o.
type copypoly poly -> poly -> o.
type unify0 (list difference) -> substitution -> substitution -> o.
type diff-subst substitution -> (list difference) -> (list
difference) -> o.
type sub-subst substitution -> substitution -> substitution -> o. 
type add-trivials (list poly) -> substitution -> substitution -> o.
type in-domain poly -> substitution -> o.
type not-indomain poly -> substitution -> o.
type integer poly.
type real poly.
type arr poly -> poly -> poly.
type u poly.
type w poly.
type q poly.
type update-sig (list poly) -> o -> o.

type setsub poly -> poly -> o.
type  member  (poly -> list poly -> o).
type  append  (list poly -> list poly -> list poly -> o).
  
%  for mltyper:


kind tm type.
type abs (tm -> tm) -> tm.
type app tm -> tm -> tm.
type let (tm -> tm) -> tm -> tm.
type zero tm.
type succ tm.
type op tm.
type op2 tm.
type op3 tm.
type op4 tm. type op5 tm. type op6 tm.
type all      (poly -> poly) -> poly.
type newvar (poly -> substitution) -> substitution.
type typeunify (list poly) -> poly -> poly -> substitution -> o.
type merge-sub substitution -> substitution -> substitution -> o. 
type append-sub substitution -> substitution -> substitution -> o. 
type merge-type poly -> poly -> poly -> o.
type typesub poly -> substitution -> poly -> o.
type apply_sub       poly -> substitution -> poly -> o.
type update-sub substitution -> substitution -> substitution -> o.
type typeunify2 (list poly) -> poly -> poly -> substitution -> o.
type fill-out substitution -> poly -> substitution -> poly -> o.
type monotype poly -> o.
type polytype (list poly) -> substitution -> tm -> poly -> o.
type resolve-for poly -> (list poly) -> substitution -> substitution -> o. 
type rsub2 (list poly) -> poly -> substitution -> substitution -> o.
type rsub3 (list poly) -> (list poly) -> poly -> substitution -> substitution -> o.
type collect-for poly -> substitution -> (list poly) -> substitution -> o.
type resolve-sub (list poly) -> (list poly) -> substitution -> substitution -> o.
type membrest poly -> (list poly) -> (list poly) -> o.

type infer-type tm -> poly -> o.
type remvac poly -> poly -> o.
type appear-in poly -> poly -> o.

type go o.
type example, tryonce, tryonce_ans  tm -> poly -> o.


