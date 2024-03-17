module uncurry.

uncurry_lift (abstr uncurry (x\ abstr Label (M x))) (abstr' (x\ (M' x x))) :-
 (pi x\ pi x'\ uncurry_lift x x' => 
    uncurry_lift (abstr Label (M x)) (abstr' (M' x'))).
    
uncurry_lift (abstr normal M) (abstr' M') :-
 (pi x\ pi x'\ uncurry_lift x x' => 
    uncurry_lift (M x) (M' x')).
