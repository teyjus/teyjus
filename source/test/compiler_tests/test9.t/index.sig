/* Testing clause structures:
 * indexing:              
 *     a. predicate with one clause definition
 *     b. predicate with two clause definitions
 *     c. predicate with more than two clauses 
 *     d. predicate with clauses with constant as first argument
 *     e. predicate with clauses with list as first argument
 *     f. predicate with multiple clauses with same constant as first argument
 */

sig index.

kind i type.

type foo1 i -> o.
type foo2 i -> o.
type foo3 i -> o.
type foo4 i -> o.
type foo5 (list i) -> o.
type foo6 i -> o.


type a i.
type b i.



