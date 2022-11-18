/* Testing term structures 
 * constants with types    
 *    a. constants with types as head argument
 *    b. constants with types as goal argument
 *    c. constants with types as application argument
 *    d. constants with types as application head
 */

sig tconst.

kind  pair  type -> type -> type.

type  tc1   A -> B.
type  tc2   (pair A B). 

type  foo   A -> o.

