/* Testing term structures:                       
 *   abstractions:
 *   a. abstractions as head argument;
 *   b. abstractions as goal or application argument; 
 *   c. nested abstractions in the above two cases
*/

sig abstraction.

kind i      type.

type abst   (i -> i) -> i.
type nabst  (i -> i -> i) -> i.
type appl   i -> i -> i.

type foo1   (i -> i) -> o.
type foo2   i -> o.
type foo3   (i -> i -> i) -> o.
