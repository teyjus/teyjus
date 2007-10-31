module useonly_test.

/* case 1: 
    useonly_test.sig: useonly foo1  i -> o.
 */
foo1 X. 

/* case 2:
    useonly_test.sig:
      useonly foo2  i -> o.
*/
type foo2 i -> o.
foo2 X.

/* case 3:
    useonly_test.sig: 
      type      foo3  i -> o.   
   *** buggy: foo3 is not marked as useonly ***
 */
useonly foo3 i -> o.
foo3 X.

/* case 4:
    useonly_test.sig
      type      foo4 i -> o.
   *** buggy: foo4 is not marked as useonly ***
*/
useonly foo4.
foo4 X.

/* case 5: 
    useonly_test.sig: 
      type      foo5  i -> o.
      useonly foo5  i -> o.
   *** buggy: foo5 is not marked as useonly ***
 */
foo5 X.

/* case 6: 
    useonly_test.sig: 
      type      foo6  i -> o.
      useonly foo6.
   *** buggy: no error should be raised  ***
 */
foo6 X.

/* case 7: 
   *** buggy: foo7 should not be allowed: useonly must be global
*/
type      foo7 i -> o.
useonly foo7 i -> o.
foo7 X.
  
/* case 8: 
   *** buggy: foo8 should not be allowed: useonly must be global
*/
type      foo8 i -> o.
useonly foo8.
foo8 X.

/* case 9:
   *** buggy: foo9 should not be allowed: useonly must have boolean target ***
*/
