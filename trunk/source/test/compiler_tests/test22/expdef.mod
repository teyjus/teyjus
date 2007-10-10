module expdef.

/* case 1: 
    expdef.sig: exportdef foo1  i -> o.
 */
foo1 X. 

/* case 2:
    expdef.sig:
      exportdef foo2  i -> o.
*/
type foo2 i -> o.
foo2 X.

/* case 3:
    expdef.sig: 
      type      foo3  i -> o.   
   *** buggy: foo3 is not marked as exportdef ***
 */
exportdef foo3 i -> o.
foo3 X.

/* case 4:
    expdef.sig
      type      foo4 i -> o.
   *** buggy: foo4 is not marked as exportdef ***
*/
exportdef foo4.
foo4 X.

/* case 5: 
    expdef.sig: 
      type      foo5  i -> o.
      exportdef foo5  i -> o.
   *** buggy: foo5 is not marked as exportdef ***
 */
foo5 X.

/* case 6: 
    expdef.sig: 
      type      foo6  i -> o.
      exportdef foo6.
   *** buggy: no error should be raised  ***
 */
foo6 X.

/* case 7: 
   *** buggy: foo7 should not be allowed: exportdef must be global
*/
type      foo7 i -> o.
exportdef foo7 i -> o.
foo7 X.
  
/* case 8: 
   *** buggy: foo8 should not be allowed: exportdef must be global
*/
type      foo8 i -> o.
exportdef foo8.
foo8 X.

/* case 9:
   *** buggy: foo9 should not be allowed: exportdef must have boolean target ***
*/
