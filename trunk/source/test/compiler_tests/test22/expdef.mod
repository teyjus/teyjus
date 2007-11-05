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


/* case 5: 
    expdef.sig: 
      type      foo5  i -> o.
      exportdef foo5  i -> o.
 */
foo5 X.

/* case 6: 
    expdef.sig: 
      type      foo6  i -> o.
      exportdef foo6.
 */
foo6 X.

% negative test cases: currently commented out
/* case 3:
    expdef.sig: 
      type      foo3  i -> o.   
 */
%exportdef foo3 i -> o.
%foo3 X.

/* case 4:
    expdef.sig
      type      foo4 i -> o.
*/
%exportdef foo4.
%foo4 X.

/* case 7: 
*/
%type      foo7 i -> o.
%exportdef foo7 i -> o.
%foo7 X.



