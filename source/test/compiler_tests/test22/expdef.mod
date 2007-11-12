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





