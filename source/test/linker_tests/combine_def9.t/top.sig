/* Testing combine definitions:
 * top-level exportdef predicate has definitions in both top and accumulated
 * modules.
 */
sig top.

kind i type.

/*           top                    acc 
 *  foo1     single def             single def
 *  foo2     multiple def           single def
 *  foo3     single def             multiple def
 *  foo4     multiple def           multiple def
 */


type foo1 i -> o. 
exportdef foo1.

type foo2 i -> o. 
exportdef foo2.

type foo3 i -> o.
exportdef foo3.

type foo4 i -> o.
exportdef foo4.

