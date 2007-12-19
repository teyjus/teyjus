/* Testing combine definitions:
 * top-level exportdef predicate has definitions in both top and accumulated 
 * modules.
 */

module acc.

/*           top                    acc 
 *  foo1     single def             single def
 *  foo2     multiple def           single def
 *  foo3     single def             multiple def
 *  foo4     multiple def           multiple def
 */


foo1 X.

foo2 X.

foo3 X.
foo3 X.

foo4 X.
foo4 X.
foo4 X.