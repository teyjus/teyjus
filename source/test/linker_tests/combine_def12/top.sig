/* Testing combine definitions:
 * top-level exportdef predicate has no definition in top and accumulated 
 * modules.
 */

sig top.

kind i type.

type foo i -> o.
exportdef foo.