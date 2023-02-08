/* Testing combine definitions:
 * top-level exportdef predicate only has definitions in accumulated module.
 */

sig top.

kind i type.

type foo1 i -> o.
exportdef foo1.

type foo2 i -> o.
exportdef foo2.

