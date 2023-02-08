/* Testing combine definitions:
 * top-level local predicate only has definitions in top module.
 */

module top.
accumulate acc.

type foo1 i -> o.
type foo2 i -> o.

foo1 X.

foo2 X.
foo2 X.