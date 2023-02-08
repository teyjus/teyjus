sig  maps.

kind  i  type.

type  father  i -> i -> o.

type  jane    i.
type  moses   i.
type  john    i.
type  peter   i.
type  james   i.
type  charles i.

type	mapfun		list A -> (A -> B) -> list B -> o.
type	mappred		list A -> (A -> B -> o) -> list B -> o.
type	reduce		list A -> (A -> B -> B) -> B -> B -> o.
type	reduce_eval	list A -> (A -> B -> B) -> B -> B -> o.
type	reduce_pred	list A -> (A -> B -> o) -> (B -> B -> B) -> B -> o.
type	for_each	list A -> (A -> o) -> o.

% for testing
type   a  i.
type   b  i.
type   g  i -> i -> i.