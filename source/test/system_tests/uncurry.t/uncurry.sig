sig uncurry.

%% This bug was reported by Litao Zhou
%% https://github.com/teyjus/teyjus/issues/125

kind label type.
type uncurry, normal label.

kind tm type.
type abstr  label -> (tm -> tm) -> tm.

kind tm' type.
type abstr'  (tm' -> tm') -> tm'.

type uncurry_lift     tm -> tm' -> o.