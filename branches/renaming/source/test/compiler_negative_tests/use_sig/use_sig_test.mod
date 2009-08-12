module use_sig_test.

% c and d are exportdef in used_sig, and should therefore be useonly,
% so these should fail.
c 0.
d 0.
