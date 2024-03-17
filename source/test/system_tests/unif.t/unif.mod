module unif.

kind i     type.
type foo i -> i.

% type c i.

%% Simple example that fails:
%% The issue occurs at 'finish_unify' instruction,
%% causing a heap overflow:

%   AM_preg 1e13bd85 opcode: 96:finish_unify
%   Simulator: Heap overflow.

%% Observations:
%% 1. When the universally quantified x is replaced with
%%    a constant c, the test passes without problem.
%% 2. Test passes with only one instance of
%%     ((foo (Y1 (X x))) = (foo (Y2 x)))

main :- (pi x \
          ((foo (Y1 (X x))) = (foo (Y2 x))),
          ((foo (Y1 (X x))) = (foo (Y2 x)))).