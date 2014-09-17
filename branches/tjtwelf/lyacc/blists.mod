module blists.
% basic list utilities


% (memb X L) succeeds if X is a member of L.  In contrast to ``member'',
%this will succeed as often as X unifies with members of L.

memb X (X :: L). 	% Test
memb X (Y :: L) :- memb X L.

% (member X L) succeeds if X is a member of L.  In contrast to ``memb'',
% this will succeed only once: for the first unifier of X with a member of L.

member X (X :: L) :- !.
member X (Y :: L) :- member X L.

% (append L K M) succeeds if M is the result of appending K to L.

append nil K K.
append (X :: L) K (X :: M) :- append L K M.

% (join L K M) is similar to append except that members of L that already
% appear in K are not appended to form M.

join nil K K.
join (X :: L) K M :- memb X K, !, join L K M.
join (X :: L) K (X :: M) :- join L K M.

% (memb_and_rest X L M) succeeds for every occurrence of X in L,
% where M is the result of removing that occurrence from L.

memb_and_rest A (A :: Rest) Rest.
memb_and_rest A (B :: Tail) (B :: Rest) :- memb_and_rest A Tail Rest.

% (nth_item N X L) succeeds if the Nth item of L is X, where
% counting starts at 1.

nth_item N A List      :- N < 1, !, fail.
nth_item 1 A (B::Rest) :- !, A = B.
nth_item N A (B::Tail) :- M is (N - 1), nth_item M A Tail.


% (nth_remove N L List) succeeds if the Nth item of L is removed
% to return List
nth_remove 1 (A::Rest) Rest.
nth_remove N (A::Rest) (A::List) :- N1 is N - 1, nth_remove N1 Rest List.


% (nth_and_rest N X L Rest) succeeds if the Nth item of L is X, and 
% Rest is the rest of L.  Counting starts at 1.  For N = 0, it behaves
% like memb_and_rest.

nth_and_rest 0 A List Rest :- !, memb_and_rest A List Rest.
nth_and_rest 1 A (B::Rest) Rest :- !, A = B.
nth_and_rest N A (B::Tail) (B::Rest) :-
  M is (N - 1), nth_and_rest M A Tail Rest.

% (nth_and_replace N X L Rep K) succeeds if the Nth item of L is X, and 
% K is the result of inserting Rep for X in L.  
% Counting starts at 1.  

nth_and_replace 0 _ _ _ _ :- !, fail.
nth_and_replace 1 A (B::Rest) Rep K :- !, A = B, append Rep Rest K.
nth_and_replace N A (B::Tail) Rep (B::Rest) :-
  M is (N - 1), nth_and_replace M A Tail Rep Rest.

% (reverse L K) succeeds if L and K are two lists that are reverses
% of each other.

reverse L K :- pi rv\((rv nil K, pi X\(pi N\(pi M\( rv (X::N) M :- rv N (X::M))))) => rv L nil).

% (length L N) succeeds if N is the length of list L.

length nil 0.
length (X::L) N :- length L M, N is M + 1.

% (nreverse L K) succeeds if L and K are two lists that are reverses
% of each other.  Implemented using the naive reverse algorithm.

nreverse nil nil.
nreverse (X::L) K :- nreverse L M, append M (X::nil) K.

%  (countup N L M) succeeds when L is the list of integers between N
% and M inclusively and when is L is nil and N > M.

countup N nil M      :-  N > M, !.
countup N (N::nil) N :- !.
countup N (N::L) M   :- N1 is N + 1, countup N1 L M.

% (permutation L K) succeeds if L and K are permutations of each other.

permutation nil nil.
permutation L (H::T) :-
  append V (H::U) L, append V U W, permutation W T.

% (flatten L F) succeeds if F is the result of flatten the list of
% lists L.

flatten (L :: R) F :- flatten R FR, append L FR F.
flatten nil nil.

% (delete_member X L K) succeeds if K is the result of deleting all
%  occurrences of X from L.

delete_member _ nil nil.
delete_member X (X::L)     K  &
delete_member X (Y::L) (Y::K) :- !, delete_member X L K.

% (split L K M) succeeds if L is split into K and M (implements 
% multiset splitting).

split nil nil nil.
split (X::L) (X::K) M :- split L K M.
split (X::L) K (X::M) :- split L K M.

% (intersection L K M) succeeds if M is the intersection of L and K.

intersection nil K nil.
intersection (X::L) K (X::M) :- member X K, !, intersection L K M.
intersection (X::L) K M      :- intersection L K M.

% (replace_member X L K M) succeeds if M is the result of replacing 
% the first occurrence of X in L with K.  Notice that an element 
% is replace with a list.

replace_member X (X::L) K M :- !, append K L M.
replace_member X (Y::L) K (Y::M) :- replace_member X L K M.

% (filter P L K) succeeds if K is the result of filtering out
% the items in L which do not have property P
filter P nil nil.
filter P (X::L) (X::K) :- P X, !, filter P L K.
filter P (X::L) K :- filter P L K.
