module append.

append nil K K.
append (X::L) K (X::M) :- append L K M.

