module string.
accumulate blists.


charAt S I C :- C is substring S I 1.

endsWith S1 S2 :- I1 is size S1, I2 is size S2, I3 is I1 - I2, S2 is substring S1 I3 I2.

startsWith S1 S2 :- I2 is size S2, S2 is substring S1 0 I2,!.

indexOf S1 _ SInd _ :- L is size S1, SInd > L, !, fail.
indexOf S1 S2 SInd SInd :- I1 is size S1, I2 is size S2, FInd is I1 - SInd, String is substring S1 SInd FInd, startsWith String S2.
indexOf S1 S2 SInd Ind :- SInd2 is SInd + 1, indexOf S1 S2 SInd2 Ind.

split_str "" _ nil.
split_str String Delist [First|Rest] :- memb X Delist, indexOf String X 0 Ind, First is substring String 0 Ind, L is size X,
				    Len is size String, NewInd is Ind + L, Endind is Len - NewInd, String2 is substring String NewInd Endind,
				    split_str String2 Delist Rest,!.
split_str String Delist [String].

unsplit_str nil _ "" :- !.
unsplit_str [A] _ A :- !.
unsplit_str [A|B] D String :- unsplit_str B D Tstring, String is (A ^ D ^ Tstring),!.

ltrim String TString :- startsWith String " ", L is size String, L2 is L - 1, String2 is substring String 1 L2, ltrim String2 TString,!.
ltrim String String.

rtrim String TString :- endsWith String " ", L is size String, L2 is L - 1, String2 is substring String 0 L2, rtrim String2 TString,!.
rtrim String String.

trim String TString :- ltrim String String2, rtrim String2 TString.

replaceAll _ Orig _ _ :- 0 is size Orig, !, fail.
replaceAll String Orig New RString :- split_str String [Orig] Strings, rejoin Strings New TStr, !,
				      ((endsWith String Orig, !, RString is TStr ^ New);RString is TStr), !.

type rejoin (list string) -> string -> string -> o.
rejoin [H] _ H.
rejoin [H|T] New RString :- TStr is H ^ New, rejoin T New TStr2, RString is TStr ^ TStr2.

contains S1 S2 :- indexOf S1 S2 0 _, !.















