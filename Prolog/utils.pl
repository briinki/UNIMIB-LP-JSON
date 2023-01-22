remove_last([_], []).
remove_last([X | Xs], [X | R]) :-
    remove_last(Xs, R).

test_split_pair(RawPairString, (Key, Value)) :-
    atom_chars(RawPairString, PairString),
    split_pair(PairString, Key, Value).

split_pair([':' | Value], [], Value) :- !.
split_pair([H | Xs], [H | R], Value) :-
    split_pair(Xs, R, Value).

format_key(RawKey, Key) :- 
    format_string(RawKey, TempKey),
    atomic_list_concat(TempKey, Key).

format_string(['"'| Xs], R) :-
    last(Xs, '"'),
    remove_last(Xs, R).