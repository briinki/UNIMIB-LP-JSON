% remove_last/2
% remove the last element from a list.
remove_last([_], []).
remove_last([X | Xs], [X | R]) :-
    remove_last(Xs, R).

%% split_members/3
split_members(RawMembersStringList, RawMembersStringList, []) :-
    \+ member(',', RawMembersStringList).
split_members([',' | Rest], [], Rest) :- !.
split_members([H | Xs], [H | R], Rest) :-
    split_members(Xs, R, Rest).

% split_pair/3
% given a key,value pair string, split it at the first ':' occurance
split_pair([':' | Value], [], Value) :- !.
split_pair([H | Xs], [H | R], Value) :-
    split_pair(Xs, R, Value).

% format_value/2
% RawString : "(a-z,)*" --> String (a-z)*
format_string(['"'| Xs], String) :-
    last(Xs, '"'),
    remove_last(Xs, Temp),
    atomic_list_concat(Temp, String).

format_number(NumberStringList, Number) :-
    atomic_list_concat(NumberStringList, NumberString),
    atom_number(NumberString, Number).

