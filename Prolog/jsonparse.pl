%%%% -*- Mode: Prolog -*- 

:- [utils].

jsonparse(JSONString, Object) :-
    atomic(JSONString),
    atom_chars(JSONString, JSONStringList),
    parse(JSONStringList, Object),
    print(Object),
    !.

% JSON Parser

% parsing json object
parse(['{', '}'], jsonobj([])).
parse(['{' | Xs], jsonobj(Members)) :-
    last(Xs, '}'),
    remove_last(Xs, MembersStringList),
    parse_members(MembersStringList, Members),
    !.

% parsing json array
parse(['[', ']'], jsonarray([])).
parse(['[' | Xs], jsonarray(Elements)) :-
    last(Xs, ']'),
    remove_last(Xs, ElementsStringList),
    parse_elements(ElementsStringList, Elements),
    !.

% parse_members/2 
% - RawMembers : Raw Json String without the external curly brackets
% - [Pair | Pairs] : Pairs List (ex: [("first_key", "first_value"), ("second_key", "second_value")])
parse_members([], []).
parse_members(MembersStringList, [Pair | Pairs]) :-
    split_members(MembersStringList, FirstPairStringList, RestPairsStringList),
    parse_pair(FirstPairStringList, Pair),
    parse_members(RestPairsStringList, Pairs).
    
parse_pair(PairStringList, (Key, Value)) :-
    split_pair(PairStringList, TempKey, TempValue),
    format_string(TempKey, Key),
    format_value(TempValue, Value).
    
parse_elements(ElementsString, [Element | Elements]).

% format_value/2
% recursively formatting jsonobjects or jsonarrays
format_value(RawValue, Value) :-
    parse(RawValue, Value).

format_value(RawValue, Value) :-
    format_string(RawValue, Value).

format_value(RawValue, Value) :-
    format_number(RawValue, Value).