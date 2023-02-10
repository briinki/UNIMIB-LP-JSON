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
parse_members(MembersStringList, [Pair | MoreMembers]) :-
    split_members(MembersStringList, FirstPairRaw, MoreMembersRaw, []),
    parse_pair(FirstPairRaw, Pair),
    parse_members(MoreMembersRaw, MoreMembers).
    
parse_pair(PairStringList, (Key, Value)) :-
    split_pair(PairStringList, TempKey, TempValue),
    format_string(TempKey, Key),
    format_value(TempValue, Value).
    
parse_elements([], []).
parse_elements(ElementsString, [Value | MoreElements]) :-
    split_members(ElementsString, FirstValueRaw, MoreElementsRaw, []),
    format_value(FirstValueRaw, Value),
    parse_elements(MoreElementsRaw, MoreElements).

% format_value/2
% recursively formatting jsonobjects or jsonarrays or native elements
format_value(RawValue, Value) :-
    print(RawValue),
    parse(RawValue, Value).
format_value(RawValue, Value) :-
    format_string(RawValue, Value).
format_value(RawValue, Value) :-
    format_number(RawValue, Value).