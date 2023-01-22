%%%% -*- Mode: Prolog -*- 

:- [utils].

jsonparse(JSONString, Object) :-
    atomic(JSONString),
    atom_chars(JSONString, JSONStringList),
    parse(JSONStringList, Object).

% JSON Parser

% parsing json object
parse(['{', '}'], jsonobj([])).
parse(['{' | Xs], jsonobj(Members)) :-
    last(Xs, '}'),
    remove_last(Xs, PairsString),
    parse_members(PairsString, Members).

% parsing json array
parse(['[', ']'], jsonarray([])).
parse(['[' | Xs], jsonarray(Elements)) :-
    last(Xs, ']'),
    remove_last(Xs, ElementsString),
    parse_elements(ElementsString, Elements).

% parse_members/2 
% - RawMembers : Raw Json String without the external curly brackets
% - [Pair | Pairs] : Pairs List (ex: [("first_key", "first_value"), ("second_key", "second_value")])
parse_members(PairString, [Pair | []]) :-
    parse_pair(PairString, Pair).
    
parse_pair(PairString, (Key, Value)) :-
    split_pair(PairString, TempKey, Value),
    format_key(TempKey, Key).
    
parse_elements(ElementsString, [Element | Elements]).
