%%%% -*- Mode: Prolog -*- 

jsonparse(JSONString, Object) :-
    atomic(JSONString),
    atom_chars(JSONString, JSONStringList),
    phrase(grammar(Object), JSONStringList).

grammar(jsonobj([])) --> ['{'], whitespace, ['}'], !.
grammar(jsonobj(Members)) --> 
    ['{'],
    pairs(Members),
    ['}'], 
    !.

grammar(jsonarray([])) --> ['['], whitespace, [']'], !.
grammar(jsonarray(Elements)) -->
    ['['],
    values(Elements),
    [']'],
    !.

% PAIRS GRAMMAR
pairs([Pair | MoreMembers]) -->
    pair(Pair),
    [','],                              % dividing pairs by ','
    pairs(MoreMembers).                 % parsing pairs recursively
pairs([Pair]) --> pair(Pair), !.           % parsing single pair

% PAIR GRAMMAR
pair((Attribute, Value)) -->
    whitespace,
    string(Attribute),
    whitespace,
    [':'],
    value(Value),
    !.

values([Value | MoreElements]) -->
    value(Value),
    [','],
    values(MoreElements).
values([Element]) --> value(Element), !.

value(Value) -->
    whitespace,
    (
        grammar(Value) | 
        string(Value) | 
        number(Value) | 
        boolean(Value) | 
        null(Value)
    ),
    whitespace,
    !.

% STRING GRAMMAR
string('') --> ['"'], ['"']. % empty string
string(String) -->                      
    ['"'],
    format_string(String),
    ['"'].

format_string(String) -->
    chars(Chars),
    {Chars \= []},                          % checking if string is not empty
    {
        atom_chars(RawAtomString, Chars),       % converting chars list to atom 
        string_to_atom(String, RawAtomString)   % string and then to Prolog string
    }.  

% stacking up chars of a string recursively using DGCs.
chars([]) --> [].
chars([Char | Chars]) -->
    char(Char),
    !,
    chars(Chars).

% need to escaping some chars
char(Char) --> [Char], {check_char(Char)}.

% check_char/1 : check if a given Char is valid or not. 
% Char must to be instantiate.
check_char('\"') :- print("char").
check_char(Char) :- 
    string_codes(Char, [CharCode | _]).

% NUMBER GRAMMAR
number(Number) --> 
    format_digits(Digits),
    {
	atom_chars(RawNumberAtom, Digits),
	atom_number(RawNumberAtom, Number)
    }.

format_digits(Digits) -->
    digits(Digits),
    {Digits \= []}.

% parsing digits of a number
digits([Digit | Digits]) --> 
    digit(Digit),
    !,
    digits(Digits).
% Base case. It's here due to the left-most SLD policy of Prolog.
digits([]) --> [].

% number could be positive or negative, floating or integers; 
% as well as exponentials (<base>e<exponent> = <base>*10^<exponent>)
digit('-') --> ['-'].
digit('+') --> ['+'].
digit('.') --> ['.'].
digit('e') --> ['e'] | ['E'].
digit(Digit) --> [Digit], {char_type(Digit, digit)}.

% BOOLEAN GRAMMAR
boolean(true) --> ['t'], ['r'], ['u'], ['e'].
boolean(false) --> ['f'],['a'],['l'],['s'],['e'].

% NULL GRAMMAR
null(null) --> ['n'], ['u'], ['l'], ['l'].

% WHITESPACE GRAMMAR
whitespace --> ['\t'] , !, whitespace.  % it manages indentations,
whitespace --> ['\n'], ! , whitespace.  % line feeds,
whitespace --> ['\r'], !, whitespace.   % and carriage returns
whitespace --> [' '], !, whitespace.    % spaces
whitespace --> [].

% JSONACCESS with Fields list
% Base case for fields list : One field in the list
jsonaccess(Jsonobj, [Field | []], Result) :-
    jsonaccess(Jsonobj, Field, Result), !.
% Recursive case for fields list: Two o More fields in the list
jsonaccess(Jsonobj, [Field | Fields], Result) :-
    jsonaccess(Jsonobj, Field, RawResult),
    jsonaccess(RawResult, Fields, Result), !.

% JSONACCESS through one Field
% Handling the case where Jsonobj is a jsonobj
% Special case
jsonaccess(jsonobj(Members), [], jsonobj(Members)) :- !. % Forcing backtracking
jsonaccess(jsonobj(Members), Field, Result) :-
    jsonaccess_members(Members, Field, Result).
% Handling the case where Jsonobj is a jsonarray
jsonaccess(jsonarray(_), [], _) :- fail, !.
jsonaccess(jsonarray(Elements), N, Result) :-
    number(N),
    jsonaccess_elements(Elements, N, Result).

jsonaccess_members([(Field, Result) | _], Field, Result) :- !.
jsonaccess_members([ _ | MoreMembers], Field, Result) :- 
    jsonaccess_members(MoreMembers, Field, Result).

jsonaccess_elements([Result | _], 0, Result) :- !.
jsonaccess_elements([_ | MoreElements], N, Result) :-
    NewIndex is N - 1,
    jsonaccess_elements(MoreElements, NewIndex, Result).

jsonread(FileName, JSON) :-
    read_file_to_string(FileName, String, []),
    jsonparse(String, JSON).

jsondump(JSON, FileName) :-
    jsonreverse(JSON, JSONAtom),
    string_to_atom(JSONString, JSONAtom),
    print(JSONString),
    open(FileName, write, Stream),
    write(Stream, JSONString),
    close(Stream).
    
% JSONReverse
jsonreverse(jsonobj(Members), AtomString) :-
    print("jsonobject"),
    jsonreverse_members(Members, IntermediateString),
    concat_atom(['{', IntermediateString, '}'], AtomString).

jsonreverse(jsonarray(Elements), AtomString) :-
    print("jsonarray"),
    jsonreverse_elements(Elements, IntermediateString),
    concat_atom(['[', IntermediateString, ']'], AtomString).

jsonreverse_members([], '').
jsonreverse_members([(RawKey, Value) | []], AtomString) :-
    jsonreverse(RawKey, Key),
    jsonreverse(Value, IntermediateString),
    concat_atom([Key, ':', IntermediateString], AtomString),
    print(StringAtom).

jsonreverse_members([Pair | MoreMembers], AtomString) :-
    jsonreverse_members(Pair, PairString),
    jsonreverse_members(MoreMembers, MoreMembersString),
    print(MoreMembersString),
    concat_atom([PairString, ',', MoreMembersString], AtomString),
    print(AtomString).

jsonreverse_elements([], '').
jsonreverse_elements([Element | []], String) :-
    jsonreverse(Element, IntermediateString).
jsonreverse_elements([Element, MoreElements], String) :-
    jsonreverse(Element, IntermediateString1),
    jsonreverse_elements(MoreElements, IntermediateString2).

jsonreverse(RawString, String) :-
    string(RawString),
    concat_atom(['\"', RawString, '\"'], String).

reload :- [jsonparse], tty_clear.


