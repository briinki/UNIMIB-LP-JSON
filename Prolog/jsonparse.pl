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

grammar(jsonarray([])) --> ['['], whitespace, [']'].
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
char(Char) --> [Char], {check_char(Char)}. %% need to add spaces, newlines etc

% check_char/1 : check if a given Char is valid or not. 
% Char must to be instantiate.
check_char('"') :- fail.
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
