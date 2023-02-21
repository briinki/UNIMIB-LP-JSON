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

values([Value | MoreElements]) -->
    value(Value),
    [','],
    values(MoreElements).
values([Element]) --> value(Element), !.

% parsing pairs
pairs([Pair | MoreMembers]) -->
    pair(Pair),
    [','],                              % dividing pairs by ','
    pairs(MoreMembers).                 % parsing pairs recursively
pairs([Pair]) --> pair(Pair), !.           % parsing single pair

% parsing single pair
pair((Attribute, Value)) -->
    whitespace,
    string(Attribute),
    whitespace,
    [':'],
    value(Value),
    !.

% whitespace grammar
whitespace --> ['\t'] , !, whitespace.  % it manages indentations,
whitespace --> ['\n'], ! , whitespace.  % line feeds,
whitespace --> ['\r'], !, whitespace.   % and carriage returns
whitespace --> [' '], !, whitespace.    % spaces
whitespace --> [].

% string grammar
string('') --> ['"'], ['"'].
string(String) -->                      
    ['"'],
    format_string(String),
    ['"'].

format_string(String) -->
    chars(Chars),
    {Chars \= []},                      % checking if string is not empty
    {
        atom_chars(RawAtomString, Chars), 
        string_to_atom(String, RawAtomString)
    }.  % converting Chars List to atom string and then to string

% stacking up chars of a string recursively using DGCs.
chars([]) --> [].
chars([Char | Chars]) -->
    char(Char),
    !,
    chars(Chars).

% need to escaping some chars
char(Char) --> [Char], {check_char(Char)}. %% need to add spaces, newlines etc

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

% number grammar
number(Number) --> 
format_digits(Digits),
{
    atom_chars(RawNumberAtom, Digits),
    atom_number(RawNumberAtom, Number)
}.

format_digits(Digits) -->
    digits(Digits),
    {Digits \= []}.

digit('-') --> ['-'].
digit('+') --> ['+'].
digit('.') --> ['.'].
digit('e') --> ['e'] | ['E'].
digit(Digit) --> [Digit], {char_type(Digit, digit)}.

digits([Digit | Digits]) --> 
    digit(Digit),
    !,
    digits(Digits).
digits([]) --> [].

% boolean grammar
boolean(true) --> ['t'], ['r'], ['u'], ['e'].
boolean(false) --> ['f'],['a'],['l'],['s'],['e'].

% null grammar
null(null) --> ['n'], ['u'], ['l'], ['l'].

% check_char/1 : check if a given Char is valid or not. 
% Char must to be instantiate.
check_char('"') :- fail.
check_char(Char) :- 
    string_codes(Char, [CharCode | _]).
