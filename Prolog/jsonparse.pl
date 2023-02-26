%%%% -*- Mode: Prolog -*- 

%%%% Luca Brini 879459

:- [jsonaccess].
:- [jsonreverse].

jsonparse(JSONString, Object) :-
    atomic(JSONString),
    atom_chars(JSONString, JSONStringList),
    phrase(grammar(Object), JSONStringList).

grammar(jsonobj([])) --> ['{'], whitespace, ['}'], !.
grammar(jsonobj(Members)) --> 
    ['{'],
    members(Members),
    ['}'], 
    !.

grammar(jsonarray([])) --> ['['], whitespace, [']'], !.
grammar(jsonarray(Elements)) -->
    ['['],
    values(Elements),
    [']'],
    !.

% PAIRS GRAMMAR
members([Pair | MoreMembers]) -->
    pair(Pair),
    [','],                              % dividing members by ','
    members(MoreMembers).                % parsing members recursively
members([Pair]) --> pair(Pair), !.        % parsing single pair

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
    {Chars \= []},                  % checking if string is not empty
    {
        atom_chars(RawAtom, Chars),  % converting chars list to atom 
        string_to_atom(String, RawAtom)   % and then to Prolog string
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
check_char('\"').
check_char(Char) :- 
    string_codes(Char, [_ | _]).

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

% Parsing digits of a number
digits([Digit | Digits]) --> 
    digit(Digit),
    !,
    digits(Digits).
% Base case. It's here due to the left-most SLD policy of Prolog.
digits([]) --> [].

% Numbers could be positive or negative, floating or integer; 
% as well as exponential (<base>e<exponent> = <base>*10^<exponent>)
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

jsonread(FileName, JSON) :-
    read_file_to_string(FileName, String, []),
    jsonparse(String, JSON).

jsondump(JSON, FileName) :-
    jsonreverse(JSON, JSONAtom),
    string_to_atom(JSONString, JSONAtom),
    open(FileName, write, Stream),
    write(Stream, JSONString),
    close(Stream).

% tty_clear valid only for UNIX systems.
reload :- [jsonparse], tty_clear. 
