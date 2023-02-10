jsonparse(JSONString, Object) :-
    atom_chars(JSONString, JSONStringList),
    phrase(parse(Object), JSONStringList).

parse(JsonTerm) -->
    whitespace,
    ['{'],
    !,
    parse_object(JsonTerm).

parse(JsonTerm) -->
    whitespace,
    ['['],
    !,
    parse_array(JsonTerm).

parse_object(jsonobj(Members)) -->
    parse_members(Members),
    ['}'],
    whitespace,
    !.

parse_members([Pair| MoreMembers]) -->
    parse_pair(Pair),
    [','],
    !,
    parse_members(MoreMembers).

parse_members([Pair]) -->
    parse_pair(Pair),
    {print(Pair)}.


parse_array(jsonarray(Elements)) -->
    parse_elements(Elements),
    [']'],
    whitespace,
    !.

parse_elements([Value | MoreElements]) -->
    whitespace,
    parse_value(Value),
    whitespace,
    [','],
    whitespace,
    parse_elements(MoreElements).

parse_elements([Value]) -->
    parse_value(Value).

parse_pair(Key','Value) -->
    whitespace,
    parse_string(Key),
    whitespace,
    [':'],
    whitespace,
    parse_value(Value),
    whitespace,
    {print(Key','Value)}.

parse_string(String) -->
    ['"'],
        parse_string_list(String),
    ['"'].

% parse_value(Value) -->
%    parse_string(Value),
%    !.
    
parse_value(Value) -->
    parse_number(Value),
    !,
    {print(Value)}.
    
parse_value(Value) -->
    parse(Value),
    !.

parse_string_list(String) -->
    parse_chars(Chars),
    {Chars \= []},
    {atom_chars(String, Chars)}.

parse_chars([]) --> [].
parse_chars([Char | Chars]) -->
    parse_char(Char),
    !,
    parse_chars(Chars).

parse_char(Char) -->
    [Char].

parse_number(Number) -->
    parse_digits(Chars),
    {Chars \= []},
    {number_chars(Number, Chars)}.

parse_digits([Digit | Digits]) -->
    parse_digit(Digit),
    !,
    parse_digits(Digits).
parse_digits([]) --> [].

parse_digit(Digit) -->
    [Digit],
    {char_type(Digit, digit)},
    {print(Digit)}.

whitespace -->
    [' '],
    !,
    whitespace.

whitespace --> [].
