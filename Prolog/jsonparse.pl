jsonparse(JSONString, Object) :-
    atom_chars(JSONString, JSONStringList),
    phrase(parse(Object), JSONStringList).

parse(JsonTerm) -->
    whitespace,
    ['{'],
    !,
    parse_object(JsonTerm).

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

parse_pair(Key','Value) -->
    whitespace,
    parse_key(Key),
    whitespace,
    [':'],
    whitespace,
    parse_value(Value),
    whitespace.

parse_key(Key) --> parse_string(Key).

parse_string(String) -->
    ['"'],
    String
    ['"'].

whitespace -->
    [' '],
    !,
    whitespace.

whitespace --> [].