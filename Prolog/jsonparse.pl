jsonparse(JSONString, Object) :-
    atomic(JSONString),
    atom_chars(JSONString, JSONCharsList),
    parser(JSONCharsList, Object).
    


% JSON Parser

% parsing json object
parser(['{', '}'], jsonobj([])).
parser(['{' | Xs], jsonobj(Members)) :-
    last(Xs, '}').


% parsing json array
parser(['[', ']'], jsonarray([])).
parser(['[' | Xs], Elements) :-
    last(Xs, ']').