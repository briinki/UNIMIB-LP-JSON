%%%% -*- Mode: Prolog -*- 

% JSONReverse
% Handling case where object is a jsonobj
jsonreverse(jsonobj(Members), AtomString) :-
    jsonreverse_members(Members, IntermediateString),
    concat_atom(['{', IntermediateString, '}'], AtomString),
    !.
% Handling case where object is a jsonarray
jsonreverse(jsonarray(Elements), AtomString) :-
    jsonreverse_elements(Elements, IntermediateString),
    concat_atom(['[', IntermediateString, ']'], AtomString),
    !.

% Members obj --> Member string : Base case
jsonreverse_members([], '').
% Members obj --> Member string : only one pair
jsonreverse_members([Pair | []], AtomString) :-
    jsonreverse_pair(Pair, AtomString).
% Members obj --> Member string : More than one Pair
jsonreverse_members([Pair | MoreMembers], AtomString) :-
    jsonreverse_pair(Pair, PairString),
    jsonreverse_members(MoreMembers, MoreMembersString),
    concat_atom([PairString, ',', MoreMembersString], AtomString).

% Pair obj --> Pair string : Only one Pair
jsonreverse_pair((RawKey, Value), AtomString) :-
    jsonreverse_value(RawKey, Key),
    jsonreverse_value(Value, IntermediateString),
    concat_atom([Key, ':', IntermediateString], AtomString).

% Reversing elements: from list of values to string 
jsonreverse_elements([], '').
jsonreverse_elements([Element | []], AtomString) :-
    jsonreverse_value(Element, AtomString).
jsonreverse_elements([Value | MoreElements], AtomString) :-
    jsonreverse_value(Value, ValueString),
    jsonreverse_elements(MoreElements, MoreElementsString),
    concat_atom([ValueString, ',', MoreElementsString], AtomString).

jsonreverse_value(false, false).
jsonreverse_value(true, true).
jsonreverse_value(null, null).
% JSONObject value is a string.
jsonreverse_value(JSONString, String) :-
    string(JSONString),
    concat_atom(['\"', JSONString, '\"'], String),
    !.
% Handling the case where JSONObject value is a number.
jsonreverse_value(JSONNumber, String) :-
    number(JSONNumber),
    concat_atom([JSONNumber], String),
    !.
% Handling the case where JSONObject value is a composite object
jsonreverse_value(JSONObj, String) :- 
    jsonreverse(JSONObj, String),
    !.