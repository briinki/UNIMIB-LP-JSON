%%%% -*- Mode: Prolog -*-

%%%% Luca Brini 879459

% JSONACCESS with Fields list
% Base case for fields list : One field in the list
jsonaccess(JSONObj, [Field | []], Result) :-
    jsonaccess(JSONObj, Field, Result), !.
% Recursive case for fields list: Two o More fields in the list
jsonaccess(JSONObj, [Field | MoreFields], Result) :-
    jsonaccess(JSONObj, Field, RawResult),
    jsonaccess(RawResult, MoreFields, Result), !.

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
