:- [utils].
:- [jsonparse].

test_split_pair(RawPairString, (Key, Value)) :-
    atom_chars(RawPairString, PairString),
    split_pair(PairString, Key, Value).

test_split_members(RawMembersString, FirstPair, OtherPairs) :-
    atom_chars(RawMembersString, MemberString),
    split_members(MemberString, FirstPair, OtherPairs).

% Parsing JSON Tests
test_two_pair_all_strings(Object) :-
    jsonparse('{"key1":"value"}', Object).

test_two_pair_all_numbers(Object) :-
    jsonparse('{"chiave_uno":1123}', Object).

test_two_pair_number_and_string(Object) :-
    jsonparse('{"key1":"value1","key,2":2}', Object).

test_pair_with_special_char(Object) :-
    jsonparse('{"key1":"\t"}', Object).


test_one_pair(Object) :-
    jsonparse('{"key1":"value1"}', Object).

test_nested_empty_object(Object) :-
    jsonparse('{"key":{}}', Object).

test_nested_object(Object) :-
    jsonparse('{"key":{"nested_key":"nested_value"}}', Object).

test_nested_empty_array(Object) :-
    jsonparse('{"key":[]}', Object).

test_nested_array(Object) :-
    jsonparse('{"key":[1,2,3]}', Object).

test_array(Object) :-
    jsonparse('[1,2,3]', Object).
