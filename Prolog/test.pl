%%%% -*- Mode: Prolog -*- 

:- [jsonparse].

test_obj_vuoto :- jsonparse('{}', jsonobj([])).
test_array_vuoto :- jsonparse('[]', jsonarray([])).

test_two_pair_std :- 
    jsonparse('{
    "nome" : "Arthur",
    "cognome" : "Dent"
    }', jsonobj([
        ("nome", "Arthur"), 
        ("cognome", "Dent")
    ])).

test_two_pair_string :- 
    jsonparse("{
    \"nome\" : \"Arthur\",
    \"cognome\" : \"Dentino\"
    }", jsonobj([
        ("nome", "Arthur"), 
        ("cognome", "Dentino")
    ])).

test_superbook_std :- 
    jsonparse('{
    "modello" : "SuperBook 1234",
    "anno di produzione" : 2014,
    "processore" : {
        "produttore" : "EsseTi",
    "velocità di funzionamento (GHz)" : [1, 2, 4, 8]
        }
    }', jsonobj([
        ("modello", "SuperBook 1234"), 
        ("anno di produzione", 2014), 
        ("processore", jsonobj([
            ("produttore", "EsseTi"), 
            ("velocità di funzionamento (GHz)", 
            jsonarray([1, 2, 4, 8]))]))
    ])).

test_superbook_string :- 
    jsonparse("{
    \"modello\" : \"SuperBook 1234\",
    \"anno di produzione\" : 2014,
    \"processore\" : {
        \"produttore\" : \"EsseTi\",
    \"velocità di funzionamento (GHz)\" : [1, 2, 4, 8]
        }
    }", jsonobj([
        ("modello", "SuperBook 1234"), 
        ("anno di produzione", 2014), 
        ("processore", jsonobj([
            ("produttore", "EsseTi"), 
            ("velocità di funzionamento (GHz)", 
            jsonarray([1, 2, 4, 8]))]))
    ])).

test_menu_std :- 
    jsonparse('{
    "type": "menu",
    "value": "File",
    "items": [
        {"value": "New", "action": "CreateNewDoc"},
        {"value": "Open", "action": "OpenDoc"},
        {"value": "Close", "action": "CloseDoc"}
        ]
    }', jsonobj([
        ("type", "menu"), 
        ("value", "File"), 
        ("items", jsonarray([
            jsonobj([("value", "New"), ("action", "CreateNewDoc")]), 
            jsonobj([("value", "Open"), ("action", "OpenDoc")]), 
            jsonobj([("value", "Close"), ("action", "CloseDoc")])]))
        ])
    ).

test_menu_string :- 
    jsonparse("{
    \"type\": \"menu\",
    \"value\": \"File\",
    \"items\": [
        {\"value\": \"New\", \"action\": \"CreateNewDoc\"},
        {\"value\": \"Open\", \"action\": \"OpenDoc\"},
        {\"value\": \"Close\", \"action\": \"CloseDoc\"}
        ]
    }", jsonobj([
        ("type", "menu"), 
        ("value", "File"), 
        ("items", jsonarray([
            jsonobj([("value", "New"), ("action", "CreateNewDoc")]), 
            jsonobj([("value", "Open"), ("action", "OpenDoc")]), 
            jsonobj([("value", "Close"), ("action", "CloseDoc")])]))
        ])
    ).

% true
test_access_special_1 :- jsonaccess(jsonobj(Members), [], jsonobj(Members)).

%false
pr_test_access_special_2 :- jsonaccess(jsonarray(_), [], _).
test_access_special_2 :-
    not(pr_test_access_special_2).

% O = jsonobj([(”nome”, ”Arthur”), (”cognome”, ”Dent”)])
% R = ”Arthur”
test_parse_access_1 :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
    jsonaccess(O, ["nome"], "Arthur").

% O = jsonobj([(”nome”, ”Arthur”), (”cognome”, ”Dent”)])
% R = ”Arthur”
test_parse_access_2 :-
    jsonparse('{"nome": "Arthur", "cognome": "Dent"}', O),
    jsonaccess(O, "nome", "Arthur"). 

% Z = jsonobj([(”name”, ”Zaphod”), (”heads”, jsonarray([”Head1”, ”Head2”]))])
% R = ”Head2”
test_parse_access_zaphod :-
    jsonparse('{"nome" : "Zaphod",
    "heads" : ["Head1", "Head2"]}', % Attenzione al newline.
    Z),
    jsonaccess(Z, ["heads", 1], "Head2").

% false
pr_test_mismatched_brackets :- jsonparse('[}', _).
test_mismatched_brackets :-
    not(pr_test_mismatched_brackets).

% false
pr_test_parse_access_out_of_bounds :- 
    jsonparse('[1, 2, 3]', A), 
    jsonaccess(A, [3], _).
test_parse_access_out_of_bounds :- 
    not(pr_test_parse_access_out_of_bounds).

% No.
pr_test_reverted_1 :- 
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}',
    jsonobj([jsonarray(_) | _])).
test_reverted_1 :- 
    not(pr_test_reverted_1).

% N = "Arthur"
test_reverted_2 :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}',
    jsonobj([("nome", "Arthur") | _])).

% R = "Dent"
test_reverted_3 :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', JSObj),
    jsonaccess(JSObj, ["cognome"], "Dent").

test_once(Predicate) :-
    write(Predicate),
    call(Predicate),
    write(": ✅\n").

test_all :-
    test_once(test_obj_vuoto),
    test_once(test_array_vuoto),
    test_once(test_two_pair_std),
    test_once(test_two_pair_string),
    test_once(test_superbook_std),
    test_once(test_superbook_string),
    test_once(test_menu_std),
    test_once(test_menu_string),
    test_once(test_access_special_1),
    test_once(test_access_special_2),
    test_once(test_parse_access_1),
    test_once(test_parse_access_2),
    test_once(test_parse_access_zaphod),
    test_once(test_mismatched_brackets),
    test_once(test_parse_access_out_of_bounds),
    test_once(test_reverted_1),
    test_once(test_reverted_2),
    test_once(test_reverted_3).

test :-
    print('\"').

reloadt :- [test], tty_clear.
