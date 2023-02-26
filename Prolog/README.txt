UNIMIB@Disco - Linguaggi di programmazione
Anno Accademico 2022/23 - Appello di febbraio 2023

Progetto JSON Parser - PROLOG - Brini Luca 879459
--------------------------------------------------------

Il parser utilizza principalmente le DCG di Prolog, ovvero un particolare
tipo di operatore che permette di definire grammatiche context-free e costruisce
in automatico le difference list per leggere/derivare una particolare stringa
del linguaggio definito attraverso, per l'appunto, le DCG

La grammatica connotata dalla DCG è quella presente sul sito di json.org, con 
l' eccezione del non gestire i valori a meno che siano all'interno di 
oggetti o array json.

Tuttavia, le DCG non sono percorribili "al contrario". Per questo motivo,
per implementare il predicato jsondump, vengono utilizzati dei predicati ad-hoc
che permettono di passare da un oggetto ad una stringa json. 

Le DCG sono molto autoesplicative, motivo per il quale non ci sono molti
commenti che le descrivono
--------------------------------------------------------

Il carattere "\/" non viene riconosciuto direttamente dall'ambiente Prolog.
Tutti i restanti caratteri unicode vengono letti senza problemi, ad eccezione
del carattere "\"" che causa problemi con la definizione delle stringhe json.
--------------------------------------------------------

Il parser supporta i numeri interi, floating ed in notazione scientifica.

Tutti i numeri, da lista di cifre, vengono convertiti nel valore che denotano
grazie all'utilizzo del predicato atom_number/2. In particolare, i numeri in
notazione scientifica vengono convertiti in numeri "normali". Per questo motivo
ci potrebbero essere delle discrepanze tra jsonread e jsondump: il valore che
precedentemente era in notazione scientifica, dopo il jsondump è stato 
"calcolato" per intero
--------------------------------------------------------

Il progetto è diviso in più file: ognuno contiene i predicati appartenenti alla
stessa località (Esempio: jsonreverse contiene tutti i predicati necessari 
per passare da un oggetto JSON alla sua rappresentazione in stringa json)

Le prime due righe nel file jsonparse.pl:

:- [jsonaccess].
:- [jsonreverse].

permettono di caricare i file necessari.

--------------------------------------------------------

L'ultima riga del file jsonparse.pl, ovvero:
reload :- [jsonparse], tty_clear. 
È un predicato per il ricaricamento del sorgente che utilizzavo durante lo 
sviluppo a linea di comando.
--------------------------------------------------------

Nella cartella è presente anche un file test.pl che contiene tutti i test
presenti nella traccia del progetto. Molto utile in fase di testing e 
costruzione del parser. Ho realizzato una repo pubblica su github per il test,
per facilitare la vita ai miei colleghi! Solo test, niente progetto :)
https://github.com/briinki/UNIMIB-LP-JSON-Parser-Tests
--------------------------------------------------------

Ho notato che l'interprete Prolog nel caso debba printare a schermo un 
oggetto con tanti livelli di innestazione tende ad abbreviare con 
"| ...". 

Per ovviare a questa cosa è possibile utilizzare il seguente predicato:
set_prolog_flag(answer_write_options,
    [ quoted(true),
        portray(true),
        spacing(next_argument)
    ]
).