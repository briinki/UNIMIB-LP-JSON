UNIMIB@Disco - Linguaggi di programmazione
Anno Accademico 2022/23 - Appello di febbraio 2023

Progetto JSON Parser - LISP - Brini Luca 879459
--------------------------------------------------------
--------------------------------------------------------
Il parser è stato implementato utilizzando la funzione multiple-value-bind, 
che permette di estrarre facilmente i valori di ritorno di funzioni che 
restituiscono più di un valore.

La funzionalità di base sta proprio in questa funzione:
Data una lista di caratteri che rappresentano la stringa json di input,
parso un oggetto alla volta e ritorno due valori: il primo è l'oggeto parsato,
il secondo la lista di caratteri rimanenti, che viene analizzata 
successivamente, finché raggiungo una lista di caratteri vuota

--------------------------------------------------------
Il parsing dei numeri avviene estrapolando le cifre del numero considerando
anche '+', '-', '.', 'e' oppure 'E'.
I numeri interi vengono successivamente calcolati utilizzando la posizione
di ogni cifra del numero intero 
- (Esempio: 123 --> 321. 123 = 3*10^0 + 2*10^1 + 1*10^2)

Per i numeri con la virgola, vengono calcolati come interi sia la parte prima
che quella dopo la virgola. Tuttavia, la parte decimale viene successivamente
divisa per 10^(numero di cifre decimali) e aggiunta alla parte intera,
calcolando cosi il reale floating numerb.

Per i numeri in notazione scientifica, calcolo la "base" come intero o floating
number, l'esponente del 10 come intero (considerando il segno), e 
successivamente calcolo il valore effettuando la moltiplicazione tra base e 10
elevato all'esponente appena parsato e faccio il casting a float, in modo da
mantenere la rappresentazione in notazione scientifica. 

. Per questo, se si effettua
il json-dump si potrebbero avere discrepanze dal json originale, proprio nella
rappresentazione del numero che precedentemente era in notazione scientifica.

--------------------------------------------------------
Il parsing delle costanti json come true, false e null avviene esclusivamente
leggendo la loro prima lettera. Questo perchè sono gli unici value che 
possono avere rispettivamente t, f ed n come primo carattere.

Per questo motivo salto la lettera dei successivi 4, 5, 4 caratteri e parso
direttamente il valore.