### TL;DR — Pipeline minimal

1. Extraction DB2 → `EXTRACT.DATA`

Compile + run : `JXTRCT54`

2. Sous-programme date (`COBG53DT`)

Compile : `CCOBG53D`

3. Génération factures (`CODISTIL`)

Compile : `CCODISTIL`
Run : `CREAPS` → `FACTURES.DATA`



### Résumé projet

3 programmes COBOL + 4 JCL permettant de :

extraire DB2 → fichier séquentiel,
produire la date en lettres,
générer les factures.



### Fichiers produits

`EXTRACT.DATA` : extraction DB2
`FACTURES.DATA` : factures générées
