# PARTIE 3
## Pipeline

Voici l’ordre dans lequel exécuter les composants :

1. Compiler et exécuter l’extracteur :

`JXTRCTG54` (compilation)

`JXTRCT54` (extraction DB2 >>> `EXTRACT.DATA`)

2. Compiler le sous-programme de date :

`CCOBG53D`

3. Compiler le programme principal CODISTIL :

`CCODISTI`

4. Exécuter la génération des factures :

`CREAPS` >>> `FACTURES.DATA`

## Structure du projet

Ce dépôt contient 3 prog. COBOL + 4 JCL pour :
1. extraire les données d’une base DB2 dans un fichier séquentiel,
2. produire la date du jour en toutes lettres,
3. générer les factures au format séquentiel.

### >_ 1 Programme d’extraction DB2

Nom du programme : `XTRCTG54`
Rôle :
1. interroge la base de données,
2. extrait les données,
3. les écrit dans un fichier séquentiel `EXTRACT.DATA`.

Compilation & exécution :

JCL utilisé pour compiler et exécuter le programme : `JXTRCT54`


### >_ 2 Sous-programme : date du jour en toutes lettres

Sous-programme : `COBG53DT`
Renvoie la date du jour écrite en toutes lettres (ex. “Thursday, December 19, 2024”).
Compilation : `CCOBG53D`
Ce sous-programme est appelé par le programme principal CODISTIL.


### >_ 3 Programme principal — Génération des factures

Nom du programme : `CODISTIL`
Rôle :
1. lit les données extraites (`EXTRACT.DATA`),
2. utilise `COBG53DT` pour la date en lettres,
3. génère le fichier des factures `FACTURES.DATA` (séquentiel).

Compilation : `CCODISTIL`
Exécution : `CREAPS`

## Fichiers générés

| `EXTRACT.DATA` par `XTRCTG54` >>> Données extraites de la base DB2 | Séquentiel
| `FACTURES.DATA` par `CODISTIL` >>> Factures finales générées | Séquentiel
