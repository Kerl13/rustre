# Utilisation

## Dépendences

Pour compiler le binaire `rustre`, il faut :
- une version d'ocaml (testé en version 4.05.x)
- menhir (testé en version 20171206)
- ocamlbuild (testé en version 0.11.0)

Il est aussi recommandé d'avoir installé :
- rustc (testé en version 1.22.1)
- ocamlgraphics (pour notre exemple pong)
- Why3 0.88.1, Alt-Ergo 1.30, Z3 4.4.2


## Compiler le binaire

Le binaire `rustre` peut être compilé à l'aide de la commande `make` lancée à la racine du projet.

La commande `./rustre -help` devrait donner quelque chose comme :

```
usage: ./rustre [options] file.lus main
  -extract {why3|rust} Extract to why3 or rust
  -o File to write the generated code
  -v Verbose output
  -opt Optimize object code
  -spec Prove compilation
  -nils Prove that nils are not used
  -ext Use automata extension
  -help  Display this list of options
  --help  Display this list of options
```

- Par défaut, `./rustre file.lus main_node` affiche le résultat de la compilation (code séquentiel)
sur la sortie standard. Utiliser `-o` pour écrire le code dans un fichier.
- Il est possible de générer du code Rust et Why3 (Rust par défaut)
- `-v` affiche toutes les représentations intermédiaires sur la sortie standard.
- `-opt` effectue des passes d'optimisation sur l'AST objet (voir le rapport).
- `-spec` et `-nils` sont deux analyses automatisées en Why3 (voir le rapport). À utiliser uniquement
  avec `-extract why3` et `-opt`.
- Si l'argument `-ext` est passé, la syntaxe concrète est étendue avec des automates (voir le rapport).

### Why3
L'extraction Why3 génère un fichier .mlw avec le code séquentiel et les spécifications. Il est très recommandé d'utiliser `-opt` pour supprimer les merge inutiles qui alourdissent le contexte des prouveurs.

### Génération de la preuve sémantique

Why3 ne semble pas disposer d'une commande pour générer les obligations de preuve Coq sans passer par l'IDE, il faut donc le faire en 2 temps. Cela demande donc quelques manipulations :
* supprimer la session Why3 correspondant au fichier spec.mlw (donc le dossier spec)
* appeler rustre avec les option `-spec -o test.mlw -extract why3 -opt` (le nom de fichier est test.mlw est important)
* ouvrir l'ide Why3 et générer les obligations de preuve (cliquer sur `Coq` puis `Edit`), fermer l'ide en sauvegardant la session
* relancer rustre avec les mêmes options
* les preuves Coq sont complétés.

### Rust
L'extraction Rust génère un fichier standalone qui peut être compilé avec la commande `rustc <file>`.

## Tester

Une série de fichiers d'exemples se trouve sous le dossier `tests`.
Des exemples de programmes non valides sont présents dans le dossier `tests/bad`.
Taper la commande `make test` à la racine du projet lance la compilation de ces fichiers automatiquement et vérifie qu'elle a lieu avec succès sur les programmes valides, et qu'elle échoue sur les programmes non valides.

## Exemples

- Nous avons écrit un pong ainsi qu'une intelligence artificielle (il suffit de faire `make` dans `pong/`). Plus de détails sont présents dans le rapport.
- Les horloges inférées sont polymorphes. Un exemple est présenté dans `tests/poly_clock.lus`.
- Un programme calculant des racines carrées utilisant la méthode de Newton est présent dans `tests/sqrt.lus`. L'utilisateur doit demander à chaque fois la valeur dont il veut calculer la valeur approchée. S'il demande de manière répétée la racine carrée de la même valeur, des approximations successives sont calculées. Lorsque la valeur change, le calcul est réinitialisé.
- Un programme calculant des valeurs approchées de π, présent dans `test/pi.lus`.
