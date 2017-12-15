# Utilisation

## Dépendences

Pour compiler le binaire `rustre`, il faut:
- une version d'ocaml (testé en version 4.04.0)
- menhir (testé en version 20171206)

Il est aussi recommandé d'avoir installé:
- rustc (testé en version 1.22.1)
- **TODO** ocamlgraphics? (pour pong)
- **TODO** why3, alt-ergo, z3?


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
  avec `-extract why3`.
- Si l'argument `-ext` est passé, la syntaxe concrète est étendue avec des automates (voir le rapport).

### Why3
**todo**

### Rust
L'extraction Rust génère un fichier standalone qui peut être compilé avec la commande `rustc <file>`.

## Tester

Une série de fichiers d'exemples se trouve sous le dossier `tests`.
Des exemples de programmes non valides sont présents dans le dossier `tests/bad`.
Taper la commande `make test` à la racine du projet lance la compilation de ces fichiers automatiquement et vérifie qu'elle a lieu avec succès sur les programmes valides, et qu'elle échoue sur les programmes non valides.
