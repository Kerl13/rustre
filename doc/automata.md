# Extension avec les automates

Par rapport à ce qui est décrit dans l'article, on n'est pas censés gérer les
transitions "fortes" (construction `unless`), mais je ne vois pas en quoi c'est
plus compliqué.
Il faut aussi introduire des annotations de types explicites pour toutes les
variables, à cause de la distinction `int/float`.

## Syntaxe

On modifie les constructions `automata` et `let` en ajoutant des déclarations de
variables

Voir `tests/ex_auto00x.ls` pour des exemples de la syntaxe modifiée.
