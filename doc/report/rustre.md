# Projet : Rustre, compilateur minilustre vers Rust


## Utilisation

### Compiler le binaire

Le binaire `rustre` peut être compilé à l'aide de la commande `make` lancée à la racine du projet. La compilation a été testée avec ocaml 4.05.0 et menhir 20171206.

La commande `./rustre -help` devrait donner quelque chose comme :

```
usage: ./rustre [options] file.lus main
  -extract {why3|rust}Extract to why3 or rust
  -o File to write the generated code.
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
- `-opt` effectue des passes d'optimisation sut l'AST objet (voir plus bas).
- `-spec` et `-nils` sont deux analyses automatisées en Why3 (voir plus bas). À utiliser uniquement
  avec `-extract why3`.
- Si l'argument `-ext` est passé, la syntaxe concrète est étendue avec des automates (voir plus bas).

L'extraction Rust génère un fichier standalone qui peut être compilé avec la commande `rustc <file>`.

### Tester

Une série de fichiers d'exemples se trouve sous le dossier `tests`.
Taper la commande `make test` à la racine du projet lance la compilation de ces fichiers automatiquement
et vérifie qu'elle a lieu avec succès.


## Les différentes stades de la compilation

**TODO** mentionner quelque part : un AST par passe en gros.

### Syntaxe concrète

Nous avons essayé de nous rapprocher au plus de la syntaxe concrète décrite dans lctes08a.
En particulier, la condition du `if` et du `merge` ainsi que le terme de droite dans la
construction `when` doivent être des variables et non des expressions arbitraires.

De nouveaux types de données peuvent être déclarés avec la syntaxe `type direction = Right + Left` par exemple.

Des variables globales peuvent être déclarées en début de fichier après les définitions de types
et avant le premier nœud avec la syntaxe `const SOMETHING = 42`. Ces constantes sont substituées
dans le code dès le parsing à la manière des `#DEFINE` de C.

Les constructions `if`, `->` et `pre` sont traitées comme du sucre syntaxique dès le parsing tel que décris
dans l'article.

### Typage

**TODO** Lucas
- GADT => correct by construction.

### Analyse des horloges

Nous avons implémenté un clocking à la Hindley-Milner à l'aide de l'algorithme W.
Un nœud peut donc être polymorphe en termes d'horloge et peut donc être utilisé
plusieurs fois sur des horloges différentes. Par exemple :

```lustre
node add2(a, b: int) = (c: int)
with
  c = a + b ;
```

a pour horloge `('a, 'a) -> 'a`.

L'analogie avec ML est la suivante :

- Un nœud est traité comme une déclaration de fonction : on associe une variable
  d'horloge fraiche à chaque entrée et on unifie à l'aide des expressions et
  équations du nœud.
- Chaque dans les expressions, on unifie comme dans W.
- Chaque équation permet d'unifier les variables du pattern avec les horloges
  inférées pour l'expression à droit du signe `=`.

La correction du résultat est assurée par une deuxième passe sur l'AST qui se
effectue une vérification des horloges (par opposition à la première qui les
infère).

### Normalisation

**TODO** Lucas

La normalisation est telle que décrite dans l'article.

### Ordonnancement

L'ordonnancement est classique, on attend du programmeur qu'il
déclare les nœud dans l'ordre où il les utilise et on effectue un tri sur l'arbre des dépendances à l'intérieur de chaque nœud.

Une deuxième passe, de vérification, parcours linéairement l'AST
pour vérifier que toutes les variables utilisées on été calculées
au préalable.

### Traduction dans le langage objet

**TODO** Lucas

### Optimisations

En plus de la traduction vers le langage objet décrit dans l'article, nous avons implémenté deux optimisations dans ce dernier langage. Elles peuvent être déclencées
à l'aide de l'option `-opt`.

#### Fusion des merges

Dans le langage objet, lorsque deux `case` successifs sur la même variable sont générés, nous savons qu'aucun effet de bord n'empêche de fusionner les branches du `case` deux à deux.
En revanche le langage cible ne peut pas nécessairement voir cette propriété et ne peut donc pas effectuer la fusion.

Nous avons implémenté cette optimisation ce qui réduit le nombre de branchements, notamment dans les deux exemples `tests/emsoft03.lus` et `tests/emsoft05.lus`.

#### Simplification des merges triviaux

Un appel de nœud `f(x0, x1, …)` non suivi de la construction `every` dans la syntaxe
concrète est du sucre pour `f(x0, x1, …) every False`. Le code généré contient pour
cette raison un nombre important de `case` constants de la forme `case false { … }`.

Bien qu'on puisse attendre du compilateur du langage cible de simplifier ce code là automatiquement, nous avons implémenté cette optimisation afin de générer du code plus lisible.


## Extraction vers Why3 et preuves

### Sémantique

**TODO**

### post-conditions

**TODO**

### Analyses d'inits

**TODO**
