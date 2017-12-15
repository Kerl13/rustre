# Rustre, un compilateur vérifiable, vérifié et vérifiant de minilustre vers Rust

intro

- but : écrire un compilateur de minilustre vers Rust
- vérification sémantique partielle et fonctions de vérification
- extraction vers why3 permet aussi de faire de la vérification
- exemple : pong
- court manuel à la fin

## Les différents stades de la compilation

Le processus de compilation est décomposé en plusieurs passes : le code est parsé, puis typé, puis les horloges sont inférées, et ensuite le code est normalisé, ordonnancé, transformé vers le langage objet, puis extrait vers Rust ou Why3.
Lors de la plupart des passes, les AST d'entrée et de sortie sont différents.

### Syntaxe concrète

Nous avons essayé de nous rapprocher au plus de la syntaxe concrète décrite dans lctes08a.
En particulier, la condition du `if` et du `merge` ainsi que le terme de droite dans la
construction `when` doivent être des variables et non des expressions arbitraires.

De nouveaux types de données peuvent être déclarés avec la syntaxe `type direction = Right + Left` par exemple.

Des variables globales peuvent être déclarées en début de fichier après les définitions de types
et avant le premier nœud avec la syntaxe `const SOMETHING = 42`. Ces constantes sont substituées
dans le code dès le parsing à la manière des `#DEFINE` de C.

Les constructions `if`, `->` et `pre` sont traitées comme du sucre syntaxique dès le parsing tel que décrit
dans l'article.

### Typage

Le typage est assez standard. Il est presque explicite (à l'exception des constantes ou des opérations arithmétiques qui demandent un peu d'inférence). Le typage se rapproche donc d'une passe de vérification assez stable.

Pour augmenter la confiance que nous avions en cette phase (et dans une moindre mesure en toutes les autres passes qui suivent), nous avons décidé d'implémenter l'AST typé par un GADT OCaml.

Nulle fonction de vérification n'est donc nécessaire, les AST typés étant nécessairement correct vis à vis des types par construction.

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
  d'horloge fraîche à chaque entrée et on unifie à l'aide des expressions et
  équations du nœud.
- Dans les expressions, on unifie comme dans W.
- Chaque équation permet d'unifier les variables du pattern avec les horloges
  inférées pour l'expression à droite du signe `=`.

La correction du résultat est assurée par une deuxième passe sur l'AST qui
effectue une vérification des horloges (par opposition à la première qui les
infère).

### Normalisation

La normalisation est telle que décrite dans l'article. On utilise un nouveau
type d'AST. L'AST après cette passe est donc nécessairement en forme normale.

### Ordonnancement

L'ordonnancement est classique, on attend du programmeur qu'il
déclare les nœuds dans l'ordre où il les utilise et on effectue un tri sur l'arbre des dépendances à l'intérieur de chaque nœud.

Une deuxième passe, de vérification, parcourt linéairement l'AST
pour vérifier que toutes les variables utilisées on été calculées
au préalable.

### Optimisations

En plus de la traduction vers le langage objet décrit dans l'article, nous avons implémenté deux optimisations dans ce dernier langage.
Elles peuvent être déclenchées à l'aide de l'option `-opt`.

#### Fusion des merges

Dans le langage objet, lorsque deux `case` successifs sur la même variable sont générés, nous savons qu'aucun effet de bord n'empêche de fusionner les branches du `case` deux à deux.
En revanche le langage cible ne peut pas nécessairement voir cette propriété et ne peut donc pas effectuer la fusion.

Nous avons implémenté cette optimisation ce qui réduit le nombre de branchements, notamment dans les deux exemples `tests/emsoft03.lus` et `tests/emsoft05.lus`.

#### Simplification des merges triviaux

Un appel de nœud `f(x0, x1, …)` non suivi de la construction `every` dans la syntaxe
concrète est du sucre pour `f(x0, x1, …) every False`. Le code généré contient pour
cette raison un nombre important de `case` constants de la forme `case false { … }`.

Bien qu'on puisse attendre du compilateur du langage cible de simplifier ce code là automatiquement, nous avons implémenté cette optimisation afin de générer du code plus lisible.

### Traduction dans le langage objet

La traduction vers le langage objet est réalisée comme décrit dans l'article. On notera
simplement que l'AST objet est aussi réalisé avec des GADT, ce qui donne certaines garanties sur le typage.

## Extraction vers Rust

L'extraction vers Rust se fait en un parcours linéaire sur l'AST objet.


Chaque noeud est encapsulé dans un module, où est décrit :

- une `struct Machine`, décrivant la mémoire et les instances du noeud courant. En utilisant `#[derive(Default)]` avant la déclaration de `struct Machine`, le compilateur Rust génère automatiquement une procédure d'initialisation pour la structure.
- une méthode `step` fonctionnant sur `Machine`, qui est définie de manière similaire au langage objet. Grâce aux transformations effectuées précédemment, toutes les variables locales de cette méthode sont immuables.
- une méthode `reset` opérant sur `Machine`, réinitialisant la mémoire et les instances du noeud.


Ensuite, l'extraction définit une fonction `parse_args` qui demande à l'utilisateur les arguments nécessaires à l'exécution d'une étape de `main_node`.
La fonction `main` est une boucle infinie. Celle-ci appelle `parse_args`, envoie le résultat au nœud principal `main_node`, affiche le résultat du noeud principal et recommence.

## Extraction vers Why3 : preuve de la compilation et vérification de code Lustre

Why3 est une plateforme de vérification déductive comportant deux langages : un
langage de programmation, WhyML, proche de OCaml et un langage logique qui permet
de raisonner sur les programmes.

Notre extraction vers Why3 a plusieurs buts : d'abord, on extrait vers un langage
particulièrement sûr. Ensuite, elle permet d'établir une preuve d'abstraction
entre le code séquentiel et le nœud Lustre originel, prouvant ainsi tout le
processus de compilation. (Par manque de temps, seul un noyau du langage est
supporté pour cette partie.) Enfin, on peut utiliser Why3 pour faire de la
vérification sur nos programmes.


L'extraction vers Why3 peut être séparée en trois parties  : la production de
code exécutable séquentiel, la spécification de ce code, puis la traduction du
nœud Lustre initial en une spécification de haut niveau.

##### Code exécutable séquentiel

La production de ce code est semblable à l'extraction vers Rust. Il s'agit
d'un code séquentiel qui met à jour en place des record pour modifier l'état.
Ce code peut ensuite être extrait vers OCaml et donne donc du code efficace.


##### Spécification logique

Le code séquentiel qu'on a produit l'a été dans le langage de programmation
WhyML. Il n'est pas pur car il agit par effet de bord sur l'état.
Pour pouvoir raisonner dessus, il faut exprimer une spécification dans le
langage logique.

##### Spécification abstraite

Indépendamment, on effectue une traduction très simple (et donc dans laquelle on
peut avoir confiance) d'un nœud Lustre vers une spécification abstraite dans
Why3 en terme de flots.

Nous avons axiomatisé ces flots dans une bibliothèque Why3. Un flot est un élément du
type `stream 'a` avec une fonction d'accès `get` :

```why3
type stream 'a
type nat = O | S nat
function get (stream 'a) nat: 'a
(* axiome d'extensionnalité *)
axiom sext: forall a, b: stream 'a.
    (forall n: nat. get a n = get b n) -> a = b
```

Ensuite, on définit toutes les opérations possibles, par exemple la somme de deux
flots ou le fby via des règles de réécriture (le reste est dans why3/stream.mlw) :

```why3
function sfby 'a (stream 'a): stream 'a
function splus (stream int) (stream int): stream int
axiom sfby_rw_s: forall a:'a, b:stream 'a, n. get (sfby a b) (S n) = get b n
axiom sfby_rw_o: forall a:'a, b:stream 'a. get (sfby a b) O = a
axiom splus_rw: forall a, b, n. get (splus a b) n = get a n + get b n
```

La traduction est très simple, il s'agit de traduire les équations en une conjonction
d'égalités logiques.

On note que cette traduction est réalisée depuis le premier AST, ainsi on n'a
pas besoin de faire confiance aux autres passes de compilation.

Par exemple, le nœud suivant :

```lustre
node add(a, b: int) = (c, d: int)
with
  c = a + b ;
  d = 0 fby c
```

est traduit par la spécification Why3 suivante :

```why3
predicate spec (a:stream int) (b:stream int) (c:stream int) (d:stream int) =
  c = (splus a b)   /\
  d = (sfby 0 c)
```

##### Preuve sémantique

On cherche ensuite à prouver que notre code séquentiel est une abstraction de cette
spécification. Formellement, cela revient à définir par induction des flots et à
montrer qu'il satisfont le prédicat `spec`.

On appelle `spec_fonct` la contrainte fonctionnelle du code séquentiel, qui est
une postcondition du code exécutable.

Ainsi dans l'exemple précédent, le lemme qu'on cherche à montrer est le suivant :
```why3
lemma valid:
  forall (* in and out vars *) a:stream int,  b:stream int,  c:stream int,
  d:stream int,  (* state *) sd: stream int.  
  (* definition by recurrence *)
  ({ Nodeadd.d = get sd O; } = reset_state /\
  forall n: nat.
    step_fonct (get a n)  (get b n)  (get c n)
      (get d n) { Nodeadd.d = get sd n; } { Nodeadd.d = get sd (S n); })
  (* correction *)
  -> spec a  b c  d
```

On note qu'on a besoin de supposer l'existence d'un flot supplémentaire pour l'état,
qui n'apparaît pourtant pas dans `spec`. Ce lemme définie bien des flots valides :
ils sont définissables par récurrence car le code exécutable satisfait `step_fonct`.

Prouver ce lemme s'est avéré être particulièrement difficile. Je pensais que sur des
exemples simples les solveurs automatiques SMT ou ATP devait pouvoir fournir des
preuves. Ce n'est pas le cas, 



##### Limitation de la preuve sémantique

Nous n'avons réalisé ce travail de preuve que sur un sous-ensemble de minilustre qu'on
a veillé à garder assez expressif. Ainsi, la syntaxe `every`, les types sommes (autre
que booléens), les nils (difficiles à axiomatiser) et les variables locales (qui ne sont
fondamentalement pas une grande difficulté mais s'exprime avec des quantificateurs
existentiels ce qui rend l'exercice assez technique).

Cela laisse tout de même les
`merge`, les `fby`, les appels de nœuds, un sous ensemble complet d'opérations
arithmétiques et booléennes.


### Sémantique

**TODO**

### post-conditions

**TODO**

### Analyses d'inits

**TODO**



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
- `-opt` effectue des passes d'optimisation sur l'AST objet (voir plus bas).
- `-spec` et `-nils` sont deux analyses automatisées en Why3 (voir plus bas). À utiliser uniquement
  avec `-extract why3`.
- Si l'argument `-ext` est passé, la syntaxe concrète est étendue avec des automates (voir plus bas).

**TODO: versions de why3 ?**

L'extraction Rust génère un fichier standalone qui peut être compilé avec la commande `rustc <file>`.
La compilation Rust a été testée avec rustc 1.22.1.

### Tester

Une série de fichiers d'exemples se trouve sous le dossier `tests`.
Des exemples de programmes non valides sont présents dans le dossier `tests/bad`.
Taper la commande `make test` à la racine du projet lance la compilation de ces fichiers automatiquement et vérifie qu'elle a lieu avec succès sur les programmes valides, et qu'elle échoue sur les programmes non valides.
