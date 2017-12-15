# Rustre, un compilateur vérifiable, vérifié et vérifiant de minilustre vers Rust

intro

- but : écrire un compilateur de minilustre vers Rust
- vérification sémantique partielle et fonctions de vérification
- extraction vers why3 permet aussi de faire de la vérification
- exemple : pong
- manuel dans README.md

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


Chaque nœud est encapsulé dans un module, où est décrit :

- une `struct Machine`, décrivant la mémoire et les instances du nœud courant. En utilisant `#[derive(Default)]` avant la déclaration de `struct Machine`, le compilateur Rust génère automatiquement une procédure d'initialisation pour la structure.
- une méthode `step` fonctionnant sur `Machine`, qui est définie de manière similaire au langage objet. Grâce aux transformations effectuées précédemment, toutes les variables locales de cette méthode sont immuables.
- une méthode `reset` opérant sur `Machine`, réinitialisant la mémoire et les instances du nœud.


Ensuite, l'extraction définit une fonction `parse_args` qui demande à l'utilisateur les arguments nécessaires à l'exécution d'une étape de `main_node`.
La fonction `main` est une boucle infinie. Celle-ci appelle `parse_args`, envoie le résultat au nœud principal `main_node`, affiche le résultat du nœud principal et recommence.

## Extraction vers Why3 et preuves

### Sémantique

**TODO**

### post-conditions

**TODO**

### Analyses d'inits

**TODO**

## Extension avec les automates hiérarchiques 

Nous avons essayé d'étendre le langage avec les constructions `reset`, `match` et 
`automata` décrites dans emsoft05b.
Le manque de temps nous a obligé à traiter ces constructions directement sur
l'AST de parsing, au détriment d'une gestion correcte des erreurs. Les
constructions `match` et `reset` sont implémentées, mais pas encore les
automates.

L'approche utilisée consiste en une passe par construction, plutôt qu'une
transformation générale comme dans l'article. On élimine d'abord les
constructions `automata` en les transformant en programmes utilisant
uniquement `match` et `reset`, puis on élimine successivement les `match` et
les `reset`.

Par rapport à l'article sont également ajoutées les déclaration des variables
partagées avec le mot-clé `shared`, ainsi qu'une valeur initiale optionnelle
pour celles-ci (permettant que `last x` soit bien définie au premier instant).
Pour être compatible avec le langage de base, nous nous sommes un peu éloigné
de la syntaxe proposée dans l'article : les déclarations de variables
locales sont limitées, et les horloges ne sont pas explicitement déclarées.
