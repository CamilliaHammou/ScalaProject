# Projet AL

## Pré-requis

Il est indispensable d'avoir installé en local:

- le compilateur Scala, [ici](https://scala-lang.org/download/) (voir version dans `build.sbt`)

- le gestionnaire de build `sbt`, [voir ici](https://www.scala-sbt.org/download.html). En installant `sbt`, le compilateur sera installé aussi.

## Structure du projet

Ceci est le projet de validation du cours d'initiation à la programmation fonctionnelle. Le code source doit être écrit dans le répertoire `./src/main/scala`. Vous pourrez créer autant de package que vous voulez.
Les tests unitaires doivent être écrit dans le répertoire `./src/test/scala`. Pour écrire des tests unitaires, veuillez vous reporter à la section [Tests Unitaires](#tests-unitaires).

## Guide de survie avec sbt

Ce projet est un application Scala standalone. Il est géré par `sbt`, le build tool Scala. Sa documentation est disponible [ici](https://www.scala-sbt.org/1.x/docs/).

Nous allons lister ici une liste de commandes utiles avec `sbt`:

- `sbt`: cette commande lance un invite de commande interactif

- `run` (ou `sbt run` hors de l'invite de commande): lance le `Main` du projet `sbt`

- `compile` (ou `sbt compile` hors de l'invite de commande): lance la compilation de l'ensemble du projet `sbt` (compile toutes les classes)

- `console` (`sbt console` hors de l'invite de commande): lance un REPL interactif Scala. Les dépendances du projet `sbt` seront disponibles et pourront être importés.

## Manipulation de fichiers

Nous allons voir ici quelques commandes pour vous aider avec la manipulation de fichiers en `Scala`. 

Pour lire un fichier nous pouvons le faire comme suit (en utilisant la lib [better-files](https://github.com/pathikrit/better-files)):

```scala
import better.files._

val f = File("/User/johndoe/Documents") // using constructor

// on va récupérer toutes les lignes du fichier
f.lines.toList

// si on veut récupérer tout le contenu du fichier en String
f.contentAsString
```

Pour écrire dans un fichier, nous pouvons le faire ainsi:

```scala
import better.files._

val f = File("/User/johndoe/Documents") // using constructor

// pour ajouter du contenu dans un fichier ligne par ligne
f.createIfNotExists()
  .appendLine() // on rajoute une ligne vide
  .appendLines("My name is", "Inigo Montoya") // on ajoute 2 nouvelles lignes

// pour écraser le contenu du fichier
f.createIfNotExists().overwrite("hello")
```

## Tests unitaires

Il est possible de lancer tous les tests du projet avec la commande: `sbt test` (ou `test` si on est dans l'invite de commande `sbt`).

Pour créer une classe de test, il suffit de créer une classe étendant `munit.FunSuite`:

```scala
package example

class MyTests extends munit.FunSuite {
  ???
}
```

Les tests devant être lancés doivent être placés dans le corps de la classe. Pour créer un test, il suffit d'appeler `test` en lui passant un nom de test et le code de test à effectuer comme ceci:

```scala
package example

class MyTests extends munit.FunSuite {

  // On rajoute un cas de test
  test("sum of two integers") {
    val obtained = 2 + 2
    val expected = 4
    assertEquals(obtained, expected)
  }

  test("all even numbers") {
    val input: List[Int] = List(1, 2, 3, 4)
    val obtainedResults: List[Int] = input.map(_ * 2)
    // check that obtained values are all even numbers
    assert(obtainedResults.forall(x => x % 2 == 0))
  }

}
```

`MUnit` propose plusieurs méthodes d'assertions disponibles dans sa [documentation](https://scalameta.org/munit/docs/assertions.html): `assertNotEquals`, `assertNoDiff`, `fail`, and `compileErrors`.

Le test sera lancé dès lorsqu'on lancera la commande `test`:

```scala
sbt:funprog-AL> test
example.StringSuite:
  + CHaine vide est vide 0.004s
  + Chaine  0.0s
example.HelloSuite:
  + The Hello object should say hello 0.004s
  + Hello size is equals to 5 0.0s
  +  "Hello"(6) should throw a "java.lang.StringIndexOutOfBoundsException"  0.0s
[info] Passed: Total 5, Failed 0, Errors 0, Passed 5
[success] Total time: 1 s, completed 11 avr. 2024 à 22:39:37
```

Une classe de test d'exemple vous est fourni dans `./src/test/example/HelloSuite.scala`.
