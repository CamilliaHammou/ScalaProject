package fr.esgi.al.funprog

@main
def Main(): Unit = {
  println("Ici le programme principal")
  println(Console.BLUE + "x")
  println(Console.RED + "x")
  println(Console.WHITE + "x")
  println(Console.GREEN + "x")

  // Le code suivant ne compilera pas.
  // var tmp = null;
  // var tmp2 = if (tmp == 1) "yes" else 1

  // println(s"tmp: $tmp, tmp2: $tmp2")
}
