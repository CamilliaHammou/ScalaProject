package fr.esgi.al.funprog.tiling

object TilingCalculator {

  def displayTilingPossibilities(n: Int, m: Int): Unit = {
    val count = countTilings(n, m, Set.empty)
    println(s"Possibilités d'affichage pour le panneau $n x $m: $count")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  private def countTilings(n: Int, m: Int, filled: Set[(Int, Int)]): BigInt = {
    val totalCells = n * m
    if (filled.sizeIs == totalCells) {
      BigInt(1)
    } else {
      val (x, y) = findFirstFree(n, m, filled)

      val horizontal = if (y + 1 < m && !filled((x, y + 1))) {
        countTilings(n, m, filled ++ Set((x, y), (x, y + 1)))
      } else BigInt(0)

      val vertical = if (x + 1 < n && !filled((x + 1, y))) {
        countTilings(n, m, filled ++ Set((x, y), (x + 1, y)))
      } else BigInt(0)

      horizontal + vertical
    }
  }

  private def findFirstFree(
      n: Int,
      m: Int,
      filled: Set[(Int, Int)]): (Int, Int) = {
    val allCells = for {
      x <- 0 until n
      y <- 0 until m
    } yield (x, y)

    allCells.find(c => !filled(c)).getOrElse((0, 0))
  }
}
