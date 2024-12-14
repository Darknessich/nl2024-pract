object Solution {
    val directions = Array(
        (-2,  1), (-1,  2),
        ( 1,  2), ( 2,  1),
        ( 2, -1), ( 1, -2),
        (-1, -2), (-2, -1)
    )

    def isCorrect(i: Int, j: Int, n: Int): Boolean = {
        i >= 0 && j >= 0 && i < n && j < n
    }

    def knightProbability(n: Int, k: Int, row: Int, column: Int): Double = {
        var dp = Array.fill(2, n, n)(0.0)
        dp(1)(row)(column) = 1.0

        var moves = k
        while (moves > 0) {
            for (i <- 0 until n) {
                for (j <- 0 until n) {
                    dp(0)(i)(j) = 0.0
                    for ((di, dj) <- directions) {
                        val ni = i + di
                        val nj = j + dj
                        if (isCorrect(ni, nj, n)) {
                            dp(0)(i)(j) += 0.125 * dp(1)(ni)(nj)
                        }
                    }
                }
            }
            val temp = dp(0)
            dp(0) = dp(1)
            dp(1) = temp

            moves -= 1
        }

        dp(1).map(_.sum).sum
    }

    def main(args: Array[String]): Unit = {
        // Test case 1
        val result1 = knightProbability(3, 2, 0, 0)
        if (math.abs(result1 - 0.0625) < 1e-6) println("Test 1: Success") else println(s"Test 1: Failed (got $result1, expected 0.0625)")

        // Test case 2
        val result2 = knightProbability(1, 0, 0, 0)
        if (math.abs(result2 - 1.0) < 1e-6) println("Test 2: Success") else println(s"Test 2: Failed (got $result2, expected 1.0)")

        // Test case 3
        val result3 = knightProbability(8, 30, 6, 4)
        if (math.abs(result3 - 0.00019052566298333645) < 1e-6) println("Test 3: Success") else println(s"Test 3: Failed (got $result3, expected ~0.00019)")
    }
}
