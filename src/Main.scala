object Main extends App {
  def runTest( test: () => Boolean, testTitle: String ): Unit = {
    if (test())
      println("Passed " + testTitle + " Test")
    else
      println("!!! " + testTitle + " Tests failed. Structure not correct !!!")
  }

  val tests = Array(
    (Tests.testBuild _ , "Build"),
    (Tests.testInsertions _ , "Insertion"),
    (Tests.testRemovals _ , "Removal"),
    (Tests.testUpdates _ , "Update"),
    (Tests.testRangeSearch _ , "Range Search"),
  )

  for(_ <- 0 to 2) {
    for (t <- tests) {
      runTest(t._1, t._2)
    }
  }
}
