
object Main extends App {

  if(Tests.testInsertions()){
    println("Passed insertion test")
  } else {
    println("!!! Tests failed. Structure not correct !!!")
  }

  if(Tests.testRemovals()){
    println("Passed removal test")
  } else {
    println("!!! Tests failed. Structure not correct !!!")
  }

}
