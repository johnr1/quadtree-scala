
object Main extends App {
  if(Tests.testInsertions()){
    println("Passed insertion test")
  } else {
    println("!!! Insertion Tests failed. Structure not correct !!!")
  }

  if(Tests.testRemovals()){
    println("Passed removal test")
  } else {
    println("!!! Removal Tests failed. Structure not correct !!!")
  }

//  if(Tests.testRangeSearch()){
//    println("Passed rangeSearch test")
//  } else {
//    println("!!! RangeSearch Tests failed. Structure not correct !!!")
//  }
}
