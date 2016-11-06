object test99WorksheetSdw {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  

// Ex 01 - find the last value in a list

def last(k : List[Int]) : Int = {

 k.reverse.head
}                                                 //> last: (k: List[Int])Int

last(List(1, 2, 3))                               //> res0: Int = 3

  
  
}