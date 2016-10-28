val l: List[Int] = List(1,2,3,4,5)


l.tail

def last(l:List[Int]):  Int  = {
  l match {
    case (h :: Nil ) => h
    case (_ :: t)    => last(t)
  }
}

def penultimate(l:List[Int]):Int = l match {
  case ( p :: l :: Nil ) => p
  case ( h :: t) => penultimate(t)
}

def nth(n:Int, l:List[Int]):Int = n match {
  case ( 0 ) => l.head
  case ( _ ) => nth( n-1, l.tail)
}

def size(l:List[Int]): Int = {
  l.fold(0) ((z,_) => z+1 )
}

def reverse(l:List[Int]) : List[Int] = l match {
  case ( h :: Nil ) => l
  case ( h :: t   ) => reverse(t) :+ h
  case ( _        ) => List[Int]()
}


//s-06
def isPalindrome(l:List[Int]): Boolean = l.equals(reverse(l))

//s-07
def flatten(l:List[List[Int]]): List[Int] = {
  for {
    sublist <- l
    e       <- sublist
  } yield e
}

//s-08
def compress(l:List[Any]): List[Any]  = l match {
  case Nil                 => l
  case h :: Nil            => h :: Nil
  case x :: y :: t if x==y => compress(x :: t)
  case x :: y :: t         => x :: compress(y :: t)
}

//s-09
def pack(l:List[Int]) =
  l.foldLeft(List[List[Int]]()) {
    (z:List[List[Int]], e:Int) => z match {
      case Nil => List(List(e))
      case p :: t if p.head == e => (p :+ e) :: t
      case p :: t                => List(e) :: p :: t
    }
  }
//s-10
def encode(l:List[Int]): List[(Int,Int)] = {
  l.foldLeft(List[(Int,Int)]()) {
    (z: List[(Int,Int)], e:Int) => z match {
      case Nil => List((1,e))
      case (n, f) :: t if f==`e` => (n+1, f) :: t
      case (n, f) :: t           => (1, e) :: (n, f) :: t
    }
  }
}

//s-11
def encodeModified(l:List[Int]): List[Any] =
  encode(l).map(e => e match {
      case (1, c ) => c
      case _       => e
    }
  )



