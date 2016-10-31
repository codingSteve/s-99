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
def pack(l:List[Int]) = pack2(l)

def pack2(l:List[Int]) =
  l.foldLeft(List[List[Int]]()) {
    (z:List[List[Int]], e:Int) => z match {
      case Nil => List(List(e))
      case p :: t if p.head == e => (p :+ e) :: t
      case p :: t                => List(e) :: p :: t
    }
  }
//s-10
def encode(l:List[Int]): List[(Int, Int)] = encode1(l)

def encode2(l:List[Int]): List[(Int,Int)] = pack2(l).map(e => (size(e), e.head))

def encode1(l:List[Int]): List[(Int,Int)] = {
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

  //s-12
def decode(l:List[(Int, Int)]) =
  l.map( e  => e match { case (m, n) => for { _ <- (1 to m) } yield (n) } )

decode(encode(List(1,1,1,22,22,22,333,333,333)))

//s-13
def encodeDirect(l:List[Int]): List[(Int,Int)] = encode1(l)

//s-14
def duplicate(l:List[Int]): List[Int] = l.flatMap(e => e::e::Nil)

duplicate(l)

//s-15
def duplicateN(n:Int, l:List[Int]):List[Int] = for {
  e <- l
  i <- (1 to n)
} yield e
duplicateN(3, l)

//s-16
def drop(n:Int, l:List[Int]) = {
  def helper(m:Int, f:List[Int], b:List[Int]):List[Int] = {
    f match {
      case Nil => b
      case _   => m match {
          case 1 => helper(n, f.tail, b)
          case _ => helper(m-1, f.tail, f.head :: b )
        }
    }
  }
  reverse(helper(n,l,List()))
}

drop(3,l)


//s-17
def split(n:Int, l:List[Int]): (List[Int], List[Int]) = {
  def helper(m: Int, f: List[Int], b: List[Int]): (List[Int], List[Int]) = m match {
    case 0 => (reverse(f), reverse(b))
    case _ => helper(m - 1, f.tail, f.head :: b)
  }
  helper(n, l, List())
}

//s-18
def slice(m:Int, n:Int, l:List[Int]):List[Int] = split(n-m,split(m,l)._1)._1

//s-19
def rotate(n:Int, l:List[Int]):List[Int] = n match  {
  case 0          => l
  case i if n > 0 => rotate(n-1, l.tail :+ l.head)
  case j if n < 0 => reverse( rotate( -j, reverse( l ) ) )

}
