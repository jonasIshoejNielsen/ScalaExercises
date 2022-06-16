package adpro

import Digit._
import Reduce.reduce

sealed trait FingerTree[+A] {

  // Exercise 14 (first part)

  def ▷ [B >: A] (b: B): FingerTree[B] = addL(b)    //a ▷ tree

  def ◁ [B >: A] (b: B): FingerTree[B] = addR(b)    //tree ◁ a


  // Delegations for convenience

  def addL[B >: A] (b: B): FingerTree[B] =
    FingerTree.addL (b, this)

  def addR[B >: A] (b: B): FingerTree[B] =
    FingerTree.addR (this, b)

  def toList: List[A] =
    FingerTree.reduceTree.toList (this)

  def headL: A =
    FingerTree.headL (this)

  def tailL: FingerTree[A] =
    FingerTree.tailL (this)

  def headR: A =
    FingerTree.headR (this)

  def tailR: FingerTree[A] =
    FingerTree.tailR (this)

  // A quick size

  def size: Int = this.toList.size

  // We implement empty/nonEmpty without using views or patterns, thanks to
  // dynamic dispatch in Scala (match simpler, IMHO).  Haskell has no dynamic
  // dispatch, because it has no inheritance.

  def empty = false
  def nonEmpty = true
}

case class Empty () extends FingerTree[Nothing] {

  override def empty = true
  override def nonEmpty = false
}

case class Single[A] (data: A) extends FingerTree[A]

// pr - prefix, m - middle, sf - suffix
case class Deep[A] (

  pr: Digit[A],
  m: FingerTree[Node[A]],   //single[A] -> Single[Node[A]]   Exercise 9
  sf: Digit[A]

) extends FingerTree[A]



object FingerTree {

  // Exercise 11

  implicit def reduceTree: Reduce[FingerTree] = new Reduce[FingerTree] {

    def reduceR[A, B] (opr: (A,B) => B) (fa: FingerTree[A], b: B): B = fa match {
      case Empty ()            => b 
      case Single(a)           => opr(a,b)
      case Deep(pr, m, t:+h)   => reduceR[A,B](opr)(Deep(pr,m,t), opr(h,b))     //lst::h
      case Deep(pr, m, List()) =>{
        val b2: B = reduceR[Node[A],B] ((node,b) => Node.reduceNode.reduceR(opr)(node, b)) (m, b) 
        Reduce.reduceList.reduceR(opr)(pr, b2)
      }
    }

    def reduceL[A, B] (opl: (B,A) => B) (b: B, fa: FingerTree[A]): B =  fa match {
      case Empty ()            => b 
      case Single(a)           => opl(b,a)
      case Deep(h::t, m, ps)   => reduceL[A,B](opl)(opl(b,h), Deep(t,m,ps)) 
      case Deep(List(), m, ps) => {
        val b2: B = reduceL[Node[A],B] ((b,node) => Node.reduceNode.reduceL(opl)(b, node)) (b, m) 
        Reduce.reduceList.reduceL(opl)(b2, ps)
      }
    }
  }



  // Exercise 14  (second part)

  implicit class LeftFingerTreeOps[A]  (a: A) {
    def ◁ [B >: A] (t: FingerTree[B]): FingerTree[B] = ???
  }



  // Exercise 12 (page 5)

  def addL[A] (a: A, t: FingerTree[A]): FingerTree[A] = t match{
    case Empty ()       => Single(a)
    case Single(b)      => Deep(List(a), Empty(), List(b))    //[a,b]
    case Deep(List(b,c,d,e), m, sf)  =>
      Deep(List(a,b), addL(Node3(c,d,e), m), sf)    //polymorphic
    case Deep(pr, m, sf) => Deep(a::pr, m, sf)


  }

  def addR[A] (t: FingerTree[A], a: A): FingerTree[A] =  t match{
    case Empty ()       => Single(a)
    case Single(b)      => Deep(List(b), Empty(), List(a))
    case Deep(pr, m, List(e,d,c,b))  =>
      Deep(pr, addR(m, Node3(e,d,c)), List(b,a))    //polymorphic
    case Deep(pr, m, sf) => Deep(pr, m, sf :+ a)


  }



  // Exercise 15
  
  def toTree[F[_]: Reduce, A] (fa:  F[A]): FingerTree[A] = {
    val r   = Reduce.reduce[F]
    val lst = r.toList(fa)
    lst.foldLeft[FingerTree[A]](Empty())((acc,a) => FingerTree.addR(acc,a))
  }



  // To be implemented as part of Exercise 19 (page 6)    //todo do part 2 of exercise 19

  def deepL[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
    : FingerTree[A] = ???

  // To be implemented as part of Exercise 19

  def deepR[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
    : FingerTree[A] = ???



  // Easy to use convenience wrappers around matchers

  def headL[A] (t: FingerTree[A]): A =
    t match {
      case ConsL(h,_) => h
    }

  def tailL[A] (t: FingerTree[A]): FingerTree[A] =
    t match {
      case ConsL(_,t) => t
    }

  def headR[A] (t: FingerTree[A]): A =
    t match {
      case ConsR(_,h) => h
    }

  def tailR[A] (t: FingerTree[A]): FingerTree[A] =
    t match {
      case ConsR(t,_) => t
    }

}


// Exercise 18

// In the paper views are generic in the type of tree used. Here I make them
// fixed for FingerTrees.

// sealed trait ViewL[+A]
// case object Nil extends ViewL[Nothing]
// case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]




// Exercise 19 (page 6, using Scala extractors)

object Nil {
  def unapply[A] (t: FingerTree[A]): Boolean = t match {
    case Empty () => true
    case _ => false
  }
}

object ConsL {    //todo explain changes
  def unapply[A] (t: FingerTree[A]): Option[(A, FingerTree[A])] = t match {
    case Empty()             => None
    case Single(a)           => Some((a,Empty()))
    case Deep(h::t, m, sf)   => Some((h, Deep(t,m,sf)))     //lst::h
    case Deep(List(), m, sf) => unapply[Node[A]](m) match  {
      case None       => 
        sf match {
          case List()        => None
          case List(a)       => Some((a,Empty()))
          case List(a,b)     => Some((a,Single(b)))
          case List(a,b,c)   => Some((a, Deep(List(b),Empty(),List(c))))
          case List(a,b,c,d) => Some((a, Deep(List(b,c),Empty(),List(d))))
        }
      case Some((Node2(a,b),t))    => Some((a,Deep(List(b),   t, sf)))
      case Some((Node3(a,b,c),t))  => Some((a,Deep(List(b,c), t, sf)))
    }
  }
}

object ConsR {
  def unapply[A] (t: FingerTree[A]): Option[(FingerTree[A], A)] = t match {
    case Empty()             => None
    case Single(a)           => Some((Empty(), a))
    case Deep(pr, m, t:+h)   => Some((Deep(pr,m,t),h))     //lst::h
    case Deep(pr, m, List()) => unapply[Node[A]](m) match  {
      case None       => 
        pr match {
          case List()        => None
          case List(a)       => Some((Empty(), a))
          case List(b,a)     => Some((Single(b), a))
          case List(c,b,a)   => Some((Deep(List(c),Empty(),List(b)), a))
          case List(d,c,b,a) => Some((Deep(List(d),Empty(),List(c,b)), a))
        }
      case Some((t, Node2(a,b)))    => Some((Deep(pr, t, List(a)),   b))
      case Some((t, Node3(a,b,c)))  => Some((Deep(pr, t, List(a,b)), c))
    }
  }
}



/*
//Exercise 1
  1: [3,2,1]
  2: [3,2,1,4,5]
  3: [1,4]

//Exercise 2.1
Single(1)
Deep([2], Empty, [1])

//Exercise 2.2
Deep([5,4,3,2], Empty, [1])

//Exercise 2.3
Deep([6,5], Single(Node3(4,3,2)), [1])

//Exercise 2.4
Deep([9,8,7,6,5], Single(Node3(4,3,2)), [1]) //becomes:
Deep([9,8], Deep([7,6,5], Empty, [4,3,2]), [1])

//Exercise 2.5
Deep([9,8], Deep([7,6,5], Empty, [4,3,2]), [1, 10])

//Exercise 2.6
Deep([9,8], Deep([7,6,5], Empty, [4,3,2]), [1]), 10
Deep([9,8], Deep([7,6,5], Empty, [4,3]), [2]), 1

//Exercise 2.7
Deep([9,8], Deep([7,6,5], Empty, [4]), [3]), 2

Deep([9,8], [7,6,5].toTree, [4]), 3
Deep([9,8], Single(Node3(7,6,5)), [4]), 3 

//Exercise 3
//Exercise 3.1
reducer (+) [1,2,3,4] 0 == 10

//Exercise 3.2
reducel (+) 0 [1,2,3,4] == 10

//Exercise 3.3
reducer (+) (Node3 1 2 3) 0 == 6

//Exercise 3.4
reducel (+) 0 (Deep [1,2] (Single (Node3 3 4 5)) [0]) == 15

//Exercise 3.5
reducer (:) (Deep [1,2] (Single (Node3 3 4 5)) [0]) [] == [0,5,4,3,2,1]

//Exercise 3.6
reducel (:) [] (Deep [1,2] (Single (Node3 3 4 5)) [0]) == [1,2,3,4,5,0]

//Exercise 4
//Exercise 4.1
Cons(1,Cons(2,Cons(3,Nill)))

//Exercise 4.2
Nil

//Exercise 4.3
Node2(1,2)

//Exercise 4.4
Node2[Node[Int]](Node2(1,2), Node2(3,4))

//Exercise 4.5
ft2: FingerTree[Int] = Single(Node3(3,4,5))
Deep[Int]([1,2], ft2, [0] ) 


//Exercise 5
//Exercise 5.1
def addL[A](v:A) (ft: FingerTree[A]): FingerTree[A] = ???
  //A=> FingerTree[A]=>FingerTree[A]

//Exercise 5.2
def addLprime[A] (v: F[A]) (ft: FingerTree[A]): FingerTree[A] = ???

//Exercise 5.3
def toTree[A] (v: F[A]): FingerTree[A] = ???

//Exercise 5.4
def viewL[A](ft: FingerTree[A]): ViewL[FingerTree[A]] = ???

//Exercise 5.5
def deepL[A] (lst: List[A]) (ft: FingerTree[A]): ViewL[FingerTree[A]] = ???


//Exercise 6
//done








*/