package adpro

import Reduce.reduce

sealed trait Node[+A] {

  import Node.reduceNode

  // convenience delegations

  def toList: List[A] =
    reduce[Node].toList[A] (this)
}

case class Node2[A] (l: A, r: A) extends Node[A]
case class Node3[A] (l: A, m: A, r: A) extends Node[A]

object Node {

  // Exercise 10

  implicit lazy val reduceNode: Reduce[Node] = new Reduce[Node]{
    def reduceR[A, B] (opr: (A,B) => B) (fa: Node[A], b: B): B = fa match {
      case Node2(l,r)   => opr(l, opr(r, b))          //b op r op l
      case Node3(l,m,r) => opr(l, opr(m, opr(r, b)))  //b op r op m op l
    } 
      
    def reduceL[A, B] (opl: (B,A) => B) (b: B, fa: Node[A]): B = fa match {
      case Node2(l,r)   => opl(opl(b, l), r)         //b op l op r
      case Node3(l,m,r) => opl(opl(opl(b, l), m), r) //b op l op m op r
    }  
  }
}
