package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 1.
 */
object ADNMatchV1 extends App {

  println("ADNMatch Version 1")
  
  val bases1 : List[Base] = List(A,T,C,G)
  
  val bases2 : List[Base] = List(T,T,G,G,C,C,A,A)
  
  val bases3 : List[Base] = List(G,C,A,T)


  //dwadawdaw/d/aw
  
  val rexp1  : RExp = Repete(UneBase(T))
  
  val rexp2  : RExp = Choix(UneBase(T),UneBase(G))
  
  val rexp3  : RExp = Concat(UneBase(A),Concat(UneBase(T),Concat(UneBase(C),UneBase(G))))
  
  
  //test
  /*
  println(listeBasesToString(bases2))
  println(listeBasesToString(bases3))
  println()
  println(rExpToString(rexp2))
  println(rExpToString(rexp3))
  println(litRExp(rExpToString(rexp2)))
  println(litRExp(rExpToString(rexp3)))
  println()
  
  */
  
}














