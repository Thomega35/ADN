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
  
  val bases2 : List[Base] = List(T,T,G,G)
  
  //val bases3 : List[Base] = Some(lireSequence())gdthd
  
  
  println("[" + listeBasesToString(bases2) + "]")
}














