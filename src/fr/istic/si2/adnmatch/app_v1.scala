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
  
  //val bases3 : List[Base] = Some(lireSequence())

  //dwadaw
  
  
  println("[" + listeBasesToString(bases2) + "]")
}














