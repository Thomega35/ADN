package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._
import java.util.Scanner

/**
 * Application ADNMatch version 1.
 */
object ADNMatchV1 extends App {

  println("ADNMatch Version 1")
  val bases1 : List[Base] = List(A,T,C,G)
  val bases2 : List[Base] = List(T,T,G,G,C,C,A,A)
  val bases3 : List[Base] = List(G,C,A,T)
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
  val s = new Scanner(System.in)
  
  /**
   * Application qui permet de saisir, afficher puis dérouler 
   * une expression RExp autant de fois qu'on le souhaite
   */
  def boucle ():Unit ={
    println("╔═════════════════════════════════════════════════════════════════╗")
    println("║ Taper D pour saisir, afficher puis dérouler une expression RExp ║")
    println("║ Taper Q pour quitter l'application                              ║")
    println("╚═════════════════════════════════════════════════════════════════╝")
    val a = s.nextLine()
    a match{
      case "D"|"d" => 
        println("Entrez une expression RExp")
        val b = s.nextLine()
        litRExp(b) match{
          case Some(lb) => println("Voici un déroulement possible de " + rExpToString(lb) + ":")
            deroule(lb) match {
              case Some(a) => println(listeBasesToString(a))
              case None    => println("Expression vide")
            }
          case None     => println("Mauvaise expression, recommencez")
        }
        boucle()
      case "Q"|"q" => println("Bye Bye")
      case _       => 
        println("Mauvaise entrée, recommencez")
        boucle()
    }
  }
  boucle()
}














