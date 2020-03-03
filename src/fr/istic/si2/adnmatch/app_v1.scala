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
  var On:Boolean = true
  while (On){
    println("╔══════════════════════════════════════════════════════════════════════════════╗")
    println("║ Taper L pour saisir et afficher une liste de base avec \"listeBasesToString()\"║")
    println("║ Taper A pour saisir et transformer une expression Rxp en type algébrique     ║")
    println("║ Taper E pour saisir et afficher une expression RExp avec \"rExpToString()\"    ║")
    println("║ Taper D pour saisir et dérouler une expression Rexp avec \"deroule()\"         ║")
    println("║ Taper Q pour quitter l'application                                           ║")
    println("╚══════════════════════════════════════════════════════════════════════════════╝")
    
    val a = s.nextLine()
    
    a match{
      case "L"|"l" =>
        println("Entrez une liste de base")
        lireSequence() match{
          case Some(lb) => println(listeBasesToString(lb))
          case _        => "Mauvaise séquence, recommencez"
        }
      case "A"|"a"=>
        println("Entrez une expression RExp")
        val b = s.nextLine()
        litRExp(b) match {
          case Some(lb) => println(lb)
          case _        => "Mauvaise séquence, recommencez"
        }
      case "E"|"e" =>
        println("Entrez une expression RExp")
        val b = s.nextLine()
        litRExp(b) match{
          case Some(lb) => println(rExpToString(lb))
          case _        => "Mauvaise expression, recommencez"
        }
      case "D"|"d" => 
        println("Entrez une expression RExp")
        val b = s.nextLine()
        litRExp(b) match{
          case Some(lb) => 
            deroule(lb) match {
              case None    => None
              case Some(a) => println(listeBasesToString(a))
            }
          case _        => "Mauvaise expression, recommencez"
        }
      case "Q"|"q" => On=false
      case _   => println("Mauvaise entrée, recommencez")
    }
  }
}














