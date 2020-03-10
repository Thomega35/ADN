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

  val exampleList1: List[Base] = List(A, T, C, G)
  val exampleList2: List[Base] = List(T, T, G, G, C, C, A, A)
  val exampleList3: List[Base] = List(G, C, A, T)
  val exampleRExp1: RExp = Repete(UneBase(T))
  val exampleRExp2: RExp = Choix(UneBase(T), UneBase(G))
  val exampleRExp3: RExp = Concat(UneBase(A), Concat(UneBase(T), Concat(UneBase(C), UneBase(G))))

  println("\nADNMatch Version 1\n")
  val scanner = new Scanner(System.in)

  /**
   * Application qui permet de saisir, afficher puis dérouler
   * une expression RExp autant de fois qu'on le souhaite
   */
  def mainLoop(): Unit = {
    println("╔═══════════════════════════════════════════════════════╗")
    println("║ Veuiller saisir une expression régulière              ║")
    println("║ Taper \"quit\" pour quitter l'application               ║")
    println("╚═══════════════════════════════════════════════════════╝")
    val input = scanner.nextLine()
    input match {
      case "quit" | "Quit" | "q" => println("\nFin de programme\nADNMatch Version 1\n")
      case _ => {
        litRExp(input) match {
          case Some(rexp) => {
            print(s"Voici un déroulement possible de ${rExpToString(rexp)} : ")
            deroule(rexp) match {
              case None       => println("-Expression impossibe-\n")
              case Some(list) => println(s"${listeBasesToString(list)}\n")
            }
          }
          case None => println("Mauvaise expression, recommencez\n")
        }
        mainLoop()
      }
    }
  }
  mainLoop()

}














