package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatchlib._
import java.util.Scanner

/**
 * Application ADNMatch version 2
 */
object ADNMatchV2 extends App {

  val exampleList1: List[Base] = List(A, T, C, G)
  val exampleList2: List[Base] = List(T, T, G, G, C, C, A, A)
  val exampleList3: List[Base] = List(G, C, A, T)
  val exampleRExp1: RExp = Repete(UneBase(T))
  val exampleRExp2: RExp = Choix(UneBase(T), UneBase(G))
  val exampleRExp3: RExp = Concat(UneBase(A), Concat(UneBase(T), Concat(UneBase(C), UneBase(G))))

  print("\nADNMatch Version 2\n")
  val scanner = new Scanner(System.in)

  /**
   * Application qui permet de saisir, afficher, dérouler, comparer
   * une expression RExp à une séquence
   */
  def isRExpEqualSeq(): Unit = {
    println()
    println("╔════════════════════════════════════════════╗")
    println("║ Veiller saisir une expression régulière.   ║")
    println("║ Taper \"Quit\" pour quitter l'application.   ║")
    println("╚════════════════════════════════════════════╝")
    val input = scanner.nextLine()
    input match {
      case _ => {
        litRExp(input) match {
          case Some(rexp) => {
            print(s"Voici un déroulement possible de ${rExpToString(rexp)} : ")
            deroule(rexp) match {
              case None       => println("-Expression impossibe-\n")
              case Some(list) => println(s"${listeBasesToString(list)}\n")
            }
            println("╔══════════════════════════════════════════════════════╗")
            println("║ Veiller saisir une séquence de bases (A, T, C ou G)  ║")
            println("║        à comparer avec l'expression régulière        ║")
            println("╚══════════════════════════════════════════════════════╝")
            lireSequence() match {
              case None => { println("Séquence de bases non valide, opération annulé"); mainLoop() }
              case Some(list) => {
                println(s"Analyse de l'expression régulière ''${rExpToString(rexp)}'' sur la séquence ''${listeBasesToString(list)}''")

                matchComplet(rexp, list) match {
                  case true  => println("✔ L'expression régulière correspond bien à la séquence saisie")
                  case false => println("✖ L'expression régulière ne correspond pas à la séquence saisie")
                }
              }
            }

          }
          case None => println("Mauvaise expression, recommencez")
        }
        mainLoop()
      }
    }
  }

  def seqAnalysis(): Unit = {
    println()
    println("╔════════════════════════════════════════════╗")
    println("║ Veiller saisir une expression régulière.   ║")
    println("║ Taper \"Quit\" pour quitter l'application.   ║")
    println("╚════════════════════════════════════════════╝")
    val input = scanner.nextLine()
    input match {
      case _ => {
        litRExp(input) match {
          case Some(rexp) => {
            print(s"Voici un déroulement possible de ${rExpToString(rexp)} : ")
            deroule(rexp) match {
              case None       => println("-Expression impossibe-\n")
              case Some(list) => println(s"${listeBasesToString(list)}\n")
            }
            println("╔══════════════════════════════════════════════════════╗")
            println("║ Veiller saisir une séquence de bases (A, T, C ou G)  ║")
            println("║        à analyser avec l'expression régulière        ║")
            println("╚══════════════════════════════════════════════════════╝")
            lireSequence() match {
              case None       => { println("Séquence de bases non valide, opération annulé"); mainLoop() }
              case Some(list) => println(s"\n${messageResultat(tousLesMatchs(rexp, list))}")
            }
          }
          case None => println("Mauvaise expression, recommencez")
        }
        mainLoop()
      }
    }
  }

  def mainLoop(): Unit = {
    println()
    println("╔═══════════════════════════════════════════════════════════════════════╗")
    println("║ Entrer \"C\" pour comparer une expression régulière avec une séquence   ║")
    println("║ Entrer \"R\" pour rechercher une expression régulière dans une séquence ║")
    println("║ Entrer \"Quit\" pour quitter l'application                              ║")
    println("╚═══════════════════════════════════════════════════════════════════════╝")
    val input = scanner.nextLine()
    input match {
      case "quit" | "Quit" | "q" => println("\nFin de programme\nADNMatch Version 2\n")
      case "c" | "C"             => isRExpEqualSeq()
      case "r" | "R"             => seqAnalysis()
      case _                     => { println("Entrée non reconnue, veuiller recommencer"); mainLoop() }
    }
  }
  mainLoop()
}