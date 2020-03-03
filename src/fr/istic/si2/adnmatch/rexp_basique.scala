package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._

/**
 * Type algébrique modélisant
 * les bases azotées composant les séquences ADN.
 */
sealed trait Base
case object A extends Base
case object T extends Base
case object G extends Base
case object C extends Base

/**
 * Type algébrique récursif modélisant les
 * expressions régulières sur les bases azotées.
 */
sealed trait RExp
case object Impossible extends RExp
case object Vide extends RExp
case object Nqb extends RExp
case class UneBase(b: Base) extends RExp
case class Choix(e1: RExp, e2: RExp) extends RExp
case class Concat(e1: RExp, e2: RExp) extends RExp
case class Repete(e: RExp) extends RExp
case class NFois(e: RExp, n: Int) extends RExp

object FonctionsRExp {

  /**
   * @param lb une liste de bases azotées
   * @return une chaîne de caractères représentant les bases de lb, dans l'ordre
   */
  def listeBasesToString(lb: List[Base]): String = {
    lb match {
      case Nil    => ""
      case a :: b => a + listeBasesToString(b)
    }
  }
      
  /**
   * @param e une expression régulière
   * @return la représentation textuelle de e, avec toutes les parenthèses nécessaires
   */
  def rExpToString(e: RExp): String = {
    e match {
      //case Impossible  => "@"
      //case Vide        => "%"
      case Nqb         => "."
      case UneBase(a)  => a + ""
      case Choix(a,b)  => "(" + rExpToString(a) + "|" + rExpToString(b) + ")"
      case Concat(a,b) => rExpToString(a) + rExpToString(b)
      case Repete(a)   => rExpToString(a) + "*"
      case NFois(a,b)  => "(" + rExpToString(a) + ")" + "{" + b + "}"
    }
  }

  /**
   * @param e une expression régulière
   * @return une liste de bases obtenue en déroulant e tout le temps de la même manière.
   * @note si choix on affiche la première valeure
   * 			 si répétition on affiche 2 fois la base
   * 			 si Nqb on affiche A
   */
  def deroule(e: RExp): Option[List[Base]] = {
    Some(derouleSous(e))
  }
  
  def derouleSous(e: RExp): List[Base] ={
    e match {
      case Nqb        => A :: Nil
      case UneBase(a) => a :: Nil
      case Choix(a,b) => derouleSous(a)
      case Concat(a,b)=> derouleSous(a) ++ derouleSous(b)
      
    }
  }
}