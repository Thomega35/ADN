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
      case Nil          => ""
      case base :: list => base + listeBasesToString(list)
    }
  }

  /**
   * @param e une expression régulière
   * @return la représentation textuelle de e, avec toutes les parenthèses nécessaires
   */
  def rExpToString(e: RExp): String = {
    e match {
      case Impossible           => "@"
      case Vide                 => "%"
      case Nqb                  => "."
      case UneBase(base)        => base.toString()
      case Choix(rexp1, rexp2)  => s"(${rExpToString(rexp1)}|${rExpToString(rexp2)})"
      case Concat(rexp1, rexp2) => rExpToString(rexp1) + rExpToString(rexp2)
      case Repete(rexp)         => s"${rExpToString(rexp)}*"
      case NFois(rexp, n)       => s"(${rExpToString(rexp)}){$n}"
    }
  }

  /**
   * @param e une expression régulière
   * @return une liste de bases obtenue en déroulant e tout le temps de la même manière.
   * @note
   * si il y a un choix, on rend le premier élément si il n'est pas Impossible sinon le second
   * si il y a répétition, on rend 2 fois l'élément
   * si il y a Nqb, on rend la base A
   */
  def deroule(e: RExp): Option[List[Base]] = {
    e match {
      case Impossible    => None
      case Vide          => Some(Nil)
      case Nqb           => Some(A :: Nil)
      case UneBase(base) => Some(base :: Nil)
      case Concat(rexp1, rexp2) =>
        deroule(rexp1) match {
          case None => None
          case Some(list1) =>
            deroule(rexp2) match {
              case None        => None
              case Some(list2) => Some(list1 ++ list2)
            }
        }
      case Repete(rexp) =>
        deroule(rexp) match {
          case None       => None
          case Some(list) => Some(list ++ list)
        }
      case NFois(rexp, n) =>
        n match {
          case 0 => Some(Nil)
          case _ =>
            deroule(rexp) match {
              case None => None
              case Some(list1) => {
                deroule(NFois(rexp, n - 1)) match {
                  case None        => None
                  case Some(list2) => Some(list1 ++ list2)
                }
              }
            }
        }
      case Choix(rexp1, rexp2) =>
        deroule(rexp1) match { //permet de prendre le second (rexp2) si le premier (rexp1) est impossible
          case None    => deroule(rexp2)
          case Some(_) => deroule(rexp1)
        }
    }
  }
}