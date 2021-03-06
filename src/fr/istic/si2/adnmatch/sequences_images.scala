package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._

object SequencesImages {

  /**
   * @param lmb une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return une liste contenant des sous-listes de lmb, toutes de taille tligne, sauf la dernière qui
   *         peut être de taille inférieure.
   */
  def lignes(lmb: List[(Marqueur, Base)], tligne: Int): List[List[(Marqueur, Base)]] = {
    lmb match {
      case Nil          => Nil
      case base :: list => calcLigne(lmb, tligne) :: lignes(supprLigne(calcLigne(lmb, tligne), lmb), tligne)
    }
  }

  /**
   * @param lmb une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return une liste contenant les éléments de la première ligne
   */
  def calcLigne(lmb: List[(Marqueur, Base)], tligne: Int): List[(Marqueur, Base)] = {
    tligne match {
      case 0 => Nil
      case _ =>
        lmb match {
          case Nil                      => Nil
          case (marqueur, base) :: list => (marqueur, base) :: calcLigne(list, tligne - 1)
        }
    }
  }

  /**
   * @param pref une liste de de bases marquées *préfixe* de lmb
   * @param lmb une liste de bases marquées
   * @return la sous-liste de lmb située après le préfixe pref
   */
  def supprLigne(pref: List[(Marqueur, Base)], lmb: List[(Marqueur, Base)]): List[(Marqueur, Base)] = {
    pref match {
      case Nil => lmb
      case base1 :: list1 => {
        lmb match {
          case Nil            => Nil
          case base2 :: list2 => supprLigne(list1, list2)
        }
      }
    }
  }

  /**
   * Taille du texte utiliser pour représenter
   * graphiquement les bases azotées.
   */
  val fontSizeBase: Int = 15
  /**
   * Couleur des bases utiliser
   */
  val textColorDecrite: Color = Color(66, 79, 170, 255)
  val boxColorDecrite: Color = Color(255, 255, 100, 100)
  val textColorDecriteBis: Color = Color(200, 79, 170, 255)
  val boxColorDecriteBis: Color = Color(255, 100, 100, 50)
  val textColorNonDecrite: Color = Color(66, 79, 170, 150)

  /**
   * @param mb une base azotée marquée
   * @return une image représentant la base avec son marqueur visuel
   */
  def marqueurBaseToImage(mb: (Marqueur, Base)): Image = {
    mb._1 match {
      case Decrite => on(
        lineColor(fillColor(Rectangle(10, fontSizeBase), boxColorDecrite), Color(0, 0, 0, 0)),
        lineColor(fillColor(Text(mb._2.toString(), fontSizeBase), textColorDecrite), textColorDecrite))
      case DecriteBis => on(
        lineColor(fillColor(Rectangle(10, fontSizeBase), boxColorDecriteBis), Color(0, 0, 0, 0)),
        lineColor(fillColor(Text(mb._2.toString(), fontSizeBase), textColorDecriteBis), textColorDecriteBis))
      case _ => lineColor(fillColor(Text(mb._2.toString(), fontSizeBase), textColorNonDecrite), textColorNonDecrite)
    }
  }

  /**
   * @param ligne une liste de bases azotées marquées
   * @return une image représentant les bases marquées de ligne, dans l'ordre, toutes sur la même ligne
   */
  def imageUneLigne(ligne: List[(Marqueur, Base)]): Image = {
    ligne match {
      case Nil           => Empty
      case basem :: list => beside(marqueurBaseToImage(basem), imageUneLigne(list))
    }
  }

  /**
   * @param llignes une liste de listes de bases azotées marquées
   * @return une image représentant les bases marquées de llignes, dans l'ordre.
   *         Chaque élément de llignes est sur une ligne distincte.
   *         Les lignes sont visualisées les unes en dessous des autres.
   */
  def imagePlusieursLignes(llignes: List[List[(Marqueur, Base)]]): Image = {
    llignes match {
      case Nil           => Empty
      case ligne :: list => below(imageUneLigne(ligne), imagePlusieursLignes(list))
    }
  }

}