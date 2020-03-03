package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._

/**
 * Type algébrique décrivant les différents marqueurs
 * indiquant les résultats de recherche.
 */
sealed trait Marqueur
// TODO V2

object RExpMatcher {

  /**
   * @param e une expression régulière
   * @param b une base azotée
   * @return la dérivée de Brzozowski de e par rapport à b
   */
  // TODO V2
  def derivee(e: RExp, b: Base): RExp = ???

  /**
   * @param e une expression régulière
   * @param lb une liste de bases azotées
   * @return vrai ssi la liste lb entière est décrite par e
   */
  // TODO V2
  def matchComplet(e: RExp, lb: List[Base]): Boolean = ???

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb est décrite
   */
  // TODO V2
  def sequenceDecrite(lb: List[Base]): List[(Marqueur, Base)] = ???

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb n'est pas décrite
   */
  // TODO V2
  def sequenceNonDecrite(lb: List[Base]): List[(Marqueur, Base)] = ???

  /**
   * @param e une expression régulière
   * @param lb une liste de bases azotées
   * @return s'il existe, le plus petit prefixe de lb qui est décrit par e
   */
  // TODO V2
  def prefixeMatch(e: RExp, lb: List[Base]): Option[List[Base]] = ???

  /**
   * @param pref une liste de bases azotées *préfixe* de lb
   * @param lb une liste de bases azotées
   * @return la sous-liste de lb située après le préfixe pref
   */
  // TODO V2
  def suppPrefixe(pref: List[Base], lb: List[Base]): List[Base] = ???

  /**
   * @param e une expression régulière
   * @param lb une liste de bases
   * @return une liste  (m1, base1)::...::(mN,baseN)::Nil, qui marque,
   *         base après base, les sous-listes de lb décrites par e.
   *         Les basei sont les bases de lb dans l'ordre.
   */
  // TODO V2
  def tousLesMatchs(e: RExp, lb: List[Base]): List[(Marqueur, Base)] = ???

  /**
   * @param lbm une liste de bases marquées selon un résultat de recherche
   * @return une description textuelle du résultat pour l'utilisateur
   */
  // TODO V2
  def messageResultat(lbm: List[(Marqueur, Base)]): String = ???

  /**
   * @param lb une liste de bases azotées marquées
   * @return liste des mêmes bases que lb, mais où tous les marqueurs indiquent
   *         une non-correspondance
   */
  // TODO V3
  def annulerResultat(lb: List[(Marqueur, Base)]): List[(Marqueur, Base)] = ???

  /**
   * @param lbm une liste de bases azotées marquées
   * @return la liste des bases de lbm dont on a oublié les marqueurs, en conservant l'ordre
   */
  // TODO V3
  def sansMarqueurs(lbm: List[(Marqueur, Base)]): List[Base] = ???

}