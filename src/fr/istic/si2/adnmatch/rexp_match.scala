package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._

/**
 * Type algébrique décrivant les différents marqueurs
 * indiquant les résultats de recherche.
 */
sealed trait Marqueur
case object DecriteBis extends Marqueur
case object Decrite extends Marqueur
case object NonDecrite extends Marqueur

object RExpMatcher {

  /**
   * @param e une expression régulière
   * @param b une base azotée
   * @return la dérivée de Brzozowski de e par rapport à b
   */
  def derivee(e: RExp, b: Base): RExp = {
    e match {
      case Impossible => Impossible
      case Vide       => Impossible
      case Nqb        => Vide
      case UneBase(base) => {
        base match {
          case `b` => Vide
          case _   => Impossible
        }
      }
      case Choix(rexp1, rexp2) => simplifiedChoix(derivee(rexp1, b), derivee(rexp2, b))
      case Concat(rexp1, rexp2) => {
        isRExpEmpty(rexp1) match {
          case false => simplifiedConcat(derivee(rexp1, b), rexp2)
          case true  => simplifiedChoix(simplifiedConcat(derivee(rexp1, b), rexp2), derivee(rexp2, b))
        }
      }
      case Repete(rexp) => simplifiedConcat(derivee(rexp, b), Repete(rexp))
      case NFois(rexp, n) => {
        n match {
          case 0 => derivee(Vide, b)
          case 1 => derivee(rexp, b)
          case _ => simplifiedConcat(derivee(rexp, b), NFois(rexp, n - 1))
        }
      }
    }
  }

  /**
   * @param rexp1 une expression régulière
   * @param rexp2 une expression régulière
   * @return RExp le choix simplifié des deux expressions si possible
   */
  def simplifiedChoix(rexp1: RExp, rexp2: RExp): RExp = {
    (rexp1, rexp2) match {
      case (Impossible, rexp) => rexp
      case (rexp, Impossible) => rexp
      case _                  => Choix(rexp1, rexp2)
    }
  }

  /**
   * @param rexp1 une expression régulière
   * @param rexp2 une expression régulière
   * @return RExp la concaténation simplifiée des deux expression si possible
   */
  def simplifiedConcat(rexp1: RExp, rexp2: RExp): RExp = {
    (rexp1, rexp2) match {
      case (Impossible, _) | (_, Impossible) => Impossible
      case (Vide, rexp)                      => rexp
      case (rexp, Vide)                      => rexp
      case _                                 => Concat(rexp1, rexp2)
    }
  }

  /**
   * @param e une expression régulière
   * @return Boolean e décrit la séquence vide
   */
  def isRExpEmpty(e: RExp): Boolean = {
    e match {
      case Nqb | UneBase(_) | Impossible => false
      case Vide                          => true
      case Choix(rexp1, rexp2) =>
        (isRExpEmpty(rexp1), isRExpEmpty(rexp2)) match {
          case (false, false) => false
          case _              => true
        }
      case Concat(rexp1, rexp2) =>
        (isRExpEmpty(rexp1), isRExpEmpty(rexp2)) match {
          case (true, true) => true
          case _            => false
        }
      case Repete(rexp)   => true
      case NFois(rexp, n) => isRExpEmpty(rexp)
    }
  }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases azotées non vide
   * @return vrai ssi la liste lb entière est décrite par e
   */
  def matchComplet(e: RExp, lb: List[Base]): Boolean = {
    lb match {
      case Nil          => isRExpEmpty(e)
      case base :: list => matchComplet(derivee(e, base), list)
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb est décrite
   */
  def sequenceDecrite(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil          => Nil
      case base :: list => (Decrite, base) :: sequenceDecrite(list)
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb est décrite (avec le marqueur DecriteBis)
   */
  def sequenceDecriteBis(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil          => Nil
      case base :: list => (DecriteBis, base) :: sequenceDecriteBis(list)
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb n'est pas décrite
   */
  def sequenceNonDecrite(lb: List[Base]): List[(Marqueur, Base)] =
    lb match {
      case Nil          => Nil
      case base :: list => (NonDecrite, base) :: sequenceNonDecrite(list)
    }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases azotées
   * @return s'il existe, le plus petit prefixe de lb qui est décrit par e
   */
  def prefixeMatch(e: RExp, lb: List[Base]): Option[List[Base]] = {
    lb match {
      case Nil => None
      case base :: list => {
        isRExpEmpty(derivee(e, base)) match {
          case true => Some(base :: Nil)
          case false => {
            prefixeMatch(derivee(e, base), list) match {
              case None       => None
              case Some(list) => Some(base :: list)
            }
          }
        }
      }
    }
  }

  /**
   * @param pref une liste de bases azotées *préfixe* de lb
   * @param lb une liste de bases azotées
   * @return la sous-liste de lb située après le préfixe pref
   */
  def suppPrefixe(pref: List[Base], lb: List[Base]): List[Base] = {
    pref match {
      case Nil => lb
      case base1 :: list1 => {
        lb match {
          case Nil            => Nil
          case base2 :: list2 => suppPrefixe(list1, list2)
        }
      }
    }
  }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases
   * @return une liste  (m1, base1)::...::(mN,baseN)::Nil, qui marque,
   *         base après base, les sous-listes de lb décrites par e.
   *         Les basei sont les bases de lb dans l'ordre.
   */
  def tousLesMatchs(e: RExp, lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil => Nil
      case base :: list => {
        prefixeMatch(e, lb) match {
          case None          => (NonDecrite, base) :: tousLesMatchs(e, list)
          case Some(subList) => sequenceDecrite(subList) ++ tousLesMatchs(e, suppPrefixe(subList, lb))
        }
      }
    }
  }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases
   * @param isBis boolean qui alterne entre les sous séquences
   * @return une liste  (m1, base1)::...::(mN,baseN)::Nil, qui marque, de façon alternée
   *         base après base, les sous-listes de lb décrites par e.
   *         Les basei sont les bases de lb dans l'ordre.
   */
  def tousLesMatchs(e: RExp, lb: List[Base], isBis: Boolean): List[(Marqueur, Base)] = {
    lb match {
      case Nil => Nil
      case base :: list => {
        prefixeMatch(e, lb) match {
          case None => (NonDecrite, base) :: tousLesMatchs(e, list, isBis)
          case Some(subList) => {
            isBis match {
              case true  => sequenceDecriteBis(subList) ++ tousLesMatchs(e, suppPrefixe(subList, lb), false)
              case false => sequenceDecrite(subList) ++ tousLesMatchs(e, suppPrefixe(subList, lb), true)
            }
          }
        }
      }
    }
  }

  /**
   * @param lbm une liste de bases marquées selon un résultat de recherche
   * @return une description textuelle du résultat pour l'utilisateur
   */
  def messageResultat(lbm: List[(Marqueur, Base)]): String = {
    lbm match {
      case (NonDecrite, _) :: list                  => messageResultat(list)
      case (Decrite, _) :: _ | (DecriteBis, _) :: _ => "✔ Il y a au moins une séquence qui décrie l'expression régulière"
      case _                                        => "✖ Il n'y a pas de séquence qui décrie l'expression régulière"
    }
  }
  /**
   * @param lbm une liste de bases marquées selon un résultat de recherche
   * @return une description textuelle du résultat pour l'utilisateur avec le nombre de match
   */
  def messageResultatDenombrable(lbm: List[(Marqueur, Base)]): String = {
    getNbSequence(lbm) match {
      case 0       => "✖ Il n'y a pas de séquence qui décrie l'expression régulière"
      case nb: Int => s"✔ Il y a $nb sous séquence(s) qui décrie(nt) l'expression régulière"
    }
  }

  /**
   * @param lb liste de doublet (marqueur , base)
   * @return est-ce que lb est dénombrable ?
   */
  def isDenombrable(lb: List[(Marqueur, Base)]): Boolean = {
    lb match {
      case Nil                  => false
      case (DecriteBis, _) :: _ => true
      case (_, _) :: list       => isDenombrable(list)
    }
  }

  /**
   * @param lb une liste de bases azotées marquées
   * @return liste des mêmes bases que lb, mais où tous les marqueurs indiquent
   *         une non-correspondance
   */
  def annulerResultat(lb: List[(Marqueur, Base)]): List[(Marqueur, Base)] = {
    lb match {
      case Nil               => Nil
      case (_, base) :: list => (NonDecrite, base) :: annulerResultat(list)
    }
  }

  /**
   * @param lbm une liste de bases azotées marquées
   * @return la liste des bases de lbm dont on a oublié les marqueurs, en conservant l'ordre
   */
  def sansMarqueurs(lbm: List[(Marqueur, Base)]): List[Base] = {
    lbm match {
      case Nil                      => Nil
      case (marqueur, base) :: list => base :: sansMarqueurs(list)
    }
  }

  /**
   * @param lb une liste de bases azotées marquées
   * @param nb le bombre de séquence trouvé
   * @return le nombre de séquence correspondantes à l'expression RExp
   */
  def getNbSequence(lb: List[(Marqueur, Base)], nb: Int): Int = {
    lb match {
      case Nil | _ :: Nil => nb
      case (Decrite, _) :: (DecriteBis, base) :: list => getNbSequence((DecriteBis, base) :: list, nb + 1)
      case (Decrite, _) :: (NonDecrite, base) :: list => getNbSequence((NonDecrite, base) :: list, nb + 1)
      case (NonDecrite, _) :: (DecriteBis, base) :: list => getNbSequence((DecriteBis, base) :: list, nb + 1)
      case (NonDecrite, _) :: (Decrite, base) :: list => getNbSequence((Decrite, base) :: list, nb + 1)
      case (DecriteBis, _) :: (NonDecrite, base) :: list => getNbSequence((NonDecrite, base) :: list, nb + 1)
      case (DecriteBis, _) :: (Decrite, base) :: list => getNbSequence((Decrite, base) :: list, nb + 1)
      case _ :: (marqueur, base) :: list => getNbSequence((marqueur, base) :: list, nb)
      case _ => -999
    }
  }

  /**
   * @param lb une liste de bases azotées marquées
   * @return le nombre de séquence correspondantes à l'expression RExp
   */
  def getNbSequence(lb: List[(Marqueur, Base)]): Int = {
    getNbSequence(lb, 0)
  }

}