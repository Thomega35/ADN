package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._

/**
 * Type algébrique décrivant les différents marqueurs
 * indiquant les résultats de recherche.
 */
sealed trait Marqueur
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
   *         que la totalité de lb n'est pas décrite
   */
  def sequenceNonDecrite(lb: List[Base]): List[(Marqueur, Base)] =
    lb match {
      case Nil          => Nil
      case base :: list => (NonDecrite, base) :: sequenceNonDecrite(list)
    }

  /**
   * @param e une expression régulière
   * @return le préfixe de l'expression régulière e
   */
  def getFirstRExp(e: RExp): RExp = {
    e match {
      case Impossible | Vide | Nqb | UneBase(_) | Repete(_) => e
      case Choix(rexp1, rexp2)                              => simplifiedChoix(rexp1, rexp2)
      case NFois(rexp, _)                                   => getFirstRExp(rexp)
      case Concat(rexp1, rexp2)                             => getFirstRExp(rexp1)
    }
  }

  /**
   * @param e une expression régulière
   * @param lb une liste de bases azotées
   * @return s'il existe, le plus petit prefixe de lb qui est décrit par e
   */
  def prefixeMatch(e: RExp, lb: List[Base]): Option[List[Base]] = {
    lb match {
      case Nil =>
        matchComplet(e, Nil) match {
          case true  => Some(Nil)
          case false => None
        }
      case _ :: _ => {
        matchComplet(e, lb) match {
          case true  => Some(lb)
          case false => prefixeMatch(e, supprDernierElement(lb))
        }
      }
    }
  }

  def supprDernierElement(lb: List[Base]): List[Base] = {
    lb match {
      case Nil | _ :: Nil => Nil
      case base :: list   => base :: supprDernierElement(list)
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
   * @param lbm une liste de bases marquées selon un résultat de recherche
   * @return une description textuelle du résultat pour l'utilisateur
   */
  def messageResultat(lbm: List[(Marqueur, Base)]): String = {
    lbm match {
      case Nil                     => "Il n'y a pas de séquence qui décrie l'expression régulière"
      case (Decrite, _) :: _       => "Il y a une séquence qui décrie l'expression régulière"
      case (NonDecrite, _) :: list => messageResultat(list)
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
      case Nil                     => Nil
      case (marqueur, base) :: list => base :: sansMarqueurs(list)
    }
  }

}