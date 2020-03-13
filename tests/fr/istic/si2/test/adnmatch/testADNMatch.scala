package fr.istic.si2.test.adnmatch

import org.junit.Test

import org.junit.Assert._
import util.Random
import fr.istic.si2.testerApp._
import fr.istic.si2.moreAssertions._
import fr.istic.si2.scribble._
import fr.istic.si2.math._

import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatchlib._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._

class ADNMatchTest {

  // Normaux
  val rexp1: RExp = Concat(Nqb, NFois(Choix(Impossible, UneBase(G)), 2))
  val rexpStr1: String = ".((@|G)){2}"
  val deroule1: Option[List[Base]] = Some(A :: G :: G :: Nil)
  val list1: List[Base] = A :: G :: G :: Nil
  val listStr1: String = "AGG"
  val deriveeRexp1A: RExp = NFois(Choix(Impossible, UneBase(G)), 2)
  val listdecrite1: List[(Marqueur,Base)] = (Decrite,A) :: (Decrite,G) :: (Decrite,G) :: Nil

  // Avec Impossible
  val rexp2: RExp = Concat(Concat(UneBase(G), Repete(Nqb)), NFois(Impossible, 3))
  val rexpStr2: String = "G.*(@){3}"
  val deroule2: Option[List[Base]] = None
  val list2: List[Base] = Nil
  val listStr2: String = ""
  val deriveeRexp2G: RExp = Concat(Repete(Nqb), NFois(Impossible, 3))
    
  // Simple
  val rexp3: RExp = Concat(Concat(Concat(UneBase(T), UneBase(C)), UneBase(A)), UneBase(G))
  val rexpStr3: String = "TCAG"
  val deroule3: Option[List[Base]] = Some(T :: C :: A :: G :: Nil)
  val list3: List[Base] = T :: C :: A :: G :: Nil
  val listStr3: String = "TCAG"
  val deriveeRexp3A: RExp = Impossible
  val deriveeRexp3T: RExp = Concat(Concat(UneBase(C), UneBase(A)), UneBase(G))

  ////     ////     ////     ////     ////     ////     ////     ////     ////     ////
  ///     ////     ////  Testes du fichier rexp_basique.scala   ////     ////     ////
  //     ////     ////     ////     ////     ////     ////     ////     ////     ////

  /**
   * Test de la fonction      listeBasesToString()
   * avec comme paramètre(s)  List(A)
   * resultat attendu(s)      "A"
   */
  @Test
  def testLBTS1() {
    assertEquals("A", listeBasesToString(List(A)))
  }

  /**
   * Test de la fonction      listeBasesToString()
   * avec comme paramètre(s)  list1
   * resultat attendu(s)      listStr1
   */
  @Test
  def testLBTS2() {
    assertEquals(listStr1, listeBasesToString(list1))
  }

  /**
   * Test de la fonction      listeBasesToString()
   * avec comme paramètre(s)  list2
   * resultat attendu(s)      listStr2
   */
  @Test
  def testLBTS3() {
    assertEquals(listStr2, listeBasesToString(list2))
  }

  /**
   * Test de la fonction      rExpToString()
   * avec comme paramètre(s)  Vide    Impossible  Nqb     UneBase(A)
   * resultat attendu(s)      "%"     "@"         "."     "A"
   */
  @Test
  def testRExpToString1() {
    assertEquals("%", rExpToString(Vide))
    assertEquals("@", rExpToString(Impossible))
    assertEquals(".", rExpToString(Nqb))
    assertEquals("A", rExpToString(UneBase(A)))
  }

  /**
   * Test de la fonction      rExpToString()
   * avec comme paramètre(s)  rexp1
   * resultat attendu(s)      rexpStr1
   */
  @Test
  def testRExpToString2() {
    assertEquals(rexpStr1, rExpToString(rexp1))
  }

  /**
   * Test de la fonction      rExpToString()
   * avec comme paramètre(s)  rexp2
   * resultat attendu(s)      rexpStr2
   */
  @Test
  def testRExpToString3() {
    assertEquals(rexpStr2, rExpToString(rexp2))
  }

  /**
   * Test de la fonction      deroule()
   * avec comme paramètre(s)  rexp1
   * resultat attendu(s)      deroule1
   */
  @Test
  def testDeroule1() {
    assertEquals(deroule1, deroule(rexp1))
  }

  /**
   * Test de la fonction      deroule()
   * avec comme paramètre(s)  rexp2
   * resultat attendu(s)      deroule2
   */
  @Test
  def testDeroule2() {
    assertEquals(deroule2, deroule(rexp2))
  }

  ////     ////     ////     ////     ////     ////     ////     ////     ////     ////
  ///     ////     ////   Testes du fichier rexp_match.scala    ////     ////     ////
  //     ////     ////     ////     ////     ////     ////     ////     ////     ////

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  Concat(UneBase(A), UneBase(T)), T
   * resultat attendu(s)      Impossible
   */
  @Test
  def testDerivee0() {
    assertEquals(Impossible, derivee(Concat(UneBase(A), UneBase(T)), T))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (Impossible , A)   (Vide , A)
   * resultat attendu(s)      Impossible
   */
  @Test
  def testDerivee1() {
    assertEquals(Impossible, derivee(Impossible, A))
    assertEquals(Impossible, derivee(Vide, A))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (rexp1, A)
   * resultat attendu(s)      deriveeRexp1A
   */
  @Test
  def testDerivee2() {
    assertEquals(deriveeRexp1A, derivee(rexp1, A))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (rexp2, G)
   * resultat attendu(s)      deriveeRexp2G
   */
  @Test
  def testDerivee3() {
    assertEquals(deriveeRexp2G, derivee(rexp2, G))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (rexp3, A)
   * resultat attendu(s)      deriveeRexp3A
   */
  @Test
  def testDerivee4() {
    assertEquals(deriveeRexp3A, derivee(rexp3, A))
  }
  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (Repete(rexp1), A)
   * resultat attendu(s)      Concat(derivee(rexp1, A), Repete(rexp1))
   */
  @Test
  def testDerivee5() {
    assertEquals(Concat(derivee(rexp1, A), Repete(rexp1)), derivee(Repete(rexp1), A))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (UneBase(T), A)
   * resultat attendu(s)      Impossible
   */
  @Test
  def testDerivee6() {
    assertEquals(Impossible, derivee(UneBase(T), A))
  }
  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (UneBase(A), A)
   * resultat attendu(s)      Vide
   */
  @Test
  def testDerivee7() {
    assertEquals(Vide, derivee(UneBase(A), A))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (Concat(rexp1, rexp2), A)
   * resultat attendu(s)      Concat(derivee(rexp1, A), rexp2)
   */
  @Test
  def testDerivee8() {
    assertEquals(Concat(derivee(rexp1, A), rexp2), derivee(Concat(rexp1, rexp2), A))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (Concat(Concat(UneBase(G), UneBase(T)), UneBase(A)), A)
   * resultat attendu(s)      Impossible
   */
  @Test
  def testDerivee9() {
    assertEquals(Impossible, derivee(Concat(Concat(UneBase(G), UneBase(T)), UneBase(A)), A))
  }

  /**
   * Test de la fonction      derivee()
   * avec comme paramètre(s)  (Concat(Concat(Repete(UneBase(A)), UneBase(C)), UneBase(T)), C))
   * resultat attendu(s)      UneBase(T)
   */
  @Test
  def testDerivee10() {
    assertEquals(UneBase(T), derivee(Concat(Concat(Repete(UneBase(A)), UneBase(C)), UneBase(T)), C))
  }

  /**
   * Test de la fonction      matchComplet()
   * avec comme paramètre(s)  rexp1, list1
   * resultat attendu(s)      true
   */
  @Test
  def testMatchComplet1() {
    assertEquals(true, matchComplet(rexp1, list1))
  }

  /**
   * Test de la fonction      matchComplet()
   * avec comme paramètre(s)  rexp1, list2
   * resultat attendu(s)      false
   */
  @Test
  def testMatchComplet2() {
    assertEquals(false, matchComplet(rexp1, list2))
  }

  /**
   * Test de la fonction      matchComplet()
   * avec comme paramètre(s)  rexp3, list3
   * resultat attendu(s)      true
   */
  @Test
  def testMatchComplet3() {
    assertEquals(true, matchComplet(rexp3, list3))
  }

  /**
   * Test de la fonction      matchComplet()
   * avec comme paramètre(s)  rexp3, list1
   * resultat attendu(s)      false
   */
  @Test
  def testMatchComplet4() {
    assertEquals(false, matchComplet(rexp3, list1))
  }

  /**
   * Test de la fonction      matchComplet()
   * avec comme paramètre(s)  Concat(Repete(UneBase(A)),UneBase(C)), A :: A :: C :: Nil
   * resultat attendu(s)      true
   */
  @Test
  def testMatchComplet5() {
    assertEquals(true, matchComplet(Concat(Repete(UneBase(A)), UneBase(C)), A :: A :: C :: Nil))
  }

  /**
   * Test de la fonction      matchComplet()
   * avec comme paramètre(s)  Repete(UneBase(A)), Nil
   * resultat attendu(s)      true
   */
  @Test
  def testMatchComplet6() {
    assertEquals(true, matchComplet(Repete(UneBase(A)), Nil))
  }
  
   /**
   * Test de la fonction      matchComplet()
   * avec comme paramètre(s)  Concat(UneBase(T),Repete(UneBase(A))), List(T))
   * resultat attendu(s)      true
   */
  @Test
  def testMatchComplet7() {
    assertEquals(true, matchComplet(Concat(UneBase(T),Repete(UneBase(A))), List(T)))
  }
  
  /**
   * Test de la fonction      prefixeMatch()
   * avec comme paramètre(s)  Concat(UneBase(A), UneBase(T)), List(A, G, G))
   * resultat attendu(s)      None
   */
  @Test
  def testPrefixeMatch1() {
    assertEquals(None, prefixeMatch(Concat(UneBase(A), UneBase(T)), List(A, G, G)))
  }

  /**
   * Test de la fonction      prefixeMatch()
   * avec comme paramètre(s)  Concat(Nqb, UneBase(G)), List(A, G, G))
   * resultat attendu(s)      Some(List(A, G))
   */
  @Test
  def testPrefixeMatch2() {
    assertEquals(Some(List(A,G)), prefixeMatch(Concat(Nqb, UneBase(G)), List(A, G, G)))
  }

  /**
   * Test de la fonction      prefixeMatch()
   * avec comme paramètre(s)  Concat(UneBase(A),Repete(UneBase(T))), List(A, G, G))
   * resultat attendu(s)      Some(List(A))
   */
  @Test
  def testPrefixeMatch3() {
    assertEquals(Some(List(A)), prefixeMatch(Concat(UneBase(A),Repete(UneBase(T))), List(A, G, G)))
  }

  /**
   * Test de la fonction      prefixeMatch()
   * avec comme paramètre(s)  Repete(UneBase(A)), List(G, G, G))
   * resultat attendu(s)      None
   */
  @Test
  def testPrefixeMatch4() {
    assertEquals(None, prefixeMatch(Repete(UneBase(A)), List(G, G, G)))
  }

  /**
   * Test de la fonction      prefixeMatch()
   * avec comme paramètre(s)  (NFois(UneBase(A), 3), List(A, A, A, T)))
   * resultat attendu(s)      Some(List(A))
   */
  @Test
  def testPrefixeMatch5() {
    assertEquals(Some(List(A)), prefixeMatch(NFois(UneBase(A), 3), List(A, A, A, T)))
  }

  /**
   * Test de la fonction      suppPrefixe()
   * avec comme paramètre(s)  List(A, T), List(A, T, C, G)
   * resultat attendu(s)      List(C, G)
   */
  @Test
  def testSuppPrefixe1() {
    assertEquals(List(C, G), suppPrefixe(List(A, T), List(A, T, C, G)))
  }

  /**
   * Test de la fonction      suppPrefixe()
   * avec comme paramètre(s)  List(), List(A, T, C, G)
   * resultat attendu(s)      List(A, T, C, G)
   */
  @Test
  def testSuppPrefixe2() {
    assertEquals(List(A, T, C, G), suppPrefixe(List(), List(A, T, C, G)))
  }
  
  /**
   * Test de la fonction      suppPrefixe()
   * avec comme paramètre(s)  (Decrite,A),(NonDecrite,G)),tousLesMatchs(UneBase(A), List(C,A,G))
   * resultat attendu(s)      List((NonDecrite,C)
   */
  @Test
  def testTousLesMatchs1() {
     assertEquals(List((NonDecrite,C),(Decrite,A),(NonDecrite,G)),tousLesMatchs(UneBase(A), List(C,A,G)))
  }
  
  /**
   * Test de la fonction      suppPrefixe()
   * avec comme paramètre(s)  (Decrite,A),(NonDecrite,G)),tousLesMatchs(UneBase(A), List(C,A,G))
   * resultat attendu(s)      List((NonDecrite,C)
   */
  @Test
  def testTousLesMatchs2() {
     assertEquals(List((NonDecrite,C),(Decrite,A),(NonDecrite,G)),tousLesMatchs(UneBase(A), List(C,A,G)))
  }
}