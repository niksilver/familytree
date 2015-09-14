package org.pigsaw.familytree

import org.scalatest.FlatSpec
import org.scalatest.ShouldMatchers

/**
 * Created by Nik on 14 Sep 2015.
 */
class FamilyTreeTest extends FlatSpec with ShouldMatchers{

  "+" should "allow a person to be added" in {
    val t = new FamilyTree
    val p = Person("Fred Bloggs", Male)
    val t2 = t + p
  }

  it should "reject a person if they're already in the tree" in {
    val t1 = new FamilyTree
    val p = Person("Joan of Arc", Female)
    val t2 = t1 + p
    an [Exception] should be thrownBy {
      t2 + p
    }
  }

  it should "not obliterate relationships" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)

    t2.parentChildPairs should equal (Seq((p1, p2)))

    val t3 = t2 + Person("Interloper", Female)

    t3.parentChildPairs should equal (Seq((p1, p2)))
  }

  "people" should "be empty for an empty family tree" in {
    (new FamilyTree).people should be (empty)
  }

  it should "contain just the one person if one person added to an empty tree" in {
    val p = Person("Amy Bloggs", Female)
    val t = (new FamilyTree) + p
    t.people should equal (Seq(p))
  }

  it should "contain just two people if two people added to an empty tree" in {
    val p1 = Person("Amy Bloggs", Female)
    val p2 = Person("Fred Bloggs", Male)
    val t = (new FamilyTree) + p1 + p2
    t.people should contain only (p1, p2)
  }

  "parentChildPairs" should "be initially empty" in {
    val t = new FamilyTree
    t.parentChildPairs should be (empty)
  }

  "parentOf" should "allow us to add a parent and child" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val t = (new FamilyTree) + p1 + p2
    t.parentOf(p1, p2)
  }

  it should "register a new parent-child relationship" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)
    t2.parentChildPairs should equal (Seq((p1, p2)))
  }

  it should "register two new parent-child relationships" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val p3 = Person("Tiny Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2 + p3
    val t2 = t1.parentOf(p1, p2).parentOf(p2, p3)
    t2.parentChildPairs should contain only ((p1, p2), (p2, p3))
  }

  it should "reject a relationship if the parent is not already in the tree" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val t = (new FamilyTree) + p2
    an [Exception] should be thrownBy {
      val t2 = t.parentOf(p1, p2)
    }
  }

  it should "reject a relationship if the child is not already in the tree" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val t = (new FamilyTree) + p1
    an [Exception] should be thrownBy {
      val t2 = t.parentOf(p1, p2)
    }
  }

  it should "reject a relationship if the child has a third parent" in {
    val p1 = Person("Big Bloggs 1", Female)
    val p2 = Person("Big Bloggs 2", Male)
    val p3 = Person("Big Bloggs 3", Female)
    val p4 = Person("Little Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2 + p3 + p4
    val t2 = t1.parentOf(p1, p4).parentOf(p2, p4)
    an [Exception] should be thrownBy {
      t2.parentOf(p3, p4)
    }
  }

  it should "reject a relationship if we're adding a second father" in {
    val p1 = Person("Big Bloggs 1", Male)
    val p2 = Person("Big Bloggs 2", Male)
    val p3 = Person("Little Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2 + p3
    val t2 = t1.parentOf(p1, p3)
    an [Exception] should be thrownBy {
      t2.parentOf(p2, p3)
    }
  }

  it should "reject a relationship if we're adding a second mother" in {
    val p1 = Person("Big Bloggs 1", Female)
    val p2 = Person("Big Bloggs 2", Female)
    val p3 = Person("Little Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2 + p3
    val t2 = t1.parentOf(p1, p3)
    an [Exception] should be thrownBy {
      t2.parentOf(p2, p3)
    }
  }

  "childOf" should "allow us to add a child and parent" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2
    t1.childOf(p1, p2)
  }

  it should "register a new child-parent relationship" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.childOf(p2, p1)
    t2.parentChildPairs should equal (Seq((p1, p2)))
  }

  it should "register two new child-parent relationships" in {
    val p1 = Person("Big Bloggs", Female)
    val p2 = Person("Little Bloggs", Male)
    val p3 = Person("Tiny Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2 + p3
    val t2 = t1.childOf(p3, p2).childOf(p2, p1)
    t2.parentChildPairs should contain only ((p1, p2), (p2, p3))
  }

  "mothers" should "be empty for an empty tree" in {
    (new FamilyTree).mothers should be (empty)
  }

  it should "give one mother if only one mother added" in {
    val p1 = Person("Mother Bloggs", Female)
    val p2 = Person("Son Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)
    t2.mothers should equal (Seq(p1))
  }

  it should "give one mother if only one mother added but there's a another female" in {
    val p1 = Person("Mother Bloggs", Female)
    val p2 = Person("Daughter Bloggs", Female)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)
    t2.mothers should equal (Seq(p1))
  }

  it should "give one mother if one mother has two children" in {
    val p1 = Person("Mother Bloggs", Female)
    val p2 = Person("Daughter Bloggs", Female)
    val p3 = Person("Son Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2 + p3
    val t2 = t1.parentOf(p1, p2).parentOf(p1, p3)
    t2.mothers should equal (Seq(p1))
  }

  "fathers" should "be empty for an empty tree" in {
    (new FamilyTree).fathers should be (empty)
  }

  it should "give one father if only one father added" in {
    val p1 = Person("Father Bloggs", Male)
    val p2 = Person("Daughter Bloggs", Female)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)
    t2.fathers should equal (Seq(p1))
  }

  it should "give one father if only one father added but there's a another male" in {
    val p1 = Person("Fathre Bloggs", Male)
    val p2 = Person("Son Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)
    t2.fathers should equal (Seq(p1))
  }

  it should "give one father if one father has two children" in {
    val p1 = Person("Father Bloggs", Male)
    val p2 = Person("Daughter Bloggs", Female)
    val p3 = Person("Son Bloggs", Male)
    val t1 = (new FamilyTree) + p1 + p2 + p3
    val t2 = t1.parentOf(p1, p2).parentOf(p1, p3)
    t2.fathers should equal (Seq(p1))
  }

  "parentsOf" should "no parents of a parentless person" in {
    val p = Person("Bobbi Bloggs", Female)
    val t = (new FamilyTree) + p
    t.parentsOf(p) should be (empty)
  }

  it should "give one parent of a child if we have one child-parent relationship" in {
    val p1 = Person("Mother Bloggs", Female)
    val p2 = Person("Daughter Bloggs", Female)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)

    t2.parentsOf(p2) should equal (Seq(p1))
  }

  it should "give empty list if we ask for the parent of a parentless person" in {
    val p1 = Person("Mother Bloggs", Female)
    val p2 = Person("Daughter Bloggs", Female)
    val t1 = (new FamilyTree) + p1 + p2
    val t2 = t1.parentOf(p1, p2)

    t2.parentsOf(p1) should be (empty)
  }
}
