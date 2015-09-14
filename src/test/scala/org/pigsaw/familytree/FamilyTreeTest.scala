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
}
