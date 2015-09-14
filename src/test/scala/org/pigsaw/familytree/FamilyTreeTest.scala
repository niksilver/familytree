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
}
