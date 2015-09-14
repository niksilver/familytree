package org.pigsaw.familytree

/**
 * Created by Nik on 14 Sep 2015.
 */
class FamilyTree {
  self =>

  def +(p: Person): FamilyTree = {
    if (people contains p) {
      throw new Exception(s"Person $p already exists")
    }
    new FamilyTree {
      override val people = p +: self.people
    }
  }

  val people: Seq[Person] = Nil

  val parentChildPairs: Seq[(Person,Person)] = Nil

  def parentOf(parent: Person, child: Person): FamilyTree = {
    if (!people.contains(parent) || !people.contains(child)) {
      throw new Exception(s"Parent $parent is not in the tree")
    }
    new FamilyTree {
      override val people = self.people
      override val parentChildPairs = (parent, child) +: self.parentChildPairs
    }
  }

  def childOf(child: Person, parent: Person): FamilyTree = parentOf(parent, child)
}
