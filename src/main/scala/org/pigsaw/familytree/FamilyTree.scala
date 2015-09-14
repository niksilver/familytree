package org.pigsaw.familytree

/**
 * Created by Nik on 14 Sep 2015.
 */
class FamilyTree {
  self =>

  val people: Seq[Person] = Nil

  val parentChildPairs: Seq[(Person,Person)] = Nil

  def +(p: Person): FamilyTree = {
    if (people contains p) {
      throw new Exception(s"Person $p already exists")
    }
    new FamilyTree {
      override val people = p +: self.people
    }
  }

  def parentOf(parent: Person, child: Person): FamilyTree = {
    if (!people.contains(parent) || !people.contains(child)) {
      throw new Exception(s"Parent $parent is not in the tree")
    }
    if (parentsOf(child).length == 2) {
      throw new Exception("Child has enough parents already")
    }
    if (hasFather(child) && parent.sex == Male) {
      throw new Exception("Child already has a father")
    }
    if (hasMother(child) && parent.sex == Female) {
      throw new Exception("Child already has a mother")
    }
    new FamilyTree {
      override val people = self.people
      override val parentChildPairs = (parent, child) +: self.parentChildPairs
    }
  }

  def childOf(child: Person, parent: Person): FamilyTree = parentOf(parent, child)

  def parents = (parentChildPairs map ( _._1 )).distinct

  def mothers = parents filter {_.sex == Female}

  def fathers = parents filter {_.sex == Male}

  def parentsOf(child: Person) = (parentChildPairs filter (_._2 == child) map ( _._1 ))

  def hasFather(child: Person) = parentsOf(child) exists { _.sex == Male }

  def hasMother(child: Person) = parentsOf(child) exists { _.sex == Female }
}
