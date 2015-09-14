package org.pigsaw.familytree

/**
 * Created by Nik on 14 Sep 2015.
 */
class FamilyTree {
  self =>

  /** Every `Person` in this family tree. */
  val people: Seq[Person] = Nil

  /** Every parent-child pair in this family tree. */
  val parentChildPairs: Seq[(Person,Person)] = Nil

  /** Add a person into this. */
  def +(p: Person): FamilyTree = {
    if (people contains p) {
      throw new Exception(s"Person $p already exists")
    }
    new FamilyTree {
      override val people = p +: self.people
      override val parentChildPairs = self.parentChildPairs
    }
  }

  /** Specify a parent-child relationship. */
  def parentOf(parent: Person, child: Person): FamilyTree = {
    if (!people.contains(parent) || !people.contains(child)) {
      throw new Exception(s"Parent $parent is not in the tree")
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

  /** Specify a child-parent relationshiop. */
  def childOf(child: Person, parent: Person): FamilyTree = parentOf(parent, child)

  /** All the parents. */
  def parents = (parentChildPairs map ( _._1 )).distinct

  /** All the female parents. */
  def mothers = parents filter {_.sex == Female}

  /** All the male parents. */
  def fathers = parents filter {_.sex == Male}

  /** Get the parents of a child. */
  def parentsOf(child: Person) = (parentChildPairs filter (_._2 == child) map ( _._1 ))

  /** Does the `child` have a father in this family tree? */
  def hasFather(child: Person) = parentsOf(child) exists { _.sex == Male }

  /** Does the `child` have a mother in this family tree? */
  def hasMother(child: Person) = parentsOf(child) exists { _.sex == Female }
}
