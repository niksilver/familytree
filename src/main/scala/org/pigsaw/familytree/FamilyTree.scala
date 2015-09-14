package org.pigsaw.familytree

/**
 * Created by Nik on 14 Sep 2015.
 */
class FamilyTree {
  self =>

  def +(p: Person): FamilyTree = new FamilyTree {
    override val people = p +: self.people
  }

  val people: Seq[Person] = Nil
}