package org.pigsaw.familytree

import org.scalatest.{ShouldMatchers, FlatSpec}

/**
 * Created by Nik on 14 Sep 2015.
 */
class PersonTest extends FlatSpec with ShouldMatchers {

  "Person" should "allow a male" in {
    Person("Fred Bloggs", Male)
  }

  it should "allow a female" in {
    Person("Fred Bloggs", Female)
  }

  it should "reject a person with name of five characters" in {
    an [Exception] should be thrownBy {
      Person("abcde", Female)
    }
  }

  it should "reject a person with name of four characters" in {
    an [Exception] should be thrownBy {
      Person("abcd", Female)
    }
  }

  it should "accept a person with name of six characters" in {
    Person("abcdef", Female)
  }

  it should "reject a person with name of 101 characters" in {
    val name = "A" * 101
    an [Exception] should be thrownBy {
      Person(name, Female)
    }
  }

  it should "accept a person with name of 100 characters" in {
    val name = "A" * 100
    Person(name, Female)
  }
}
