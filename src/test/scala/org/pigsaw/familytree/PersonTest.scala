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
}
