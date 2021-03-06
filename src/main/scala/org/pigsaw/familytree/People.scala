package org.pigsaw.familytree

/**
 * Created by Nik on 14 Sep 2015.
 */
case class Person(name: String, sex: Sex) {
  if (name.length < 6) {
    throw new Exception("Name to short")
  }
  if (name.length > 100) {
    throw new Exception("Name too long")
  }
}

sealed class Sex
object Male extends Sex
object Female extends Sex
