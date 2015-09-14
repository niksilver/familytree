package org.pigsaw.familytree

/**
 * Created by Nik on 14 Sep 2015.
 */
case class Person(name: String, sex: Sex)

sealed class Sex
object Male extends Sex
object Female extends Sex
