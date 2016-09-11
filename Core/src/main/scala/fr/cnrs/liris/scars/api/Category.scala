package fr.cnrs.liris.scars.api

import scala.annotation.tailrec

abstract class Category extends Identifiable {
  
  def name: String
  def parent: Option[Category]
  def items: Set[Item]
  def children: Set[Category]
  def experts: Set[Actor]
  
  @tailrec
  final def isAncestorOf(child: Category): Boolean = {
    this == child || (child.parent.isDefined && isAncestorOf(child.parent.get))
  }

}