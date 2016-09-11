package fr.cnrs.liris.scars.api

trait Identifiable {
  
  def id: Int
  
  override def equals(other: Any) = other match {
    case subject: Identifiable => 
      getClass == subject.getClass && id == subject.id
    case _ => false
  }
  
  override def hashCode = id.hashCode
  
  override def toString = id.toString
  
}

trait Reviews {
  def reviews: Set[Review]
}

trait Similarities[Subject] {
  def similars: Set[Subject]
  def similarity(other: Subject): Option[Double]
}