// Advanced Programming,  Andrzej Wąsowski
package adpro

object Example1 {

  // The running example (a nested value structure)

	case class Company (customers: Customer*)
	case class Customer (persons: Contact*)
	case class Contact (name: String, tags :String*)

	val c = Company (
      Customer (
        Contact ("Andrzej", "boring", "unpredictable"),
        Contact ("Paco", "helpful", "serious"),
        Contact ("Anders", "nice", "crazy"),
        Contact ("Alfredo", "clever", "absent-minded"),
        Contact ("Bugvi", "fast", "attentive"),
        Contact ("Nikolaj", "dilligent", "controlled")
      ),
			Customer (
        Contact("Mads", "important")
      )
    )





  // Let's get tags of Andrzej out of the 'c' object
  // and add a new tag

  val tags = c                     // The company
    .customers                     // get the sequence of customers
    .flatMap { _.persons }         // get the sequence of all persons we know
    .find { _.name == "Andrzej" }  // find the person called Andrzej
    .map { _.tags }                // if such a person exists (Some) get tags
    .getOrElse (Nil)
    .toList

  // Add this point we want to add a new tag, but we cannot.  The following
  // does not change 'c' at all!

  "annoying" :: tags

  // In Java we could just do, but immutable lists do not have 'add'

  // tags.add ("annoying")

  // So we are left with something like the following

  def revise (person: Contact): Contact =
    person.name match {
      case "Andrzej" =>
        Contact (person.name, person.tags.prepended ("annoying"): _*)
      case _ => person
    }

  val customers1 = c
    .customers
    .map[Customer] {
      case Customer (persons @ _*) =>
        Customer (persons map { revise _ } :_*)
    }

  val c1 = Company (customers1 : _*)

  // This is very complex, and the access pattern is completely hidden!

}




object Example2 {

  // If we did not use varargs above, we could benefit from 'copy'
  // ('copy' is not generated for classes with variable lists of arguments in
  // constructors).
  //
  // Let's implement copy manually for our classes:

	case class Company (customers: Customer*) {

    def copy (customers: Seq[Customer] = this.customers) =
      Company (customers: _*)
  }

	case class Customer (persons: Contact*) {

    def copy (persons: Seq[Contact] = this.persons) =
      Customer (persons: _*)
  }

	case class Contact (name: String, tags: String*) {

    def copy (name: String = this.name, tags: Seq[String] = this.tags): Contact =
      Contact (name, tags: _*)
  }



  // No change here
	val c = Company (
      Customer (
        Contact ("Andrzej", "boring", "unpredictable"),
        Contact ("Paco", "helpful", "serious"),
        Contact ("Anders", "nice", "crazy"),
        Contact ("Alfredo", "clever", "absent-minded"),
        Contact ("Bugvi", "fast", "attentive"),
        Contact ("Nikolaj", "dilligent", "controlled")
      ),
			Customer (
        Contact("Mads", "important")
      )
    )



  // We can use copy to simplify things a bit, but not much
  // (still useful that Scala generates copy for  most classes)

  def revise (person: Contact): Contact =
    person.name match {
      case "Andrzej" =>
        person.copy (tags = person.tags.prepended ("annoying"))
      case _ =>
        person
    }

  val customers1 = c
    .customers
    .map[Customer] {
      case Customer (persons @ _*) =>
        Customer (persons map { revise _ } :_*)
    }

  val c1 = c.copy (customers = customers1)

  // Here 'copy' arguably does not help much, but if Contact/Company had many
  // more fields, which do not change, we would like it!
  //
  // But this helps only a bit. If 'a' is nested several times in an object
  // (like Company).  we still have to modify the containing objects.
  //
  // We need a systematic way to access and change deeply nested values.
  //
  // This is what Lenses provide.
}






object Example3 {

  import monocle._
  import monocle.macros.GenLens
  import monocle.macros.syntax.lens._

  // An example with lenses (Monocle syntax):

  case class Company (customers: Customer*) {

    def copy (customers: Seq[Customer] = this.customers) =
      Company (customers: _*)
  }

  case class Customer (persons: Contact*) {

    def copy (persons: Seq[Contact] = this.persons) =
      Customer (persons: _*)
  }

	case class Contact (name: String, tags: String*) {

    def copy (name: String = this.name, tags: Seq[String] = this.tags): Contact =
      Contact (name, tags: _*)
  }

	val c = Company (
      Customer (
        Contact ("Andrzej", "boring", "unpredictable"),
        Contact ("Paco", "helpful", "serious"),
        Contact ("Anders", "nice", "crazy"),
        Contact ("Alfredo", "clever", "absent-minded"),
        Contact ("Bugvi", "fast", "attentive"),
        Contact ("Nikolaj", "dilligent", "controlled")
      ),
			Customer (
        Contact("Mads", "important")
      )
    )


  // We jump over these in the lecture presentation (you can study them after
  // class, or we can explain a bit in the end of the lecture if there is time)

  // A lens that extracts tags from a contact and allows modifying them
  val _tags = GenLens[Contact] (_.tags)


  // A lens that extracts a contact by name and allows modifying it
  // It is partial, because it may happen that the contact with the given name
  // does not exist
  def _person (name: String): Optional[Customer, Contact] = {

    def get (customer: Customer): Option[Contact] =
      customer.persons.find { _.name == name }

    def set (person: Contact) (customer: Customer): Customer =
      customer.copy (customer
        .persons
        .filter { _.name != name } // remove previous entries with the same name
        .prepended (person)        // assuming no duplicates and order insignificant
      )

    Optional (get) (set)
  }


  //  A traversal lense that allows to do something for each customer in a seq
  val eachCustomer = Traversal.fromTraverse[Seq, Customer]

  val c1 = c
    .lens (_.customers)                      // Access customers
    .composeTraversal  (eachCustomer)        // For each
    .composeOptional (_person ("Andrzej"))   // Get the person with name "Andrzej"
    .composeLens (_tags)                     // Extract his tags
    .modify (_.prepended ("annoying"))       // Prepend annoying to tags.

  // - Note: this is essentially as concise as the imperative version, but it is
  //   referentially transparent.
  //
  // - The composed lenses reconstruct the suitable Company object.
  //
  // ### In my experience:
  //
  // - Lenses pay off if you have to write many such expressions (for 1-2
  //   complex accesses still better to write a dedicated function)
  //
  // - Lenses are quite hard to use (although experience helps a lot over time)
  //
  // - Lenses are easier to read than to write (which in principle is a good
  //   property for an API)

}

import monocle.Optional
import monocle.Lens


object Example4 {

  def index[K,V] (k: K): Optional[Map[K,V],V] = {
    def get (m: Map[K,V]): Option[V] = m.get (k)
    def set (v: V) (m: Map[K,V]): Map[K,V] = m + (k->v)
    Optional[Map[K,V],V] (get)  (set _)
  }


  def compose[S,A,T] (l: Lens[S,A]) (k: Lens[A,T]): Lens[S,T] = {
    def get (s: S): T = k.get (l.get (s))
    def set (t: T) (s: S): S =
      l.set (k.set (t) (l.get (s))) (s)
    Lens[S,T]  (get) (set _)
  }

}
