package deriving

import org.scalatest.FunSuite

trait IsCat[T] {
	def meow(cat:T):String
	def mate(cat1:T, cat2:T):T
}

class Cat(val name:String) {
}

case class CatInABox(cat:Cat) {
}

trait IsBetterCat[T] extends IsCat[T] {
	def scratch(cat:T):Int
}

object Instances {
	implicit val catIsBetterCat = new IsBetterCat[Cat] {
		def meow(cat:Cat) = "meow"
		def mate(cat1:Cat, cat2:Cat) = new Cat(s"${cat1.name} junior")
		def scratch(cat:Cat) = 4
	}
}

class DerivingUtilTest extends FunSuite {
	import Instances._
	
	test("deriving works") {
		implicit val catInABoxIsCat = deriving[CatInABox, IsCat].equiv(_.cat, CatInABox)
		
		val cat = CatInABox(new Cat("cat"))
		
		assert(implicitly[IsCat[CatInABox]].meow(cat)=="meow")
		assert(implicitly[IsCat[CatInABox]].mate(cat, cat).cat.name == "cat junior")
	}
	
	test("deriving works with inheritance") {
		implicit val catInABoxIsBetterCat = deriving[CatInABox, IsBetterCat].equiv(_.cat, CatInABox)
		val cat = CatInABox(new Cat("cat"))
		assert(implicitly[IsCat[CatInABox]].meow(cat)=="meow")
		assert(implicitly[IsCat[CatInABox]].mate(cat, cat).cat.name == "cat junior")
		assert(implicitly[IsBetterCat[CatInABox]].scratch(cat)==4)
	}
}