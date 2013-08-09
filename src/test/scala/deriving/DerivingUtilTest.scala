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

class DerivingUtilTest extends FunSuite {
	test("Deriving util test") {
				
		implicit val catIsCat = new IsCat[Cat] {
			def meow(cat:Cat) = "meow"
			def mate(cat1:Cat, cat2:Cat) = new Cat(s"${cat1.name} junior")
		}
		
		implicit val catInABoxIsCat = deriving[CatInABox, IsCat].equiv(_.cat, CatInABox)
		
		val cat = CatInABox(new Cat("cat"))
		
		implicitly[IsCat[CatInABox]].meow(cat)
	}
}