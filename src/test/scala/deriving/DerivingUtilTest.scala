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

trait Nested[T,Q] {
	def func(x:(T, T)):(T, T, Q)
}

trait BaseTypeClass[T, Q] {
	def funcT(t:T, q:Q):T
	def funcQ(t:T, q:Q):Q
}

trait SubTypeClass[T] extends BaseTypeClass[T, Int] {
	def func(t:T):T
}

case class NestedCClass[T](a:T, b:T)

trait BetterNested[T] extends Nested[T, Int] {
	def func2(x:NestedCClass[T]):NestedCClass[T]
}

trait IsPrivateCat[T] extends IsCat[T] {
	private def priv(t:T) = meow(t)
	protected[this] def protthis(t:T) = meow(t)
	def pub(t:T) = priv(t)
	def pub2(t:T) = protthis(t)
}

object Instances {
	implicit val catIsBetterCat = new IsBetterCat[Cat] {
		def meow(cat:Cat) = "meow"
		def mate(cat1:Cat, cat2:Cat) = new Cat(s"${cat1.name} junior")
		def scratch(cat:Cat) = 4
	}
}

class DerivingUtilTest extends FunSuite {
	
	
	test("deriving works") {
		import Instances._
		implicit val catInABoxIsCat = deriving[CatInABox, IsCat].equiv(_.cat, CatInABox)
		
		val cat = CatInABox(new Cat("cat"))
		
		assert(implicitly[IsCat[CatInABox]].meow(cat)=="meow")
		assert(implicitly[IsCat[CatInABox]].mate(cat, cat).cat.name == "cat junior")
	}
	
	test("deriving works with inheritance") {
		import Instances._
		implicit val catInABoxIsBetterCat = deriving[CatInABox, IsBetterCat].equiv(_.cat, CatInABox)
		val cat = CatInABox(new Cat("cat"))
		assert(implicitly[IsCat[CatInABox]].meow(cat)=="meow")
		assert(implicitly[IsCat[CatInABox]].mate(cat, cat).cat.name == "cat junior")
		assert(implicitly[IsBetterCat[CatInABox]].scratch(cat)==4)
	}
	
	test("deriving works with private members") {
		implicit val catIsPrivateCat = new IsPrivateCat[Cat] {
			def meow(cat:Cat) = "meow"
			def mate(cat1:Cat, cat2:Cat) = new Cat(s"${cat1.name} junior")
			protected def prot(cat:Cat) = "protected"
		}
		implicit val catInABoxIsPrivateCat = deriving[CatInABox, IsPrivateCat].equiv(_.cat, CatInABox)
		val cat = CatInABox(new Cat("cat"))
		assert(implicitly[IsPrivateCat[CatInABox]].pub(cat)=="meow")
		assert(implicitly[IsPrivateCat[CatInABox]].pub2(cat)=="meow")
	}
	
	test("deriving works with inheritance with subclass taking multiple type parameters") {
		implicit val catIsInstanceOfSubTypeClass = new SubTypeClass[Cat] {
			def func(c:Cat) = c
			def funcT(c:Cat, i:Int) = c
			def funcQ(c:Cat, i:Int) = i
		}
		
		implicit val catInABoxIsInstanceOfSubTypeClass = deriving[CatInABox, SubTypeClass].equiv(_.cat, CatInABox)
		
		val inst = implicitly[SubTypeClass[CatInABox]]
		assert(inst.func(CatInABox(new Cat("ferdinand"))).cat.name == "ferdinand")
		assert(inst.funcT(CatInABox(new Cat("ferdinand")), 1).cat.name == "ferdinand")
		assert(inst.funcQ(CatInABox(new Cat("ferdinand")), 2) == 2)
	}
	
	test("deriving works with type classes containing type arguments nested in other types") {
		implicit val catIsNested = new BetterNested[Cat] {
			def func(x:(Cat, Cat)) = (x._1, x._2, 4)
			def func2(x:NestedCClass[Cat]) = x
		}
		
		implicit val catInABoxIsNested = deriving[CatInABox, BetterNested].equiv(_.cat, CatInABox)
		
		val kitty = new Cat("kitty")
		val b = new Cat("b")
		val inst = implicitly[BetterNested[CatInABox]]
		assert(inst.func((CatInABox(kitty), CatInABox(b))) == (CatInABox(kitty), CatInABox(b), 4))
		val cclass = NestedCClass(CatInABox(kitty), CatInABox(b))
		assert(inst.func2(cclass) == cclass)
	}
}