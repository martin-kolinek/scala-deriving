import scala.language.higherKinds

package object deriving {
	def deriving[New, Cls[_]] = new DerivingUtil[New, Cls]() {}
}