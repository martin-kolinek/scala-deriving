package deriving

import scala.language.experimental.macros
import scala.language.higherKinds

trait DerivingUtil[New, Cls[_]] {
	def equiv[Old](to:New => Old, from: Old => New)(implicit ev:Cls[Old]):Cls[New] = macro DerivingImpl.macroImpl[New, Old, Cls[New], Cls[Old]]
}
