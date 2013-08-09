package deriving

import scala.reflect.macros.Context
import scala.language.higherKinds
import scala.collection.mutable.ListBuffer
import sun.reflect.generics.tree.ReturnType

object DerivingImpl {
	
	def createClassDef[NewT, OldT, OldInst](c:Context)(to:c.Expr[NewT => OldT], from:c.Expr[OldT=>NewT], typeClass:c.universe.ClassSymbol, newInstance:c.Type, ev:c.Expr[OldInst]) = {
		import c.universe._
		val fromName = newTermName("$deriving$from")
		val toName = newTermName("$deriving$to")
		val fromTree = Ident(fromName)
		val toTree = Ident(toName)
		val fromVal = ValDef(NoMods, fromName, TypeTree(from.actualType), from.tree)
		val toVal = ValDef(NoMods, toName, TypeTree(to.actualType), to.tree)
		
		val typeClassParam = typeClass.typeParams(0)
		
		def processParameters(params:List[Symbol]) = {
			params.map {
			    case x if x.typeSignature.typeSymbol == typeClassParam => 
			        ValDef(NoMods, x.name.asInstanceOf[TermName], TypeTree(newInstance), EmptyTree) ->
			        Apply(toTree, List(Ident(x.name)))
			    case x => 
			        ValDef(NoMods, x.name.asInstanceOf[TermName], TypeTree(x.typeSignature), EmptyTree) ->
			        Ident(x.name)
			}.unzip
		}
		
		def processReturnValue(t:Tree, retType:Type) = {
			if(retType.typeSymbol == typeClassParam)
				Apply(fromTree, List(t))
			else
				t
		}
			
		def replaceType(t:Type) = {
			if(t.typeSymbol == typeClassParam)
				newInstance
			else
				t
		}
			
		val ctor = {
			val constructorBody = Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))
			DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), constructorBody)
		}
			
		val toChange = typeClass.typeSignature.members.collect {
		    case x:MethodSymbol 
		        if x.paramss.flatten.exists(_.typeSignature.typeSymbol == typeClassParam)
		            || x.returnType.typeSymbol == typeClassParam
		            => x
		}
			
		val changed = toChange.map {
		    case x => {
		    	val mods = NoMods
		    	val name = x.name.asInstanceOf[TermName]
		    	val tpt = TypeTree(replaceType(x.returnType))
		    	val base:Tree = Select(ev.tree, name)
		    	val (typeParamLists, paramLists) = x.paramss.map(processParameters(_)).unzip
		    	val body = processReturnValue((base /: paramLists)(Apply(_, _)), x.returnType)
		    	if(x.isVal) 
		    		ValDef(mods, name, tpt, body)
		    	else 
		    		DefDef(mods, name, Nil, typeParamLists, tpt, body)
		    }
		}.toList
			
		val newTypeClassCls = AppliedTypeTree(Ident(typeClass), List(TypeTree(newInstance)))
		val tmpl:Template = Template(List(newTypeClassCls),
				emptyValDef,
				List(ctor, fromVal, toVal) ++ changed)
		ClassDef(NoMods, newTypeName(s"${newInstance.typeSymbol.name}InstanceOf${typeClass.name}"), Nil, tmpl)
	}
	
	def macroImpl[NewT, OldT, NewInst, OldInst](c:Context)(to:c.Expr[NewT => OldT], from:c.Expr[OldT => NewT])(ev:c.Expr[OldInst])(implicit wttco:c.WeakTypeTag[OldInst], wtto:c.WeakTypeTag[OldT], wttn:c.WeakTypeTag[NewT], wttcn:c.WeakTypeTag[NewInst]) = {
		import c.universe._
		
		val ref = wttco.tpe match {
			case x:TypeRef => x
			case _ => c.abort(c.enclosingPosition, "Cls type is not a TypeRef")
		}
		val symb = ref.typeConstructor.typeSymbol
		val cls = symb match {
			case x:ClassSymbol => x
			case _ => c.abort(c.enclosingPosition, "Cls is not a class")
		}
		
		val cdef = createClassDef(c)(to, from, cls, wttn.tpe, ev)
		
		val const = Select(New(Ident(cdef.name)), nme.CONSTRUCTOR)
		val block = Block(List(cdef), Apply(const, Nil))
		c.Expr[NewInst](block)
	}
}
