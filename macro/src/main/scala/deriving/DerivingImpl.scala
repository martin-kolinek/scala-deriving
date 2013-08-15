package deriving

import scala.reflect.macros.Context
import scala.language.higherKinds
import scala.collection.mutable.ListBuffer
import sun.reflect.generics.tree.ReturnType
import sun.reflect.generics.tree.TypeSignature

object DerivingImpl {
	
	def createClassDef[NewT, OldT, OldInst](c:Context)(to:c.Expr[NewT => OldT], from:c.Expr[OldT=>NewT], typeClass:c.universe.ClassSymbol, newInstance:c.Type, ev:c.Expr[OldInst]) = {
		import c.universe._
		
		def info(msg:Any) = c.info(c.enclosingPosition, msg.toString, true)
		
		def isDeferred(sym: Symbol) = sym
         .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
         .hasFlag(scala.reflect.internal.Flags.DEFERRED)
		
		val tupleSize = {
			val base = List(
				typeOf[(_, _)],
				typeOf[(_, _, _)],
				typeOf[(_, _, _, _)],
				typeOf[(_, _, _, _, _)],
				typeOf[(_, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)],
				typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)]
				)
			base.zipWithIndex.map {
				case(tp, i) => tp.typeConstructor.typeSymbol -> (i+2)
			}.toMap
		} 
		
		val fromName = newTermName("$deriving$from")
		val toName = newTermName("$deriving$to")
		val fromTree = Ident(fromName)
		val toTree = Ident(toName)
		val fromVal = ValDef(NoMods, fromName, TypeTree(from.actualType), from.tree)
		val toVal = ValDef(NoMods, toName, TypeTree(to.actualType), to.tree)
		
		val basicTypeClassParam = typeClass.typeParams(0)
		
		def hasTypeClassParam(tp:Type):Boolean = {
			if(tp.typeSymbol == basicTypeClassParam)
				true
			else {
				tp match {
					case ref:TypeRef => ref.args.exists(hasTypeClassParam(_))
					case _ => false
				}
			} 
		}
		
		def hasIdentType(t:Type, identSymbols:Set[Symbol]):Boolean = {
		    if(identSymbols.contains(t.typeSymbol))
		        true
		    else t match {
		        case ref:TypeRef => {
		            ref.args.exists(hasIdentType(_, identSymbols))
		        }
		        case _ => false
		    }
		}
		
		def typeParamNewTypeName(s:Symbol) = newTypeName(s.name.decoded+"Deriv")
		
		def replaceType(t:Type, identSymbols:Set[Symbol]):Tree = {
			if(hasTypeClassParam(t) || hasIdentType(t, identSymbols)) {
				if(t.typeSymbol == basicTypeClassParam)
					TypeTree(newInstance)
				else if (identSymbols.contains(t.typeSymbol))
				    Ident(typeParamNewTypeName(t.typeSymbol))
				else {
					t match {
						case ref:TypeRef => {
							val newArgs = ref.args.map(replaceType(_, identSymbols))
							AppliedTypeTree(Ident(t.typeConstructor.typeSymbol), newArgs)
						}
					}
				}
			}
			else
				TypeTree(t)
		}
		
		def fixUnapply(comp:Symbol, tp:TypeRef) = {
			comp.typeSignature.member("unapply":TermName) match {
			    case m:MethodSymbol => {
			    	if(m.paramss.size != 1 || m.paramss(0).size != 1)
			    		c.abort(c.enclosingPosition, s"unapply of $comp has wrong number of parameters")
			    	val paramSymb = m.paramss(0)(0).typeSignature
			    	val toSubst = paramSymb match {
			    		case r:TypeRef => r.args.map(_.typeSymbol)
			    		case _ => c.abort(c.enclosingPosition, "unapply parameter not a TypeRef")
			    	}
			    	m.returnType.substituteSymbols(toSubst, tp.args.map(_.typeSymbol))
			    }
			    case _ => c.abort(c.enclosingPosition, s"unapply of $comp is not a method")
			}
		}
		
		def transformTree(t:Tree, tp:Type, func:Tree):Tree = {
			if(hasTypeClassParam(tp)) {
				if(tp.typeSymbol == basicTypeClassParam)
					Apply(func, List(t))
				else {
					tp match {
						case ref:TypeRef => {
							val comp = tp.typeSymbol.companionSymbol
							val unappType = fixUnapply(comp, ref)
							val tup = unappType.asInstanceOf[TypeRef].args(0).asInstanceOf[TypeRef]
							val size = tupleSize(tup.typeConstructor.typeSymbol)
							val params = (1 to size).map(nr => s"$$deriving$$param$$$nr").toList//.map(n => newTermName(n)).toList
							val appliedParams = for((p, pt) <- params.map(x=>Ident(newTermName(x))).zip(tup.args)) yield transformTree(p, pt, func)
							val cse = CaseDef(
									Apply(Ident(comp), 
											params.map(p => Bind(newTermName(p), Ident(nme.WILDCARD)))),
									EmptyTree,
									Apply(Select(Ident(comp), "apply":TermName), appliedParams)
									)
							Match(t, List(cse))
						}
					}
				}
			}
			else
				t
		}
		
		val baseSymbols = (for {
			bs <- typeClass.baseClasses.drop(1)
			if bs.isClass
			bstype = typeClass.typeSignature.baseType(bs)
			(tp, symb) <- bstype.asInstanceOf[TypeRef].args zip bs.asClass.typeParams
		} yield symb -> tp.typeSymbol).unzip
		
		def substituteBaseSymbols(tpe:Type) = {
			tpe.substituteSymbols(baseSymbols._1, baseSymbols._2)
		}
		
		def processReturnValue(t:Tree, retType:Type):Tree = 
			transformTree(t, substituteBaseSymbols(retType), fromTree)
		
		def createParameterTrees(params:List[Symbol]) = {
		    params.map(x=>x.name -> substituteBaseSymbols(x.typeSignature)).map {
		        case (name, tpe) if hasTypeClassParam(tpe) => transformTree(Ident(name), tpe, toTree)
		        case (name, tpe) => Ident(name)
		    }
		}
		
		def createParameterValDefs(params:List[Symbol], identSymbols:Set[Symbol]) = {
		    params.map(x=>x.name -> substituteBaseSymbols(x.typeSignature)).map {
		        case (name, tpe) if hasTypeClassParam(tpe) || hasIdentType(tpe, identSymbols) =>
		            ValDef(NoMods, name.asInstanceOf[TermName], replaceType(tpe, identSymbols), EmptyTree)
		        case (name, tpe) =>
		            ValDef(NoMods, name.asInstanceOf[TermName], TypeTree(tpe), EmptyTree)
		    }
		}
		
		val ctor = {
			val constructorBody = Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))
			DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), constructorBody)
		}
			
		val toChange = typeClass.typeSignature.members.collect {
		    case x:MethodSymbol 
		        if isDeferred(x)
		            => x
		}
			
		val changed = toChange.map {
		    case x => {
		    	val tparams = x.typeParams.map { p =>
		    		TypeDef(Modifiers(Flag.PARAM), typeParamNewTypeName(p), Nil, TypeBoundsTree(Ident(typeOf[Nothing].typeSymbol), Ident(typeOf[Any].typeSymbol)))
		    	}
		    	val identSymbols = x.typeParams.toSet
		    	val mods = Modifiers(Flag.OVERRIDE)
		    	val name = x.name.asInstanceOf[TermName]
		    	val tpt = replaceType(substituteBaseSymbols(x.returnType), identSymbols)
		    	val base:Tree = Select(ev.tree, name)
		    	val paramValDefs = x.paramss.map(createParameterValDefs(_, identSymbols))
		    	val paramTrees = x.paramss.map(createParameterTrees(_))
		    	val body = processReturnValue((base /: paramTrees)(Apply(_, _)), x.returnType)
		    	if(x.isVal) 
		    		ValDef(mods, name, tpt, body)
		    	else 
		    		DefDef(mods, name, tparams, paramValDefs, tpt, body)
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
