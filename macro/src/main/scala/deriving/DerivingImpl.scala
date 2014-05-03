package deriving

import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds

//TODO: refactor
object DerivingImpl {

    def macroImpl[NewT, OldT, NewInst, OldInst](c: Context)(to: c.Expr[NewT => OldT], from: c.Expr[OldT => NewT])(ev: c.Expr[OldInst])(implicit wttco: c.WeakTypeTag[OldInst], wtto: c.WeakTypeTag[OldT], wttn: c.WeakTypeTag[NewT], wttcn: c.WeakTypeTag[NewInst]) = {
        import c.universe._

        def info(msg: Any) = c.info(c.enclosingPosition, msg.toString, true)

        case class TypeClassInfo(classSymb: ClassSymbol, tparams: IndexedSeq[Type], classParamIdx: Int) {
            def newInstance = tparams(classParamIdx)
            def basicTypeClassParam = classSymb.typeParams(classParamIdx)

            val replaceFunc: PartialFunction[Symbol, Tree] = {
                case s if classSymb.typeParams.contains(s) => TypeTree(tparams(classSymb.typeParams.indexOf(s)))
            }

            def typeParameters = tparams.patch(classParamIdx, Seq(newInstance), 1)
        }

        def getClassSymb(t: Type) = t.typeConstructor.typeSymbol match {
            case x: ClassSymbol => x
            case _ => c.abort(c.enclosingPosition, "Cls is not a class")
        }

        def getTypeClassInfo: TypeClassInfo = {
            wttco.tpe match {
                case x: TypeRef => TypeClassInfo(getClassSymb(x), IndexedSeq(wttn.tpe), 0)
                case PolyType(params, result) => {
                    (params, result) match {
                        case (List(param), TypeRef(_, typeClass, typeClassParams)) => {
                            val index = typeClassParams.indexWhere(_.typeSymbol == param)
                            val replacedParams = typeClassParams.toIndexedSeq.patch(index, Seq(wttn.tpe), 1)
                            TypeClassInfo(getClassSymb(result), replacedParams, index)
                        }
                        case _ => c.abort(c.enclosingPosition, "More than one type class type parameter")
                    }
                }
                case _ => c.abort(c.enclosingPosition, "Cls type is not a TypeRef")
            }
        }

        def createClassDef[NewT, OldT, OldInst](to: c.Expr[NewT => OldT], from: c.Expr[OldT => NewT], classInfo: TypeClassInfo, ev: c.Expr[OldInst]) = {

            def isDeferred(sym: Symbol) = sym
                .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
                .hasFlag(scala.reflect.internal.Flags.DEFERRED)

            val newInstance = classInfo.newInstance

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
                    typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)])
                base.zipWithIndex.map {
                    case (tp, i) => tp.typeConstructor.typeSymbol -> (i + 2)
                }.toMap
            }

            val fromName = TermName("$deriving$from")
            val toName = TermName("$deriving$to")
            val fromTree = Ident(fromName)
            val toTree = Ident(toName)
            val fromVal = ValDef(NoMods, fromName, TypeTree(from.actualType), from.tree)
            val toVal = ValDef(NoMods, toName, TypeTree(to.actualType), to.tree)

            def hasTypeClassParam(tp: Type): Boolean = {
                if (tp.typeSymbol == classInfo.basicTypeClassParam)
                    true
                else {
                    tp match {
                        case ref: TypeRef => ref.args.exists(hasTypeClassParam)
                        case _ => false
                    }
                }
            }

            def hasTypeToReplace(tp: Type): Boolean = {
                classInfo.replaceFunc.isDefinedAt(tp.typeSymbol) || {
                    tp match {
                        case ref: TypeRef => ref.args.exists(hasTypeToReplace)
                        case _ => false
                    }
                }
            }

            def hasIdentType(t: Type, identSymbols: Set[Symbol]): Boolean = {
                if (identSymbols.contains(t.typeSymbol))
                    true
                else t match {
                    case ref: TypeRef =>
                        ref.args.exists(hasIdentType(_, identSymbols))
                    case _ => false
                }
            }

            def typeParamNewTypeName(s: Symbol) = TypeName(s.name.decodedName.toString + "Deriv")

            def replaceType(t: Type, identSymbols: Set[Symbol]): Tree = {
                if (hasTypeClassParam(t) || hasIdentType(t, identSymbols) || hasTypeToReplace(t)) {
                    if (classInfo.replaceFunc.isDefinedAt(t.typeSymbol))
                        classInfo.replaceFunc(t.typeSymbol)
                    else if (identSymbols.contains(t.typeSymbol))
                        Ident(typeParamNewTypeName(t.typeSymbol))
                    else {
                        t match {
                            case ref: TypeRef => {
                                val newArgs = ref.args.map(replaceType(_, identSymbols))
                                AppliedTypeTree(Ident(t.typeConstructor.typeSymbol), newArgs)
                            }
                        }
                    }
                } else
                    TypeTree(t)
            }

            def fixUnapply(comp: Symbol, tp: TypeRef) = {
                comp.typeSignature.member(TermName("unapply")) match {
                    case m: MethodSymbol => {
                        if (m.paramLists.size != 1 || m.paramLists(0).size != 1)
                            c.abort(c.enclosingPosition, s"unapply of $comp has wrong number of parameters")
                        val paramSymb = m.paramLists(0)(0).typeSignature
                        val toSubst = paramSymb match {
                            case r: TypeRef => r.args.map(_.typeSymbol)
                            case _ => c.abort(c.enclosingPosition, "unapply parameter not a TypeRef")
                        }
                        m.returnType.substituteSymbols(toSubst, tp.args.map(_.typeSymbol))
                    }
                    case _ => c.abort(c.enclosingPosition, s"unapply of $comp is not a method")
                }
            }

            def transformTree(t: Tree, tp: Type, func: Tree): Tree = {
                if (hasTypeClassParam(tp)) {
                    if (tp.typeSymbol == classInfo.basicTypeClassParam)
                        Apply(func, List(t))
                    else {
                        tp match {
                            case ref: TypeRef => {
                                val comp = tp.typeSymbol.companion
                                val compApply = comp.typeSignature.member(TermName("apply"))
                                val unappType = fixUnapply(comp, ref)
                                val tup = unappType.asInstanceOf[TypeRef].args(0).asInstanceOf[TypeRef]
                                val size = tupleSize(tup.typeConstructor.typeSymbol)
                                val paramNames = (1 to size).map(nr => s"deriving$$param$$$nr").map(TermName(_))
                                val patterns = paramNames.map(x => pq"$x @ _")
                                val params = paramNames.zip(tup.args).map(x => transformTree(q"${x._1}", x._2, func))
                                q"""
							$t match {
							    case $comp(..$patterns) => $comp.apply(..$params)
						    }
							"""
                            }
                        }
                    }
                } else
                    t
            }

            val baseSymbols = (for {
                bs <- classInfo.classSymb.baseClasses.drop(1)
                if bs.isClass
                bstype = classInfo.classSymb.typeSignature.baseType(bs)
                (tp, symb) <- bstype.asInstanceOf[TypeRef].args zip bs.asClass.typeParams
            } yield symb -> tp.typeSymbol).unzip

            def substituteBaseSymbols(tpe: Type) = {
                tpe.substituteSymbols(baseSymbols._1, baseSymbols._2)
            }

            def processReturnValue(t: Tree, retType: Type): Tree =
                transformTree(t, substituteBaseSymbols(retType), fromTree)

            def createParameterTrees(params: List[Symbol]) = {
                params.map(x => x.name -> substituteBaseSymbols(x.typeSignature)).map {
                    case (name, tpe) if hasTypeClassParam(tpe) => transformTree(Ident(name), tpe, toTree)
                    case (name, tpe) => Ident(name)
                }
            }

            def createParameterValDefs(params: List[Symbol], identSymbols: Set[Symbol]) = {
                def getMods(impl: Boolean) = if (impl) Modifiers(Flag.IMPLICIT) else NoMods
                params.map(x => (x.name, substituteBaseSymbols(x.typeSignature), x.isImplicit)).map {
                    case (name, tpe, impl) if hasTypeClassParam(tpe) || hasIdentType(tpe, identSymbols) =>
                        ValDef(getMods(impl), name.asInstanceOf[TermName], replaceType(tpe, identSymbols), EmptyTree)
                    case (name, tpe, impl) =>
                        ValDef(getMods(impl), name.asInstanceOf[TermName], TypeTree(tpe), EmptyTree)
                }
            }

            val ctor = {
                val constructorBody = Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))
                DefDef(NoMods, termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), constructorBody)
            }

            val toChange = classInfo.classSymb.typeSignature.members.collect {
                case x: MethodSymbol if isDeferred(x) => x
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
                    val base: Tree = Select(ev.tree, name)
                    val paramValDefs = x.paramLists.map(createParameterValDefs(_, identSymbols))
                    val paramTrees = x.paramLists.map(createParameterTrees(_))
                    val body = processReturnValue((base /: paramTrees)(Apply(_, _)), x.returnType)
                    if (x.isVal)
                        ValDef(mods, name, tpt, body)
                    else
                        DefDef(mods, name, tparams, paramValDefs, tpt, body)
                }
            }.toList

            val newTypeClassCls = AppliedTypeTree(Ident(classInfo.classSymb), classInfo.typeParameters.map(TypeTree(_)).toList)
            val tmpl: Template = Template(List(newTypeClassCls),
                noSelfType,
                List(ctor, fromVal, toVal) ++ changed)
            ClassDef(NoMods, TypeName(s"${newInstance.typeSymbol.name}InstanceOf${classInfo.classSymb.name}"), Nil, tmpl)
        }

        val cdef = createClassDef(to, from, getTypeClassInfo, ev)

        val const = Select(New(Ident(cdef.name)), termNames.CONSTRUCTOR)
        val block = Block(List(cdef), Apply(const, Nil))
        c.Expr[NewInst](block)
    }

}
