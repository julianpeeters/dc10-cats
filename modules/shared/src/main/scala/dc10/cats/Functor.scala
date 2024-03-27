package dc10.cats

import cats.data.StateT

import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.dsl.==>
import org.tpolecat.sourcepos.SourcePos

trait Functor[F[_], G[_]]:
  extension [A, X, Y] (fa: F[ValueExpr[G[A], (Unit, Y)]])
    def AS[B, Z](b: F[ValueExpr[B, Z]])(using sp: SourcePos): F[ValueExpr[G[B], (Unit, Z)]]
    def MAP[B, Z](f: F[ValueExpr[A => B, Z]])(using sp: SourcePos): F[ValueExpr[G[B], (Unit, Z)]]

object Functor:

  trait OptionMixins extends Functor[[A] =>> StateT[ErrorF, List[Statement], A], Option]:
    extension [A, X, Y] (fa: StateT[ErrorF, List[Statement], ValueExpr[Option[A], (Unit, Y)]])
      def AS[B, Z](b: StateT[ErrorF, List[Statement], ValueExpr[B, Z]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Option[B], (Unit, Z)]] =
        for
          o <- fa
          v <- b
          f <- o.value match
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) =>
              for
                // t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[B], (Unit, Z)]](Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.SomeType(None, tpe.dep), v.value.tpe, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())))
                t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[B], (Unit, Z)]](Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.SomeType(None, tpe.dep), v.value.tpe,
                  Term.ValueLevel.App.AppCtor2(None, "",
                      Term.TypeLevel.App.App2(
                        None,
                        Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                        Term.TypeLevel.Var.UnitType(None),
                        v.value.tpe.dep.tpe,
                        v.value.tpe.dep.tpe.dep
                    ),
                    Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                    Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                  )
                ))
                g <- b ==> ((s: ValueExpr[B, Z]) => StateT.pure[ErrorF, List[Statement], ValueExpr[Option[B], (Unit, Z)]](
                  ValueExpr(Term.ValueLevel.App.AppCtor1(None, t, arg.manageDep(_ => t.dep)))
                ))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[B => Option[B], (Unit, Z)]](ValueExpr[B => Option[B], (Unit, Z)](
                  Term.ValueLevel.Var.UserDefinedValue[B => Option[B], (Unit, Z)](None, "as", g.value.tpe.asInstanceOf[Term.TypeLevel[B => Option[B], (Unit, Z)]], Some(g.value.manageDep(_ => t.dep)))
                ))
              yield v
            case _ => StateT.liftF[ErrorF, List[Statement], ValueExpr[B => Option[B], (Unit, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nValue is not a Functor"))))
        yield ValueExpr(Term.ValueLevel.App.Dot1(
          None,
          f.value,
          o.value.manageDep(_ =>
            Term.ValueLevel.App.AppCtor2(None, "",
                Term.TypeLevel.App.App2(
                  None,
                  Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                  Term.TypeLevel.Var.UnitType(None),
                  v.value.tpe.dep.tpe,
                  v.value.tpe.dep.tpe.dep
              ),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            )
          ),
          v.value.manageDep(_ =>
            Term.ValueLevel.App.AppCtor2(None, "",
                Term.TypeLevel.App.App2(
                  None,
                  Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                  Term.TypeLevel.Var.UnitType(None),
                  v.value.tpe.dep.tpe,
                  v.value.tpe.dep.tpe.dep
              ),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            )
          ),
          Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.OptionType(None, v.value.tpe.dep), v.value.tpe, Term.ValueLevel.App.AppCtor2(None, "",
                Term.TypeLevel.App.App2(
                  None,
                  Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                  Term.TypeLevel.Var.UnitType(None),
                  v.value.tpe.dep.tpe,
                  v.value.tpe.dep.tpe.dep
              ),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            ))))

      def MAP[B, Z](f: StateT[ErrorF, List[Statement], ValueExpr[A => B, Z]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Option[B], (Unit, Z)]] =
        for
          o <- fa
          v <- f
          h <- o.value match
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) =>
              println(v.value.tpe)
              for
                t <- StateT.liftF[ErrorF, List[Statement], TypeExpr[Option[B], (Unit, Z)]](v.value.tpe match
                  case Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, dep) => Right(TypeExpr(Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.OptionType(None, dep), tb, Term.ValueLevel.App.AppCtor2(None, "",
                        Term.TypeLevel.App.App2(
                          None,
                          Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                          Term.TypeLevel.Var.UnitType(None),
                          v.value.tpe.dep.tpe,
                          v.value.tpe.dep.tpe.dep
                      ),
                      Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                      Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                    )
                  )))
                  case _ =>
                    println("WEWE")
                    Left(List(Error(s"${sp.file}:${sp.line}\nValue is not a Functor")))
                )
                g <- f ==> ((s: ValueExpr[A => B, Z]) => StateT.pure(ValueExpr[Option[B], (Unit, Z)](
                  Term.ValueLevel.App.AppCtor1(None, t.tpe, arg.manageDep(_ => t.tpe.dep)))))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[(A => B) => Option[B], (Unit, Z)]](
                  ValueExpr(Term.ValueLevel.Var.UserDefinedValue[(A => B) => Option[B], (Unit, Z)](None, "map", g.value.tpe.manageDep( _ => t.tpe.dep), None)))
              yield (t, v)
            case _ => StateT.liftF[ErrorF, List[Statement], (TypeExpr[Option[B], (Unit, Z)],ValueExpr[(A => B) => Option[B], (Unit, Z)])](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor")))) 
        // yield ValueExpr(Term.ValueLevel.App.Dot1(None, h._2.value, o.value.manageDep(_ => h._1.tpe.dep), v.value.manageDep(_ => h._1.tpe.dep), h._1.tpe))
        yield ValueExpr(Term.ValueLevel.App.Dot1(
          None, 
          h._2.value,
          o.value.manageDep(_ => Term.ValueLevel.App.AppCtor2(None, "",
                Term.TypeLevel.App.App2(
                  None,
                  Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                  Term.TypeLevel.Var.UnitType(None),
                  v.value.tpe.dep.tpe,
                  v.value.tpe.dep.tpe.dep
              ),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            )),
          v.value.manageDep(_ => Term.ValueLevel.App.AppCtor2(None, "",
                Term.TypeLevel.App.App2(
                  None,
                  Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                  Term.TypeLevel.Var.UnitType(None),
                  v.value.tpe.dep.tpe,
                  v.value.tpe.dep.tpe.dep
              ),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            )),
          h._1.tpe.manageDep(_ => Term.ValueLevel.App.AppCtor2(None, "",
                Term.TypeLevel.App.App2(
                  None,
                  Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                  Term.TypeLevel.Var.UnitType(None),
                  v.value.tpe.dep.tpe,
                  v.value.tpe.dep.tpe.dep
              ),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
              Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            ))
        ))
