package dc10.cats

import cats.data.StateT

import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.dsl.==>
import org.tpolecat.sourcepos.SourcePos
// import dc10.scala.Symbol.Term.ValueLevel.App.App1
// import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
// import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
// import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
// import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
// import dc10.scala.Symbol.Term.ValueLevel.App.Dotless
// import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
// import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
// import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
// import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
// import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
// import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
// import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
// import dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor
// import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue

trait Functor[F[_], G[_]]:
  extension [A, X, Y] (fa: F[ValueExpr[G[A], (Unit, Y)]])
    def AS[B, Z](b: F[ValueExpr[B, Z]])(using sp: SourcePos): F[ValueExpr[G[B], (Unit, Z)]]
    def MAP[B, Z](f: F[ValueExpr[A => B, Unit]])(using sp: SourcePos): F[ValueExpr[G[B], (Unit, Z)]]

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
                t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[B], (Unit, Z)]](Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.SomeType(None, tpe.dep), v.value.tpe, ((), v.value.tpe.dep)))
                // g <- b ==> ((s: ValueExpr[B, Z]) => StateT.pure[ErrorF, List[Statement], ValueExpr[B => Option[B], (Unit, Z)]](ValueExpr(Term.ValueLevel.App.AppCtor1(None, ???, arg.asInstanceOf[Term.ValueLevel[A, (Unit, Z)]]))))
                g <- b ==> ((s: ValueExpr[B, Z]) => StateT.pure[ErrorF, List[Statement], ValueExpr[Option[B], (Unit, Z)]](
                  // t
                  // ValueExpr(Term.ValueLevel.App.AppPure(None, Term.ValueLevel.Var.OptionCtor(None, ""), ???, ???))
                  ValueExpr(Term.ValueLevel.App.AppCtor1(None, t, arg.manageDep(_ => t.dep)))
                ))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[B => Option[B], (Unit, Z)]](ValueExpr[B => Option[B], (Unit, Z)](
                  // Term.ValueLevel.Var.UserDefinedValue[B => Option[B], Z](None, "as", g.value.tpe.asInstanceOf[Term.TypeLevel[B => Option[B], Z]], Some(g.value))
                  Term.ValueLevel.Var.UserDefinedValue[B => Option[B], (Unit, Z)](None, "as", g.value.tpe.asInstanceOf[Term.TypeLevel[B => Option[B], (Unit, Z)]], Some(g.value.manageDep(_ => t.dep)))
                ))
              yield v
            case _ => StateT.liftF[ErrorF, List[Statement], ValueExpr[B => Option[B], (Unit, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nValue is not a Functor"))))
        yield ValueExpr(Term.ValueLevel.App.Dot1(None, f.value, o.value.manageDep(_ => ((), v.value.tpe.dep)), v.value.manageDep(_ => ((), v.value.tpe.dep)), Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.OptionType(None, v.value.tpe.dep), v.value.tpe, ((), v.value.tpe.dep))))

      def MAP[B, Z](f: StateT[ErrorF, List[Statement], ValueExpr[A => B, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Option[B], (Unit, Z)]] =
        for
          o <- fa
          v <- f
          h <- o.value match
            // case Term.ValueLevel.App.App1(_, _, _, _) => StateT.liftF[ErrorF, List[Statement], (TypeExpr[Option[B]],ValueExpr[(A => B) => Option[B], Unit]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) =>
              for
                t <- StateT.liftF[ErrorF, List[Statement], TypeExpr[Option[B], (Unit, Z)]](v.value.tpe match
                  case Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => Right(TypeExpr(Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.OptionType(None, dep), tb, ((), tb.dep.asInstanceOf[Z]))))
                  case _ => Left(List(Error(s"${sp.file}:${sp.line}\nValue is not a Functor")))
                )
                // g <- f ==> ((s: ValueExpr[A => B, Unit]) => StateT.pure(ValueExpr[Option[B], Unit](Term.ValueLevel.App.AppCtor1(None, t.tpe, arg.asInstanceOf[Term.ValueLevel[A, Unit]]))))
                g <- f ==> ((s: ValueExpr[A => B, Unit]) => StateT.pure(ValueExpr[Option[B], (Unit, Z)](
                  // Term.ValueLevel.App.AppCtor1(None, t.tpe, arg.asInstanceOf[Term.ValueLevel[A, (Unit, Z)]]))))
                  Term.ValueLevel.App.AppCtor1(None, t.tpe, arg.manageDep(_ => t.tpe.dep)))))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[(A => B) => Option[B], (Unit, Z)]](
                  // ValueExpr(Term.ValueLevel.Var.UserDefinedValue[(A => B) => Option[B], (Unit, Z)](None, "map", g.value.tpe.asInstanceOf[Term.TypeLevel[(A => B) => Option[B], (Unit, Z)]], Some(g.value))))
                  ValueExpr(Term.ValueLevel.Var.UserDefinedValue[(A => B) => Option[B], (Unit, Z)](None, "map", g.value.tpe.manageDep( _ => t.tpe.dep), None)))
              yield (t, v)
            case _ => StateT.liftF[ErrorF, List[Statement], (TypeExpr[Option[B], (Unit, Z)],ValueExpr[(A => B) => Option[B], (Unit, Z)])](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor")))) 
        // yield ValueExpr(Term.ValueLevel.App.Dot1(None, h._2.value, o.value, v.value, h._1.tpe))
        yield ValueExpr(Term.ValueLevel.App.Dot1(None, h._2.value, o.value.manageDep(_ => h._1.tpe.dep), v.value.manageDep(_ => h._1.tpe.dep), h._1.tpe))
