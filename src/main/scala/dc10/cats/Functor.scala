package dc10.cats

import cats.data.StateT

import cats.Eval
import cats.free.Cofree
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import org.tpolecat.sourcepos.SourcePos
import dc10.scala.Symbol.Term.ValueLevel.App.App1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.dsl.==>

import dc10.scala.Symbol.Term.ValueLevel.Var.Println
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue
import dc10.scala.Symbol.Term.TypeLevel.App1
import dc10.scala.Symbol.Term.TypeLevel.App2
import dc10.scala.Symbol.Term.TypeLevel.App3

import dc10.scala.Symbol.Term.TypeLevel.Var.Function1Type

import dc10.scala.Symbol.Term.TypeLevel.Var.OptionType
import dc10.scala.Symbol.Term.TypeLevel.Var.OptionType.SomeType
import dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType


trait Functor[F[_], G[_]]:
  extension [A, X] (fa: F[ValueExpr[Unit, G[A]]])
    def AS[B](b: F[ValueExpr[Unit, B]])(using sp: SourcePos): F[ValueExpr[Unit, G[B]]]
    def MAP[B](f: F[ValueExpr[Unit, A => B]])(using sp: SourcePos): F[ValueExpr[Unit, G[B]]]

object Functor:

  trait OptionMixins extends Functor[[A] =>> StateT[ErrorF, List[Statement], A], Option]:
    extension [A, X] (fa: StateT[ErrorF, List[Statement], ValueExpr[Unit, Option[A]]])
      def AS[B](b: StateT[ErrorF, List[Statement], ValueExpr[Unit, B]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Unit, Option[B]]] =
        for
          o <- fa
          v <- b
          f <- o.value.tail.value match
            case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) =>
              for
                t <- StateT.pure[ErrorF, List[Statement], Term.Type[Unit, Option[B]]](Cofree((), Eval.now(Term.TypeLevel.App1(None, Cofree((), Eval.now(Term.TypeLevel.Var.OptionType.SomeType(None))), v.value.tail.value.tpe))))
                g <- b ==> ((s: ValueExpr[Unit, B]) => StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.App.AppCtor1(None, t, arg.asInstanceOf[Term.Value[Unit, A]]))))))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](ValueExpr[Unit, B => Option[B]](Cofree((), Eval.now(Term.ValueLevel.Var.UserDefinedValue[Unit, B => Option[B], Nothing](None, "as", g.value.tail.value.tpe.asInstanceOf[Term.Type[Unit, B => Option[B]]], Some(g.value))))))
              yield v
            case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.Var.Println(qnt, s) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, B => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, f.value, o.value, v.value))))

      def MAP[B](f: StateT[ErrorF, List[Statement], ValueExpr[Unit, A => B]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Unit, Option[B]]] =
        for
          o <- fa
          v <- f
          h <- o.value.tail.value match
            case Term.ValueLevel.App.App1(_, _, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) =>
              for
                t <- StateT.liftF[ErrorF, List[Statement], TypeExpr[Unit, Option[B]]](v.value.tail.value.tpe.tail.value match
                  case Term.TypeLevel.App1(qnt, tfun, targ) => Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor")))
                  case Term.TypeLevel.App2(qnt, tfun, ta, tb) => Right(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.App1(None, Cofree((), Eval.now(Term.TypeLevel.Var.OptionType(None))), tb)))))
                  case Term.TypeLevel.App3(qnt, tfun, ta1, ta2, tb) => Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor")))
                  case Term.TypeLevel.Var.Function1Type(qnt) => Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor")))
                  case Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl) => Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor")))
                )
                g <- f ==> ((s: ValueExpr[Unit, A => B]) => StateT.pure(ValueExpr[Unit, Option[B]](Cofree((), Eval.now(Term.ValueLevel.App.AppCtor1(None, t.tpe, arg.asInstanceOf[Term.Value[Unit, A]]))))))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.UserDefinedValue[Unit, (A => B) => Option[B], Nothing](None, "map", g.value.tail.value.tpe.asInstanceOf[Term.Type[Unit, (A => B) => Option[B]]], Some(g.value))))))
              yield v
            case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg)    => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor")))) 
            case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.Var.Println(qnt, s)            => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, (A => B) => Option[B]]](Left(List(Error(s"${sp.file}:${sp.line}\nValue in not a Functor"))))
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, h.value, o.value, v.value))))
