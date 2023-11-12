package dc10.cats.data

import cats.data.StateT
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term

trait Store[S, A]

trait Data[F[_]]:
  def STORE[S, A]: F[TypeExpr[Store[S, A], Unit]]
  def Store[S, A]: F[ValueExpr[Store[S, A], Unit]]
  extension [S, A] (list: F[ValueExpr[Store[S, A], Unit]])
    @scala.annotation.targetName("appVStore")
    def apply[Y, Z](dir: F[ValueExpr[S => A, Y]], pos: F[ValueExpr[S, Z]]): F[ValueExpr[Store[S, A], (Unit, (Y, Z))]]

object Data:

  trait Mixins extends Data[[A] =>> StateT[ErrorF, List[Statement], A]]:
    def STORE[S, A]: StateT[ErrorF, List[Statement], TypeExpr[Store[S, A], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Store", None, ())))
    def Store[S, A]: StateT[ErrorF, List[Statement], ValueExpr[Store[S, A], Unit]] =
      for
        t <- STORE[S, A]
        s <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Store", t.tpe, None)))
      yield s


    extension [S, A] (store: StateT[ErrorF, List[Statement], ValueExpr[Store[S, A], Unit]])
      @scala.annotation.targetName("appVStore")
      def apply[Y, Z](
        dir: StateT[ErrorF, List[Statement], ValueExpr[S => A, Y]],
        pos: StateT[ErrorF, List[Statement], ValueExpr[S, Z]]
      ): StateT[ErrorF, List[Statement], ValueExpr[Store[S, A], (Unit, (Y, Z))]] =
        for
          s <- store
          d <- dir
          p <- pos
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Store[S, A], (Unit, (Y, Z))]](
            Term.ValueLevel.App.AppCtor2(
              None,
              s.value.tpe.manageDep(_ => ((), (d.value.tpe.dep, p.value.tpe.dep))),
              d.value.manageDep(_ => ((), (d.value.tpe.dep, p.value.tpe.dep))),
              p.value.manageDep(_ => ((), (d.value.tpe.dep, p.value.tpe.dep))))
          )
        yield ValueExpr(v)