package dc10.cats.data

import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term

trait StateT[F[_], S, A]
trait Store[S, A]

trait Data[F[_]]:
  def STATET[G[_], S, A]: F[TypeExpr[StateT[G, S, A], Unit]]
  def STORE[S, A]: F[TypeExpr[Store[S, A], Unit]]
  def Store[S, A]: F[ValueExpr[Store[S, A], Unit]]
  // extension [S, A] (list: F[ValueExpr[Store[S, A], Unit]])
  //   @scala.annotation.targetName("appVStore")
  //   def apply[Y, Z](dir: F[ValueExpr[S => A, Y]], pos: F[ValueExpr[S, Z]]): F[ValueExpr[Store[S, A], (Unit, (Y, Z))]]

object Data:

  trait Mixins extends Data[[A] =>> cats.data.StateT[ErrorF, List[Statement], A]]:

    def STATET[G[_], S, A]: cats.data.StateT[ErrorF, List[Statement], TypeExpr[StateT[G, S, A], Unit]] =
      cats.data.StateT.pure[ErrorF, List[Statement], TypeExpr[StateT[G, S, A], Unit]](TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "StateT", None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))

    def STORE[S, A]: cats.data.StateT[ErrorF, List[Statement], TypeExpr[Store[S, A], Unit]] =
      cats.data.StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Store", None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
    def Store[S, A]: cats.data.StateT[ErrorF, List[Statement], ValueExpr[Store[S, A], Unit]] =
      for
        t <- STORE[S, A]
        s <- cats.data.StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Store", t.tpe, None)))
      yield s

    // extension [S, A] (store: cats.data.StateT[ErrorF, List[Statement], ValueExpr[Store[S, A], Unit]])
    //   @scala.annotation.targetName("appVStore")
    //   def apply[Y, Z](
    //     dir: cats.data.StateT[ErrorF, List[Statement], ValueExpr[S => A, Y]],
    //     pos: cats.data.StateT[ErrorF, List[Statement], ValueExpr[S, Z]]
    //   ): cats.data.StateT[ErrorF, List[Statement], ValueExpr[Store[S, A], (Unit, (Y, Z))]] =
    //     for
    //       s <- store
    //       d <- dir
    //       p <- pos
    //       v <- cats.data.StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Store[S, A], (Unit, (Y, Z))]](
    //         Term.ValueLevel.App.AppCtor2(
    //           None,
    //           s.value.tpe.manageDep(_ => (Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()), (d.value.tpe.dep, p.value.tpe.dep))),
    //           d.value.manageDep(_ => (Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()), (d.value.tpe.dep, p.value.tpe.dep))),
    //           p.value.manageDep(_ => (Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()), (d.value.tpe.dep, p.value.tpe.dep))))
    //       )
    //     yield ValueExpr(v)