package flow

import cats.Traverse
import cats.implicits._
import cats.Applicative
import cats.Monad

case class Path(p: String)

case class Input(path: Path)
case class StoreInput(path: Path)

case class Step(inputs: List[Input], cmd: Input)
case class ReifiedStep(inputs: List[StoreInput], cmd: StoreInput, out: StoreInput)

case class ExitCode(code: Int)

case class Hash(s: String)

trait Store[F[_]] {
    def load(i: Input): F[StoreInput]
    def hash(is: List[StoreInput]): F[Hash]
    def outputPathFromHash(h: Hash): F[StoreInput]
}

class Workflow[F[_] : Monad](store: Store[F]) {
    def reify(s: Step): F[ReifiedStep] =   for {
            is <- s.inputs.traverse(store.load)
            c <- store.load(s.cmd)
            h <- store.hash(is ++ List(c))
            o <- store.outputPathFromHash(h)
        } yield ReifiedStep(is, c, o)
}

trait Runner[F[_]] {
    def run(step: ReifiedStep): F[ExitCode]
}

object Flow {

}