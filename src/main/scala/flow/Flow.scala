package flow
import cats.Traverse
import cats.implicits._
import cats.Applicative

case class Path(p: String)

case class Input(path: Path)
case class StoreInput(path: Path)

case class Step(inputs: List[Input])
case class ReifiedStep(inputs: List[StoreInput])

trait Store[F[_]] {
    def load(i: Input): F[StoreInput]
}

class Workflow[F[_] : Applicative](store: Store[F]) {
    def reify(s: Step): F[ReifiedStep] =   for {
            is <- s.inputs.traverse(store.load)
        } yield ReifiedStep(is)
}

object Flow {

}