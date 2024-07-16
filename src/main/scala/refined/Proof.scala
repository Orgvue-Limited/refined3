package refined

type Proof[+T, P] = Proof.Successful[T, P] | Proof.Failure[T, P]

object Proof:
  sealed trait Successful[+T, P]
  sealed trait Failure[+T, P]

  private[refined] val successInstance: Successful[Any, Any] = new Successful[Any, Any] {}
  private[refined] val failureInstance: Failure[Any, Any]    = new Failure[Any, Any] {}

  private[refined] def success[T, P] =
    successInstance.asInstanceOf[Successful[T, P]]

  private[refined] def failure[T, P] =
    failureInstance.asInstanceOf[Failure[T, P]]