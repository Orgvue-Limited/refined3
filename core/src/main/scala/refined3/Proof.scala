package refined3

type Proof[+T, P] = Proof.Successful[T, P] | Proof.Failure[T, P]

object Proof:
  sealed trait Successful[+T, P]
  sealed trait Failure[+T, P]

  private[refined3] val successInstance: Successful[Any, Any] = new Successful[Any, Any] {}
  private[refined3] val failureInstance: Failure[Any, Any]    = new Failure[Any, Any] {}

  private[refined3] def success[T, P] =
    successInstance.asInstanceOf[Successful[T, P]]

  private[refined3] def failure[T, P] =
    failureInstance.asInstanceOf[Failure[T, P]]