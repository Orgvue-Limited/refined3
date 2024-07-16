package refined3

import _root_.pureconfig.{
  BasicReaders,
  BasicWriters,
  CollectionReaders,
  CollectionWriters,
  ConfigConvert,
  ConfigCursor,
  ConfigReader,
  ConfigWriter
}
import _root_.pureconfig.error.{CannotConvert, ConfigReaderFailures, ConvertFailure}

import com.typesafe.config.ConfigValue

import scala.reflect.ClassTag

object pureconfig:
  given refineConfigConvert[T, P](using
    typeTag: ClassTag[Refined[T, P]],
    validate: Validate[T, P],
    show: Show[T, P],
    configConvert: ConfigConvert[T]
  ): ConfigConvert[Refined[T, P]] =
    new ConfigConvert[Refined[T, P]]:
      override def from(cur: ConfigCursor): Either[ConfigReaderFailures, Refined[T, P]] =
        configConvert.from(cur) match
          case Right(t) =>
            Refined.refineV(t)(using validate, show) match
              case Left(message) =>
                Left(
                  ConfigReaderFailures(
                    ConvertFailure(
                      reason = CannotConvert(
                        value = cur.valueOpt.map(_.render()).getOrElse("none"),
                        toType = typeTag.runtimeClass.getTypeName,
                        because = message
                      ),
                      cur = cur
                    )
                  )
                )

              case Right(refined) =>
                Right(refined)

          case Left(configReaderFailures) =>
            Left(configReaderFailures)

      override def to(t: Refined[T, P]): ConfigValue =
        configConvert.to(Refined.unwrap(t))

  given coercibleConfigConvert[A, B](using
    configConvert: ConfigConvert[B],
    coercibleA: Coercible[A, B],
    coercibleB: Coercible[B, A]
  ): ConfigConvert[A] =
    new ConfigConvert[A]:
      import refined3.Coercible.*

      override def from(cur: ConfigCursor): Either[ConfigReaderFailures, A] =
        configConvert.from(cur).map(_.coerce)

      override def to(t: A): ConfigValue =
        configConvert.to(t.coerce)