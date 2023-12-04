package adts

import cats.effect.{IOApp, IO}
import cats.syntax.all.*
import cats.data.{ValidatedNec, Validated}
import cats.data.Validated.{Valid, Invalid}
import cats.data.NonEmptyChain
import cats.Apply
import cats.data.NonEmptyChainImpl.Type
import cats.Semigroupal

object ExistingCode:
  case class Age(value: Int)
  object Age:
    def apply(value: Int): Option[Age] =
      (value > 25).guard[Option].as(new Age(value))

  case class Height(value: Double)
  object Height:
    def apply(value: Double): Option[Height] =
      (value > 5.0).guard[Option].as(new Height(value))

object Adts:
  import ExistingCode.*
  enum DetailsError(val err: String):
    case AgeError extends DetailsError("Required age is 25 years or more")
    case HeightError extends DetailsError("Required height is 5 feet or more.")

  case class Details(age: Age, height: Height)
  object Details:
    import DetailsError.*
    def apply(age: Int, height: Double): ValidatedNec[DetailsError, Details] =
      (
        Age(age).toValidNec(AgeError),
        Height(height).toValidNec(HeightError)
      ).mapN(new Details(_, _))

object Adts2:
  import ExistingCode.*
  sealed trait DetailsError:
    val errMsg: String
  object DetailsError:
    case object AgeError extends DetailsError:
      override val errMsg: String = "Required age is 25 years or more"
    case object HeightError extends DetailsError:
      override val errMsg: String = "Required height is 5 feet or more."

  case class Details(age: Age, height: Height)

  object Details:
    import DetailsError.*

    // The Semigroupal instance

    // given smg: Semigroupal[[A] =>> Validated[
    //   NonEmptyChain[AgeError.type] | NonEmptyChain[HeightError.type],
    //   A
    // ]] with
    //   override def product[A, B]
    //   (fa: Validated[Type[AgeError.type] | Type[HeightError.type], A],
    //   fb: Validated[Type[AgeError.type] | Type[HeightError.type], B]):
    //      Validated[Type[AgeError.type] | Type[HeightError.type], (A, B)] = ???

    given smg: Apply[[A] =>> Validated[
      NonEmptyChain[AgeError.type] | NonEmptyChain[HeightError.type],
      A
    ]] with
      override def ap[A, B](
        ff: Validated[Type[AgeError.type] | Type[HeightError.type], A => B]
      )(
        fa: Validated[Type[AgeError.type] | Type[HeightError.type], A]
      ): Validated[Type[AgeError.type] | Type[HeightError.type], B] =
        (fa, ff) match
          case (Valid(a), Valid(fab))     => Valid(fab(a))
          case (i @ Invalid(_), Valid(_)) => i
          case (Valid(_), i @ Invalid(_)) => i
          case (Invalid(e1), Invalid(e2)) =>
            Invalid(
              e2.combineK(e1)
                .asInstanceOf[Type[AgeError.type] | Type[HeightError.type]]
            )

      override def map[A, B](
        fa: Validated[Type[AgeError.type] | Type[HeightError.type], A]
      )(f: A => B): Validated[Type[AgeError.type] | Type[HeightError.type], B] =
        fa match
          case Valid(a)       => Valid(f(a))
          case i @ Invalid(e) => i
    end smg

    def apply(age: Int, height: Double): ValidatedNec[DetailsError, Details] =
      (
        Age(age).toValidNec(AgeError),
        Height(height).toValidNec(HeightError)
      ).mapN(new Details(_, _))

  end Details

object ExistingCode2:
  sealed trait DetailsError:
    val errMsg: String
  object DetailsError:
    case object AgeError extends DetailsError:
      override val errMsg: String = "Required age is 25 years or more"
    case object HeightError extends DetailsError:
      override val errMsg: String = "Required height is 5 feet or more."

  import DetailsError.*
  case class Age(value: Int)
  object Age:
    def apply(value: Int): ValidatedNec[DetailsError, Age] =
      Validated.condNec(
        value > 25,
        new Age(value),
        AgeError
      )

  case class Height(value: Double)
  object Height:
    def apply(value: Double): ValidatedNec[DetailsError, Height] =
      Validated.condNec(
        value > 5.0,
        new Height(value),
        HeightError
      )

object Adts3:
  import ExistingCode2.*
  case class Details(age: Age, height: Height)
  object Details:
    def apply(age: Int, height: Double): ValidatedNec[DetailsError, Details] =
      (
        Age(age),
        Height(height)
      ).mapN(new Details(_, _))

object ExistingCode3:
  enum DetailsError(val err: String):
    case AgeError extends DetailsError("Required age is 25 years or more")
    case HeightError extends DetailsError("Required height is 5 feet or more.")

  import DetailsError.*
  case class Age(value: Int)
  object Age:
    def apply(value: Int): ValidatedNec[DetailsError, Age] =
      Validated.condNec(
        value > 25,
        new Age(value),
        AgeError
      )

  case class Height(value: Double)
  object Height:
    def apply(value: Double): ValidatedNec[DetailsError, Height] =
      Validated.condNec(
        value > 5.0,
        new Height(value),
        HeightError
      )

object Adts4:
  import ExistingCode3.*
  case class Details(age: Age, height: Height)
  object Details:
    def apply(age: Int, height: Double): ValidatedNec[DetailsError, Details] =
      (
        Age(age),
        Height(height)
      ).mapN(new Details(_, _))

object Adts5:
  import ExistingCode2.DetailsError
  import Adts3.*

  case object RelationshipStatusError extends DetailsError:
    override val errMsg: String = "Must be either single or married"

  import RelationshipStatusError.*
  case class RelationshipStatus(value: String)
  object RelationshipStatus:
    def apply(value: String): ValidatedNec[DetailsError, RelationshipStatus] =
      Validated.condNec(
        value == "married" || value == "single",
        new RelationshipStatus(value),
        RelationshipStatusError
      )

  case class PersonDetails(
    relationshipStatus: RelationshipStatus,
    details: Details
  )
  object PersonDetails:
    def apply(
      relationshipStatus: String,
      age: Int,
      height: Double
    ): ValidatedNec[DetailsError, PersonDetails] =
      (
        RelationshipStatus(relationshipStatus),
        Details(age, height)
      ).mapN(new PersonDetails(_, _))

object Adts6:
  import ExistingCode3.DetailsError
  import Adts4.*

  enum OtherError(val err: String):
    case RelationshipStatusError
      extends OtherError("Must be either single or married")

  import OtherError.*
  case class RelationshipStatus(value: String)
  object RelationshipStatus:
    def apply(
      value: String
    ): ValidatedNec[OtherError | DetailsError, RelationshipStatus] =
      Validated.condNec(
        value == "married" || value == "single",
        new RelationshipStatus(value),
        RelationshipStatusError
      )

  case class PersonDetails(
    relationshipStatus: RelationshipStatus,
    details: Details
  )
  object PersonDetails:
    def apply(
      relationshipStatus: String,
      age: Int,
      height: Double
    ): ValidatedNec[OtherError | DetailsError, PersonDetails] =
      (
        RelationshipStatus(relationshipStatus),
        Details(age, height)
      ).mapN(new PersonDetails(_, _))

object AdtProgram extends IOApp.Simple:
  import Adts6.*
  override def run: IO[Unit] =
    PersonDetails("available", 24, 4.5) match
      case Valid(a)   => IO.println(a)
      case Invalid(e) => IO.println(e.toChain)
