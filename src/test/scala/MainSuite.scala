package adts
import weaver.SimpleIOSuite
import cats.data.{ValidatedNec, NonEmptyChain}
import cats.data.Validated.Invalid

object MainSuite extends SimpleIOSuite:
  pureTest("Age from ExistingCode should produce a Option[Age]"){
    import ExistingCode.Age
    expect.all(
      Age(26).isInstanceOf[Option[Age]], 
      Age(24) == None
      ) 
  }

  pureTest("Height from ExistingCode should produce a Option[Height]"){
    import ExistingCode.Height
    expect.all(
      Height(5.5).isInstanceOf[Option[Height]], 
      Height(4.9) == None
      )
  }

  pureTest("Details from Adts should produce a ValidatedNec[DetailsError, Details]"){
    import Adts.{Details,DetailsError}
    import Adts.DetailsError.*
    expect.all(
      Details(27,5.5).isInstanceOf[ValidatedNec[DetailsError, Details]], 
      Details(24, 4.9) == Invalid(NonEmptyChain.of(AgeError, HeightError)))
  }

  pureTest("Details from Adts2 should produce a ValidatedNec[DetailsError, Details]"){
    import Adts2.{Details,DetailsError}
    import Adts2.DetailsError.*
    expect.all(
      Details(27,5.5).isInstanceOf[ValidatedNec[DetailsError, Details]], 
      Details(24, 4.9) == Invalid(NonEmptyChain.of(AgeError, HeightError)))
  }

  pureTest("Age from ExistingCode2 should produce a ValidatedNec[DetailsError, Age]"){
    import ExistingCode2.{Age, DetailsError}
    import adts.ExistingCode2.DetailsError.AgeError
    expect.all(
      Age(26).isInstanceOf[ValidatedNec[DetailsError, Age]], 
      Age(24) == Invalid(NonEmptyChain.of(AgeError))
      ) 
  }

  pureTest("Height from ExistingCode2 should produce a ValidatedNec[DetailsError, Height]"){
    import ExistingCode2.{Height, DetailsError}
    import adts.ExistingCode2.DetailsError.HeightError
    expect.all(
      Height(5.5).isInstanceOf[ValidatedNec[DetailsError, Height]], 
      Height(4.9) == Invalid(NonEmptyChain.of(HeightError))
      ) 
  }

  pureTest("Details from Adts3 should produce a ValidatedNec[DetailsError, Details]"){
    import Adts3.Details
    import adts.ExistingCode2.DetailsError
    import adts.ExistingCode2.DetailsError.{AgeError,HeightError}
    expect.all(
      Details(27,5.5).isInstanceOf[ValidatedNec[DetailsError, Details]], 
      Details(24, 4.9) == Invalid(NonEmptyChain.of(AgeError, HeightError)))
  }

  pureTest("Age from ExistingCode3 should produce a ValidatedNec[DetailsError, Age]"){
    import ExistingCode3.{Age, DetailsError}
    import adts.ExistingCode3.DetailsError.AgeError
    expect.all(
      Age(26).isInstanceOf[ValidatedNec[DetailsError, Age]], 
      Age(24) == Invalid(NonEmptyChain.of(AgeError))
      ) 
  }

  pureTest("Height from ExistingCode3 should produce a ValidatedNec[DetailsError, Height]"){
    import ExistingCode3.{Height, DetailsError}
    import ExistingCode3.DetailsError.HeightError
    expect.all(
      Height(5.5).isInstanceOf[ValidatedNec[DetailsError, Height]], 
      Height(4.9) == Invalid(NonEmptyChain.of(HeightError))
      ) 
  }

  pureTest("RelationshipStatus from Adts5 should produce a ValidatedNec[DetailsError, RelationshipStatus]"){
    import Adts5.{RelationshipStatus, RelationshipStatusError}
    import ExistingCode2.DetailsError
    expect.all(
      RelationshipStatus("married").isInstanceOf[ValidatedNec[DetailsError, RelationshipStatus]],
      RelationshipStatus("divorced") == Invalid(NonEmptyChain.of(RelationshipStatusError))
    )
  }

  pureTest("PersonDetails from Adts5 should produce a ValidatedNec[DetailsError, PersonDetails]"){
    import Adts5.{PersonDetails, RelationshipStatusError}
    import ExistingCode2.DetailsError
    import ExistingCode2.DetailsError.{AgeError,HeightError}
    expect.all(
      PersonDetails("available", 24, 4.5).isInstanceOf[ValidatedNec[DetailsError, PersonDetails]],
      PersonDetails("available", 24, 4.5) == Invalid(NonEmptyChain.of(RelationshipStatusError, AgeError, HeightError))
    )
  }

  pureTest("RelationshipStatus from Adts6 should produce a ValidatedNec[OtherError | DetailsError, RelationshipStatus]"){
    import Adts6.{RelationshipStatus, OtherError}
    import ExistingCode3.DetailsError
    import Adts6.OtherError.RelationshipStatusError
    expect.all(
      RelationshipStatus("married").isInstanceOf[ValidatedNec[OtherError | DetailsError, RelationshipStatus]],
      RelationshipStatus("divorced") == Invalid(NonEmptyChain.of(RelationshipStatusError))
    )
  }

  pureTest("PersonDetails from Adts6 should produce a ValidatedNec[OtherError | DetailsError, PersonDetails]"){
    import Adts6.{PersonDetails, OtherError}
    import ExistingCode3.DetailsError
    import ExistingCode3.DetailsError.{AgeError,HeightError}
    import Adts6.OtherError.RelationshipStatusError
    expect.all(
      PersonDetails("available", 24, 4.5).isInstanceOf[ValidatedNec[OtherError | DetailsError, PersonDetails]],
      PersonDetails("available", 24, 4.5) == Invalid(NonEmptyChain.of(RelationshipStatusError, AgeError, HeightError))
    )
  }