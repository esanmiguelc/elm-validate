module Validations
    exposing
        ( ValidationResult(Valid, Err)
        , beginValidation
        , validatePresenceOf
        , validateLengthOf
        , validateMatchOf
        , equals
        )

{-| This library is inspired by ecto changeset validations


# Result Context

@docs ValidationResult


# Functions:

@docs beginValidation
@docs validatePresenceOf
@docs validateLengthOf
@docs validateMatchOf
@docs equals

-}

import Regex exposing (..)


{-| A Validation result behaves very similarly to a Result, the main difference is that you can have an aggregation of all the errors instead of a single one.
-}
type ValidationResult a
    = Valid a
    | Err a (List String)


validate : (a -> Bool) -> String -> ValidationResult a -> ValidationResult a
validate test errorMessage currentResult =
    case currentResult of
        Valid value ->
            if (test value) then
                Err value [ errorMessage ]
            else
                Valid value

        Err value errors ->
            if (test value) then
                Err value <| List.reverse (errorMessage :: errors)
            else
                Err value errors


{-| Wraps value in a ValidationResult.

    (beginValidation {password = "somepass"}) == Valid {password = "somepass"}

-}
beginValidation : a -> ValidationResult a
beginValidation value =
    Valid value


{-| Validates the presence of a string value.

    (beginValidation {password = "somepass"})
    |> validatePresenceOf .password "Password is too short"

-}
validatePresenceOf : (a -> String) -> String -> ValidationResult a -> ValidationResult a
validatePresenceOf f errorMessage currentResult =
    validate
        (\value -> String.isEmpty (f value))
        errorMessage
        currentResult


{-| Validates the length of a string value.

    (beginValidation {password = "somepass"})
    |> validateLengthOf .password 8 "Password is too short" == Err {password = "somepass} ["Password is too short"]

-}
validateLengthOf : (a -> String) -> Int -> String -> ValidationResult a -> ValidationResult a
validateLengthOf f length errorMessage currentResult =
    validate
        (\value -> String.length (f value) < length)
        errorMessage
        currentResult


{-| Validates equality between two values.

    (beginValidation {password = "somepass"})
    |> equals 8 8 "Values are not the same" == Err {password = "somepass} ["Values are not the same"]

-}
equals : a -> a -> String -> ValidationResult b -> ValidationResult b
equals left right errorMessage currentResult =
    validate (\_ -> not <| left == right) errorMessage currentResult


{-| Validates a regex match of a value.

    (beginValidation {password = "somepass"})
    |> validateMatchOf .password (regex "somepass") "Not in regex" == Err {password = "somepass} ["Not in regex"]

-}
validateMatchOf : (a -> String) -> Regex -> String -> ValidationResult a -> ValidationResult a
validateMatchOf f re errorMessage currentResult =
    validate
        (\value -> not <| contains re (f value))
        errorMessage
        currentResult
