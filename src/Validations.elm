module Validations
    exposing
        ( ValidationResult(Valid, Err)
        , begin
        , validatePresenceOf
        , validateLengthOf
        , validateMatchOf
        , equals
        )

{-| This library is inspired by ecto changeset validations


# Result Context

@docs ValidationResult


# Functions

@docs begin
@docs validatePresenceOf
@docs validateLengthOf
@docs validateMatchOf
@docs equals

-}

import Regex exposing (..)


{-| A Validation result behaves very similarly to a Result,
the main difference is that on an error state keep sending the value
and you can have an aggregation of all the errors instead of a single one.
-}
type ValidationResult value errorMsg
    = Valid value
    | Err value (List errorMsg)


validate : (value -> Bool) -> errorMsg -> ValidationResult value errorMsg -> ValidationResult value errorMsg
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

    (Validations.begin {password = "somepass"}) == Valid {password = "somepass"}

-}
begin : value -> ValidationResult value errorMsg
begin value =
    Valid value


{-| Validates the presence of a string value.

    (Validations.begin {password = "somepass"})
    |> validatePresenceOf .password "Password is not present"

-}
validatePresenceOf : (value -> String) -> errorMsg -> ValidationResult value errorMsg -> ValidationResult value errorMsg
validatePresenceOf f errorMessage currentResult =
    validate
        (\value -> String.isEmpty (f value))
        errorMessage
        currentResult


{-| Validates the length of a string value.

    (Validations.begin {password = "somepass"})
    |> validateLengthOf .password 8 "Password is too short" == Err {password = "somepass} ["Password is too short"]

-}
validateLengthOf : (value -> String) -> Int -> errorMsg -> ValidationResult value errorMsg -> ValidationResult value errorMsg
validateLengthOf f length errorMessage currentResult =
    validate
        (\value -> String.length (f value) < length)
        errorMessage
        currentResult


{-| Validates equality between two values.

    (Validations.begin {password = "somepass"})
    |> equals 8 8 "Values are not the same" == Err {password = "somepass} ["Values are not the same"]

-}
equals : a -> a -> errorMsg -> ValidationResult value errorMsg -> ValidationResult value errorMsg
equals left right errorMessage currentResult =
    validate (\_ -> not <| left == right) errorMessage currentResult


{-| Validates a regex match of a value.

    (Validations.begin {password = "somepass"})
    |> validateMatchOf .password (regex "somepass") "Not in regex" == Err {password = "somepass} ["Not in regex"]

-}
validateMatchOf : (value -> String) -> Regex -> errorMsg -> ValidationResult value errorMsg -> ValidationResult value errorMsg
validateMatchOf f re errorMessage currentResult =
    validate
        (\value -> not <| contains re (f value))
        errorMessage
        currentResult
