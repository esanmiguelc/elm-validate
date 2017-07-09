module ValidationsTests exposing (tests)

import Expect exposing (Expectation)
import Test exposing (..)
import Validations as Val
    exposing
        ( ValidationResult(Valid, Err)
        , validatePresenceOf
        , validateLengthOf
        , equals
        )
import Regex exposing (..)


type alias Credentials =
    { email : String, password : String }


emailError =
    "No email present"


passwordError =
    "No password present"


validateEmailPresence credentials =
    (Val.validatePresenceOf .email emailError (Val.begin credentials))


validatePasswordPresence credentials =
    (Val.validatePresenceOf .password passwordError (Val.begin credentials))


validatePasswordLength credentials =
    (Val.validateLengthOf .password 8 "Password too short" (Val.begin credentials))


tests : Test
tests =
    describe "Validations"
        [ test "returns error when there is no email available" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "somepassword")
                in
                    Expect.equal (Val.Err credentials [ emailError ])
                        (validateEmailPresence credentials)
        , test "returns valid when email field is not empty" <|
            \() ->
                let
                    credentials =
                        Credentials "someEmail" ""
                in
                    Expect.equal (Val.Valid credentials) <|
                        (validateEmailPresence credentials)
        , test "returns Err when there is no password" <|
            \() ->
                let
                    credentials =
                        (Credentials "someEmail" "")
                in
                    Expect.equal (Val.Err credentials [ passwordError ])
                        (validatePasswordPresence credentials)
        , test "returns valid when there is a password" <|
            \() ->
                let
                    credentials =
                        (Credentials "someEmail" "somepassword")
                in
                    Expect.equal (Val.Valid credentials)
                        (validatePasswordPresence credentials)
        , test "aggregates errors when multiple" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "")
                in
                    Expect.equal (Val.Err credentials [ emailError, passwordError ])
                        ((Val.begin credentials)
                            |> (Val.validatePresenceOf .email emailError)
                            |> (Val.validatePresenceOf .password passwordError)
                        )
        , test "does not become valid if already an error" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "password")
                in
                    ((Val.begin credentials)
                        |> (Val.validatePresenceOf .email emailError)
                        |> (Val.validatePresenceOf .password passwordError)
                    )
                        |> Expect.equal (Val.Err credentials [ emailError ])
        , test "validates password length" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "")
                in
                    Expect.equal (Val.Err credentials [ "Password too short" ])
                        (validatePasswordLength credentials)
        , test "is valid if it has the same length" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "password")
                in
                    Expect.equal (Val.Valid credentials)
                        (validatePasswordLength credentials)
        , test "is valid if it is equal integers" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "password")
                in
                    Expect.equal (Val.Valid credentials)
                        (Val.equals 8 (String.length credentials.password) "Nope" (Val.begin credentials))
        , test "is not valid if it is different integers" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "password")
                in
                    Expect.equal (Val.Err credentials [ "Nope" ])
                        (Val.equals 9 (String.length credentials.password) "Nope" (Val.begin credentials))
        , test "is valid if it is equal strings" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "password")
                in
                    Expect.equal (Val.Valid credentials)
                        (Val.equals "password" credentials.password "Nope" (Val.begin credentials))
        , test "is not valid if it is different strings" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "password")
                in
                    Expect.equal (Val.Err credentials [ "Nope" ])
                        (Val.equals "passwor" credentials.password "Nope" (Val.begin credentials))
        , test "is valid if it matches format" <|
            \() ->
                let
                    credentials =
                        (Credentials "" "password")
                in
                    Expect.equal (Val.Valid credentials)
                        (Val.validateMatchOf .password (regex "password") "Nope" (Val.begin credentials))
        ]
