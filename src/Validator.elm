module Validator exposing (..)


type alias Error =
    List ( String, String )


type alias Validator model a =
    model -> Result Error a


success : a -> Validator model a
success val _ =
    Ok val


andThen : (a -> Validator model b) -> Validator model a -> Validator model b
andThen f a model =
    case a model of
        Err e ->
            Err e

        Ok v ->
            case model |> f v of
                Err e ->
                    Err e

                Ok v_ ->
                    Ok v_


map : (a -> b) -> Validator model a -> Validator model b
map f v =
    \model -> v model |> Result.map f


map2 : (a -> b -> c) -> Validator model a -> Validator model b -> Validator model c
map2 f a b =
    map f a |> andMap b


run : model -> Validator model a -> Result Error a
run model validator =
    validator model


andMap : Validator model a -> Validator model (a -> b) -> Validator model b
andMap v vf model =
    case v model of
        Err value_err ->
            case vf model of
                Err func_err ->
                    Err <| value_err ++ func_err

                Ok _ ->
                    Err value_err

        Ok value ->
            case vf model of
                Err func_err ->
                    Err func_err

                Ok func ->
                    Ok <| func value


float : (model -> String) -> Error -> Validator model Float
float f e model =
    f model |> String.toFloat |> Result.fromMaybe e


notEmptyString : (model -> String) -> Error -> Validator model String
notEmptyString f e model =
    case f model of
        "" ->
            Err e

        str ->
            Ok str
