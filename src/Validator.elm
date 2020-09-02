module Validator exposing (..)


type alias Error =
    List ( String, String )


type alias Validator model a =
    model -> Result Error a


success : a -> Validator model a
success val _ =
    Ok val


map : (a -> b) -> Validator model a -> Validator model b
map f v =
    \model -> v model |> Result.map f


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
