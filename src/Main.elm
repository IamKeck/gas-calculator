port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Validator as V


type alias EntryId =
    Int



-- TODO: dateをStringではなく、専用のオブジェクトにしたい


type alias Entry =
    { date : String
    , distance : Float
    , gas : Float
    , memo : String
    , id : EntryId
    }


type alias Model =
    { entries : List Entry
    , form_date : String
    , form_time : String
    , form_distance : String
    , form_gas : String
    , form_memo : String
    , messages : List String
    , total_distance : Maybe Float
    , total_gas : Maybe Float
    }


type Msg
    = NoOp
    | InputDate String
    | InputTime String
    | InputDistance String
    | InputGas String
    | InputMemo String
    | Save
    | ReCalc
    | Remove EntryId
    | ClearAllConfirm
    | ClearAll


port saveData : List Entry -> Cmd msg


port clearAllConfirm : () -> Cmd msg


port clearAllMessage : (() -> msg) -> Sub msg


init : List Entry -> ( Model, Cmd Msg )
init entries =
    update ReCalc (Model entries "" "" "" "" "" [] Nothing Nothing)


main : Program (List Entry) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    clearAllMessage <| always ClearAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputDate d ->
            ( { model | form_date = d }, Cmd.none )

        InputTime t ->
            ( { model | form_time = t }, Cmd.none )

        InputDistance d ->
            ( { model | form_distance = d }, Cmd.none )

        InputGas g ->
            ( { model | form_gas = g }, Cmd.none )

        InputMemo m ->
            ( { model | form_memo = m }, Cmd.none )

        Save ->
            let
                id =
                    nextId model.entries
            in
            case validate id model of
                Err errors ->
                    ( { model | messages = List.map Tuple.second errors }, Cmd.none )

                Ok new_entry ->
                    let
                        new_model =
                            { model | entries = new_entry :: model.entries, messages = [ "給油記録を追加しました" ] }
                    in
                    update ReCalc new_model

        Remove entry_id ->
            let
                new_model =
                    { model
                        | entries = List.filter (\entry -> entry.id /= entry_id) model.entries
                    }
            in
            update ReCalc new_model

        ReCalc ->
            case model.entries of
                [] ->
                    ( { model | total_distance = Nothing, total_gas = Nothing }, Cmd.none )

                entries ->
                    let
                        new_dist =
                            List.sum <| List.map (\entry -> entry.distance) entries

                        new_fuel =
                            List.sum <| List.map (\entry -> entry.gas) entries
                    in
                    ( { model | total_distance = Just new_dist, total_gas = Just new_fuel }
                    , saveData model.entries
                    )

        ClearAllConfirm ->
            ( model, clearAllConfirm () )

        ClearAll ->
            update ReCalc { model | entries = [] }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text <| "平均燃費は" ++ calcAvgEco model ++ "km/l です" ]
        , ul [] <| List.map (\msg -> li [] [ text msg ]) model.messages
        , div [ HA.id "input_form" ]
            [ div []
                [ label [] [ text "日付" ]
                , div []
                    [ input [ HA.type_ "date", HE.onInput InputDate ] []
                    , input [ HA.type_ "time", HE.onInput InputTime ] []
                    ]
                ]
            , div []
                [ label [] [ text "距離(km)" ]
                , div []
                    [ input [ HE.onInput InputDistance ] []
                    ]
                ]
            , div []
                [ label [] [ text "給油量(l)" ]
                , div []
                    [ input [ HE.onInput InputGas ] []
                    ]
                ]
            , div []
                [ label [] [ text "備考" ]
                , div []
                    [ input [ HE.onInput InputMemo ] []
                    ]
                ]
            ]
        , div []
            [ button [ HE.onClick Save ] [ text "保存" ]
            , button [ HE.onClick ClearAllConfirm ] [ text "データクリア" ]
            ]
        , div []
            [ table []
                [ thead []
                    [ tr []
                        [ td [] [ text "日付" ]
                        , td [] [ text "距離" ]
                        , td [] [ text "給油量" ]
                        , td [] [ text "燃費" ]
                        , td [] [ text "備考" ]
                        , td [] [ text "削除" ]
                        ]
                    ]
                , tbody [] <|
                    List.map
                        (\entry ->
                            tr []
                                [ td [] [ text entry.date ]
                                , td [] [ text <| (String.fromFloat entry.distance ++ "km") ]
                                , td [] [ text <| (String.fromFloat entry.gas ++ "l") ]
                                , td [] [ text <| (String.fromFloat <| entry.distance / entry.gas) ++ "km/l" ]
                                , td [] [ text entry.memo ]
                                , td [] [ button [ HE.onClick <| Remove entry.id ] [ text "削除" ] ]
                                ]
                        )
                        model.entries
                ]
            ]
        ]


validate : EntryId -> Model -> Result V.Error Entry
validate id model =
    let
        distValidator : V.Validator Model Float
        distValidator model_ =
            case String.toFloat model_.form_distance of
                Nothing ->
                    Err [ ( "distance", "不正な距離です" ) ]

                Just dist ->
                    Ok dist

        gasValidator : V.Validator Model Float
        gasValidator model_ =
            case String.toFloat model_.form_gas of
                Nothing ->
                    Err [ ( "gas", "不正な給油量です" ) ]

                Just gas_ ->
                    Ok gas_
    in
    V.success Entry
        |> V.andMap (V.success <| model.form_date ++ model.form_time)
        |> V.andMap distValidator
        |> V.andMap gasValidator
        |> V.andMap (V.success model.form_memo)
        |> V.andMap (V.success id)
        |> V.run model


nextId : List Entry -> EntryId
nextId entries =
    List.map (\entry -> entry.id) entries |> List.foldl max 0 |> (\id -> id + 1)


calcAvgEco : Model -> String
calcAvgEco model =
    let
        avg =
            Maybe.map2 (\dist gas -> dist / gas) model.total_distance model.total_gas
    in
    case avg of
        Nothing ->
            "N/A"

        Just avg_ ->
            String.fromFloat avg_
