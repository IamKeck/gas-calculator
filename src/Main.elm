port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Set
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
    , selected_entry_id : Set.Set EntryId
    }


type Msg
    = NoOp
    | InputDate String
    | InputTime String
    | InputDistance String
    | InputGas String
    | InputMemo String
    | EntityClicked EntryId
    | UnselectAll
    | Save
    | ReCalc
    | Remove EntryId
    | ClearAllConfirm
    | ClearAll


port saveData : List Entry -> Cmd msg


port clearAllConfirm : () -> Cmd msg


port clearAllMessage : (() -> msg) -> Sub msg


init : String -> ( Model, Cmd Msg )
init entries_json =
    case JD.decodeString (JD.list entryDecoder) entries_json of
        Err _ ->
            ( Model [] "" "" "" "" "" [] Nothing Nothing Set.empty, saveData [] )

        Ok entries ->
            update ReCalc (Model entries "" "" "" "" "" [] Nothing Nothing Set.empty)


main : Program String Model Msg
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

        EntityClicked id ->
            if Set.member id model.selected_entry_id then
                ( { model | selected_entry_id = Set.remove id model.selected_entry_id }, Cmd.none )

            else
                ( { model | selected_entry_id = Set.insert id model.selected_entry_id }, Cmd.none )

        UnselectAll ->
            ( { model | selected_entry_id = Set.empty }, Cmd.none )

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

                        reordered_entries =
                            List.sortBy .date entries |> List.reverse
                    in
                    ( { model
                        | total_distance = Just new_dist
                        , total_gas = Just new_fuel
                        , entries = reordered_entries
                      }
                    , saveData model.entries
                    )

        ClearAllConfirm ->
            ( model, clearAllConfirm () )

        ClearAll ->
            update ReCalc { model | entries = [] }


view : Model -> Html Msg
view model =
    div []
        [ section [ HA.class "section" ]
            [ h1 [ HA.class "title" ]
                [ text <|
                    "平均燃費は"
                        ++ (calcAvgEco model |> Maybe.map formatAvg |> Maybe.withDefault "N/A")
                        ++ "km/l です"
                ]
            , case calcSelectedAvgEco model of
                Nothing ->
                    div [] []

                Just f ->
                    h1 [ HA.class "title" ]
                        [ text <|
                            "選択された記録の平均燃費は"
                                ++ formatAvg f
                                ++ "km/l です"
                        ]
            ]
        , if model.messages == [] then
            div [] []

          else
            section [ HA.class "section" ]
                [ ul [] <| List.map (\msg -> li [] [ text msg ]) model.messages
                ]
        , section [ HA.id "input_form", HA.class "section" ]
            [ div [ HA.class "field" ]
                [ label [ HA.class "label" ] [ text "日付" ] ]
            , div [ HA.class "columns" ]
                [ div [ HA.class "control column" ]
                    [ input [ HA.class "input", HA.type_ "date", HE.onInput InputDate ] [] ]
                , div [ HA.class "control column" ] [ input [ HA.class "input", HA.type_ "time", HE.onInput InputTime ] [] ]
                ]
            , div [ HA.class "field" ]
                [ label [ HA.class "label" ] [ text "距離(km)" ]
                , div [ HA.class "control" ]
                    [ input [ HA.class "input", HA.type_ "number", HE.onInput InputDistance ] []
                    ]
                ]
            , div [ HA.class "field" ]
                [ label [ HA.class "label" ] [ text "給油量(l)" ]
                , div [ HA.class "control" ]
                    [ input [ HA.class "input", HA.type_ "number", HE.onInput InputGas ] []
                    ]
                ]
            , div [ HA.class "field" ]
                [ label [ HA.class "label" ] [ text "備考" ]
                , div [ HA.class "control" ]
                    [ input [ HA.class "input", HE.onInput InputMemo ] []
                    ]
                ]
            , div [ HA.class "field is-grouped is-grouped-centered" ]
                [ div [ HA.class "control" ]
                    [ button [ HA.class "button is-primary", HE.onClick Save ] [ text "保存" ] ]
                , div [ HA.class "control" ]
                    [ button [ HA.class "button is-info", HE.onClick UnselectAll ] [ text "全選択解除" ]
                    ]
                , div [ HA.class "control" ]
                    [ button [ HA.class "button is-danger", HE.onClick ClearAllConfirm ] [ text "データクリア" ]
                    ]
                ]
            ]
        , section [ HA.class "section" ] <|
            List.map
                (\entry ->
                    div
                        [ HA.class "message gas-entry"
                        , HE.onClick <| EntityClicked entry.id
                        , HA.class <|
                            if Set.member entry.id model.selected_entry_id then
                                "is-danger"

                            else
                                "is-info"
                        ]
                        [ div [ HA.class "message-header" ]
                            [ text entry.date
                            , button [ HA.class "delete", HE.onClick <| Remove entry.id ] []
                            ]
                        , div [ HA.class "message-body" ]
                            [ table [ HA.class "table is-fullwidth" ]
                                [ tr []
                                    [ th [] [ text "距離" ]
                                    , td [] [ text <| (String.fromFloat entry.distance ++ "km") ]
                                    ]
                                , tr []
                                    [ th [] [ text "給油量" ]
                                    , td [] [ text <| (String.fromFloat entry.gas ++ "l") ]
                                    ]
                                , tr []
                                    [ th [] [ text "燃費" ]
                                    , td []
                                        [ text <|
                                            (entry.distance / entry.gas |> formatAvg)
                                                ++ "km/l"
                                        ]
                                    ]
                                , tr []
                                    [ th [] [ text "備考" ]
                                    , td [] [ text entry.memo ]
                                    ]
                                ]
                            ]
                        ]
                )
                model.entries
        ]


validate : EntryId -> Model -> Result V.Error Entry
validate id model =
    let
        date =
            V.notEmptyString .form_date [ ( "date", "日付が入力されていません" ) ]

        time =
            V.notEmptyString .form_time [ ( "time", "時刻が入力されていません" ) ]
    in
    V.success Entry
        |> V.andMap (V.map2 (\d t -> d ++ " " ++ t) date time)
        |> V.andMap (V.float .form_distance [ ( "distance", "不正な距離です" ) ])
        |> V.andMap (V.float .form_gas [ ( "gas", "不正な給油量です" ) ])
        |> V.andMap (V.success model.form_memo)
        |> V.andMap (V.success id)
        |> V.run model


nextId : List Entry -> EntryId
nextId entries =
    List.map (\entry -> entry.id) entries |> List.foldl max 0 |> (\id -> id + 1)


calcAvgEco : Model -> Maybe Float
calcAvgEco model =
    Maybe.map2 (\dist gas -> dist / gas) model.total_distance model.total_gas


entryDecoder : JD.Decoder Entry
entryDecoder =
    JD.map5 Entry
        (JD.field "date" JD.string)
        (JD.field "distance" JD.float)
        (JD.field "gas" JD.float)
        (JD.field "memo" JD.string)
        (JD.field "id" JD.int)


formatAvg : Float -> String
formatAvg avg =
    avg * 100 |> floor |> toFloat |> (\i -> i / 100) |> String.fromFloat


calcSelectedAvgEco : Model -> Maybe Float
calcSelectedAvgEco model =
    let
        selected_entries =
            List.filter (\entity -> Set.member entity.id model.selected_entry_id) model.entries
    in
    case selected_entries of
        [] ->
            Nothing

        entries ->
            let
                total_distance =
                    entries |> List.map (\entity -> entity.distance) |> List.sum

                total_gas =
                    entries |> List.map (\entity -> entity.gas) |> List.sum
            in
            Just <| total_distance / total_gas
