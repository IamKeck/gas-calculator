port module Main exposing (..)

import Array
import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Set
import Util exposing (viewIf)
import Validator as V


type alias EntryId =
    Int


encodeEntryId : EntryId -> JE.Value
encodeEntryId =
    JE.int


type alias SaveDataV2 =
    Array.Array Vehicle


type alias SaveDataV1 =
    List Entry


type alias Vehicle =
    { name : String
    , entries : List Entry
    }



-- TODO: dateをStringではなく、専用のオブジェクトにしたい


type alias Entry =
    { date : String
    , distance : Float
    , gas : Float
    , memo : String
    , id : EntryId
    }


type alias Model =
    { form_date : String
    , form_time : String
    , form_distance : String
    , form_gas : String
    , form_memo : String
    , messages : List String
    , total_distance : Maybe Float
    , total_gas : Maybe Float
    , selected_entry_id : Set.Set EntryId
    , saveData : SaveDataV2
    , currentItem : Int
    , changeNameModal : Maybe { name : String }
    , askDeleteVehicleModal : Bool
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
    | AddVehicle
    | Save
    | ReCalc
    | ChangeVehicle Int
    | ChangeVehicleName String
    | InputVehicleName String
    | OpenNameChangeModal
    | CloseNameChangeModal Bool
    | OpenAskDeleteVehicleModal
    | CloseAskDeleteVehicleModal Bool
    | Remove EntryId
    | ClearAllConfirm
    | ClearAll


port saveData : String -> Cmd msg


port clearAllConfirm : () -> Cmd msg


port clearAllMessage : (() -> msg) -> Sub msg


saveDataV1ToV2 : SaveDataV1 -> SaveDataV2
saveDataV1ToV2 entryList =
    [ Vehicle "新規カー" entryList ] |> Array.fromList


saveDataV2ToModel : SaveDataV2 -> Model
saveDataV2ToModel saveDataV2 =
    { form_date = ""
    , form_time = ""
    , form_distance = ""
    , form_gas = ""
    , form_memo = ""
    , messages = []
    , total_distance = Nothing
    , total_gas = Nothing
    , selected_entry_id = Set.empty
    , saveData = saveDataV2
    , currentItem = 0
    , changeNameModal = Nothing
    , askDeleteVehicleModal = False
    }


defaultModel : Model
defaultModel =
    { form_date = ""
    , form_time = ""
    , form_distance = ""
    , form_gas = ""
    , form_memo = ""
    , messages = []
    , total_distance = Nothing
    , total_gas = Nothing
    , selected_entry_id = Set.empty
    , saveData = Array.fromList [ { name = "新規カー", entries = [] } ]
    , currentItem = 0
    , changeNameModal = Nothing
    , askDeleteVehicleModal = False
    }


init : String -> ( Model, Cmd Msg )
init entries_json =
    case JD.decodeString saveDataV1Decoder entries_json of
        Err _ ->
            case JD.decodeString saveDataV2Decoder entries_json of
                Err _ ->
                    update ReCalc defaultModel

                Ok saveDataV2 ->
                    saveDataV2ToModel saveDataV2 |> update ReCalc

        Ok saveDataV1 ->
            saveDataV1ToV2 saveDataV1 |> saveDataV2ToModel |> update ReCalc


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


updateCurrentVehicle : Model -> (Vehicle -> Vehicle) -> Model
updateCurrentVehicle model f =
    case Array.get model.currentItem model.saveData of
        Nothing ->
            model

        Just currentVehicle ->
            f currentVehicle |> (\newVehicle -> Array.set model.currentItem newVehicle model.saveData |> (\newSaveData -> { model | saveData = newSaveData }))


updateCurrentVehicleEntries : Model -> (List Entry -> List Entry) -> Model
updateCurrentVehicleEntries model f =
    updateCurrentVehicle model (\vehicle -> { vehicle | entries = f vehicle.entries })


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

        AddVehicle ->
            let
                newModel =
                    { model | saveData = Array.append model.saveData (Array.fromList [ { name = "新規カー", entries = [] } ]) }
            in
            ( newModel, encodeSaveDataV2ToJSONString newModel.saveData |> saveData )

        UnselectAll ->
            ( { model | selected_entry_id = Set.empty }, Cmd.none )

        Save ->
            case Array.get model.currentItem model.saveData of
                Nothing ->
                    ( model, Cmd.none )

                Just vehicle ->
                    let
                        id =
                            nextId vehicle.entries
                    in
                    case validate id model of
                        Err errors ->
                            ( { model | messages = List.map Tuple.second errors }, Cmd.none )

                        Ok new_entry ->
                            new_entry :: vehicle.entries |> (\newEntries -> { vehicle | entries = newEntries }) |> (\newVehicle -> Array.set model.currentItem newVehicle model.saveData |> (\newVehicles -> { model | saveData = newVehicles }) |> update ReCalc)

        Remove entry_id ->
            let
                removeEntryIdFromVehicle : List Entry -> List Entry
                removeEntryIdFromVehicle =
                    List.filter (\entry -> entry.id /= entry_id)
            in
            updateCurrentVehicleEntries model removeEntryIdFromVehicle |> update ReCalc

        ReCalc ->
            case Array.get model.currentItem model.saveData of
                Nothing ->
                    ( { model | total_distance = Nothing, total_gas = Nothing }, Cmd.none )

                Just vehicle ->
                    case vehicle.entries of
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

                                newSaveData =
                                    updateCurrentVehicleEntries model (always reordered_entries) |> .saveData
                            in
                            ( { model
                                | total_distance = Just new_dist
                                , total_gas = Just new_fuel
                                , saveData = newSaveData
                              }
                            , encodeSaveDataV2ToJSONString newSaveData |> saveData
                            )

        ChangeVehicle index ->
            ( { model | currentItem = index }, Cmd.none )

        ChangeVehicleName name ->
            let
                changeVehicleFunc : Vehicle -> Vehicle
                changeVehicleFunc vehicle =
                    { vehicle | name = name }

                newModel =
                    updateCurrentVehicle model changeVehicleFunc
            in
            ( newModel, encodeSaveDataV2ToJSONString newModel.saveData |> saveData )

        InputVehicleName name ->
            ( { model | changeNameModal = Just { name = name } }, Cmd.none )

        OpenNameChangeModal ->
            let
                name =
                    Array.get model.currentItem model.saveData |> Maybe.map .name |> Maybe.withDefault ""
            in
            ( { model | changeNameModal = Just { name = name } }, Cmd.none )

        CloseNameChangeModal save ->
            { model | changeNameModal = Nothing }
                |> (\newModel ->
                        if save then
                            case model.changeNameModal of
                                Nothing ->
                                    ( newModel, Cmd.none )

                                Just modal ->
                                    update (ChangeVehicleName modal.name) newModel

                        else
                            ( newModel, Cmd.none )
                   )

        OpenAskDeleteVehicleModal ->
            ( { model | askDeleteVehicleModal = True }, Cmd.none )

        CloseAskDeleteVehicleModal okToDelete ->
            if okToDelete then
                ( { model | saveData = removeVehicle model.currentItem model.saveData, currentItem = max 0 <| model.currentItem - 1, askDeleteVehicleModal = False }, Cmd.none )

            else
                ( { model | askDeleteVehicleModal = False }, Cmd.none )

        ClearAllConfirm ->
            ( model, clearAllConfirm () )

        ClearAll ->
            updateCurrentVehicleEntries model (always [])
                |> update ReCalc


removeVehicle : Int -> SaveDataV2 -> SaveDataV2
removeVehicle index data =
    Array.toIndexedList data |> List.filter (\( i, _ ) -> i /= index) |> List.map Tuple.second |> Array.fromList


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
            , div [ HA.class "field is-grouped is-grouped-centered" ]
                [ -- FIXME: ここに車両関係のコントロールを配置することは適切ではないのは承知の上 いつか直す
                  div [ HA.class "control" ]
                    [ button [ HA.class "button is-light", HE.onClick AddVehicle ] [ text "車両追加" ]
                    ]
                , div [ HA.class "control" ]
                    [ button [ HA.class "button is-light", HE.onClick OpenNameChangeModal ] [ text "車両名変更" ]
                    ]
                , viewIf (Array.length model.saveData > 1) <|
                    div [ HA.class "control" ]
                        [ button [ HA.class "button is-danger", HE.onClick OpenAskDeleteVehicleModal ] [ text "現在の車両を削除" ]
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
                (Array.get model.currentItem model.saveData |> Maybe.map .entries |> Maybe.withDefault [])
        , toolbar model.saveData model.currentItem
        , vehicleNameModal model.changeNameModal
        , viewIf model.askDeleteVehicleModal <| askDeleteModal
        ]


toolbar : Array.Array Vehicle -> Int -> Html Msg
toolbar vehicles currentItemIndex =
    let
        names =
            Array.toList vehicles |> List.map .name
    in
    case names of
        [] ->
            div [] []

        nameList ->
            nav [ HA.class "navbar is-link is-fixed-bottom" ]
                [ div [ HA.class "navbar-brand" ] <|
                    List.indexedMap
                        (\index vehicleName ->
                            a [ HA.classList [ ( "navbar-item", True ), ( "is-expanded", True ), ( "has-text-center", True ), ( "is-active", currentItemIndex == index ) ], HE.onClick <| ChangeVehicle index ]
                                [ text vehicleName
                                ]
                        )
                        nameList
                ]


vehicleNameModal : Maybe { name : String } -> Html Msg
vehicleNameModal vehicleName =
    case vehicleName of
        Nothing ->
            text ""

        Just v ->
            div [ HA.class "modal is-active" ]
                [ div [ HA.class "modal-background" ] []
                , div [ HA.class "modal-card" ]
                    [ div [ HA.class "modal-card-body" ]
                        [ div [ HA.class "field" ]
                            [ label [ HA.class "label" ] [ text "車両名" ]
                            , div [ HA.class "control" ]
                                [ input [ HA.class "input", HE.onInput InputMemo, HA.value v.name, HE.onInput InputVehicleName ] []
                                ]
                            ]
                        ]
                    , footer [ HA.class "modal-card-foot" ]
                        [ button [ HA.class "button is-success", HE.onClick <| CloseNameChangeModal True ] [ text "保存" ]
                        , button
                            [ HA.class "button is-light", HE.onClick <| CloseNameChangeModal False ]
                            [ text "キャンセル" ]
                        ]
                    ]
                ]


askDeleteModal : Html Msg
askDeleteModal =
    div [ HA.class "modal is-active" ]
        [ div [ HA.class "modal-background" ] []
        , div [ HA.class "modal-card" ]
            [ div [ HA.class "modal-card-body" ]
                [ div [ HA.class "content" ]
                    [ p [] [ text "現在の車両を削除してよろしいでしょうか" ] ]
                ]
            , footer [ HA.class "modal-card-foot" ]
                [ button [ HA.class "button is-danger", HE.onClick <| CloseAskDeleteVehicleModal True ]
                    [ text "削除" ]
                , button
                    [ HA.class "button is-light", HE.onClick <| CloseAskDeleteVehicleModal False ]
                    [ text "キャンセル" ]
                ]
            ]
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


v2Decoder : JD.Decoder ()
v2Decoder =
    JD.string
        |> JD.andThen
            (\version ->
                if version == "v2" then
                    JD.succeed ()

                else
                    JD.fail "not v2"
            )


saveDataV2Decoder : JD.Decoder SaveDataV2
saveDataV2Decoder =
    let
        saveDataDecoder =
            JD.map2 Vehicle (JD.field "name" JD.string) (JD.field "entries" saveDataV1Decoder)
                |> JD.array

        saveDataAndVersionDecoder =
            JD.map2 (\a b -> ( a, b )) (JD.field "version" v2Decoder) (JD.field "values" saveDataDecoder)
    in
    saveDataAndVersionDecoder |> JD.map (\( _, val ) -> val)


saveDataV1Decoder : JD.Decoder (List Entry)
saveDataV1Decoder =
    JD.list entryDecoder


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
    Array.get model.currentItem model.saveData
        |> Maybe.andThen
            (\currentVehicle ->
                let
                    selected_entries =
                        List.filter (\entity -> Set.member entity.id model.selected_entry_id) currentVehicle.entries
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
            )


encodeSaveDataV2ToJSONString : SaveDataV2 -> String
encodeSaveDataV2ToJSONString data =
    let
        entry : Entry -> JE.Value
        entry ent =
            JE.object
                [ ( "date", JE.string ent.date )
                , ( "distance", JE.float ent.distance )
                , ( "gas", JE.float ent.gas )
                , ( "memo", JE.string ent.memo )
                , ( "id", encodeEntryId ent.id )
                ]

        vehicle : Vehicle -> JE.Value
        vehicle v =
            JE.object [ ( "name", JE.string v.name ), ( "entries", JE.list entry v.entries ) ]
    in
    JE.object
        [ ( "version", JE.string "v2" )
        , ( "values"
          , JE.array vehicle data
          )
        ]
        |> JE.encode 0
