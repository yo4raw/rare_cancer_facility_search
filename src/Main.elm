port module Main exposing (CancerPart, CancerType(..), Csv, EyelidType(..), IntraocularType(..), KeratoconjunctivalType(..), Model, Msg(..), OrbitalType(..), SearchMode(..), getEyelidCsv, getIntraocularCsv, getKeratoconjunctivalCsv, getOrbitalCsv, getSoftTissueCsv, htmlSelectEyelid, htmlSelectIntraocular, htmlSelectKeratoconjunctival, htmlSelectOrbital, init, main, selectOption, unique, update, view)

import Browser
import Csv exposing (..)
import Debug exposing (..)
import Distance exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Json
import List.Extra exposing (find, getAt)
import Table exposing (defaultCustomizations)


port updateCurrentLocation : (CurrentLocation -> msg) -> Sub msg


main : Program CurrentLocation Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



--検索モード


type SearchMode
    = Geolocation
    | Zipcode



--がん部位


type CancerType
    = SoftTissue --四肢軟部肉腫
    | Intraocular --眼内腫瘍
    | Keratoconjunctival --角結膜腫瘍
    | Orbital --眼窩腫瘍
    | Eyelid --眼瞼腫瘍



-- 眼内腫瘍の種類


type IntraocularType
    = Retinoblastoma --網膜芽細胞腫
    | UvealMalignantMelanoma --ぶどう膜悪性黒色腫
    | IntraocularLymphoma --眼内リンパ腫



-- 角結膜腫瘍の種類


type KeratoconjunctivalType
    = ConjunctivalMalignantLymphoma --結膜悪性リンパ腫
    | KeratoconjunctivalSquamousCellCarcinoma --角結膜扁平上皮癌
    | ConjunctivalMalignantMelanoma --結膜悪性黒色腫



-- 眼窩腫瘍の種類


type OrbitalType
    = OrbitalMalignantLymphoma --眼窩悪性リンパ腫
    | LacrimalGlandCancer --涙腺がん



-- 眼瞼腫瘍の種類


type EyelidType
    = BasalCellCarcinoma --基底細胞がん
    | SebaceousGlandCancer --脂腺がん
    | SquamousCellCarcinoma --扁平上皮がん


type alias CancerPart =
    { value : String
    , name : String
    }


type alias CurrentLocation =
    { lat : Maybe Float
    , lng : Maybe Float
    }


type alias Location =
    CurrentLocation


type alias Facility =
    { id : Maybe String
    , name : Maybe String
    , lat : Maybe Float
    , lng : Maybe Float
    , distance : Maybe Int
    }


type alias Facilities =
    List Facility


type alias SoftTissueFacility =
    { id : String
    , name : String
    , joshi : Int
    , kashi : Int
    , taikan : Int
    , saihatsushoshin : Int
    , ope : Int
    , housyasen : Int
    , yakubutsu : Int
    , secondopinion : Int
    , distance : Int
    , location : Location
    , selected : Bool
    }


type alias SoftTissueFacilities =
    List SoftTissueFacility


type alias GeneralCancerFacility =
    { id : String
    , name : String
    , count : Int --件数
    , diagnosis : String --診断
    , treatment : String --治療
    , distance : Int
    , location : Location
    }


type alias GeneralCancerFacilities =
    List GeneralCancerFacility


setFacilities : Csv -> Facilities
setFacilities csv =
    csv.records
        |> List.map helperConvListtoFacilityRecord


helperConvListtoFacilityRecord : List String -> Facility
helperConvListtoFacilityRecord list =
    { id = getAt 0 list
    , name = getAt 1 list
    , lat = getAt 3 list |> Maybe.withDefault "0" |> String.toFloat
    , lng = getAt 2 list |> Maybe.withDefault "0" |> String.toFloat
    , distance = Nothing
    }


setSoftTissueFacilities : Csv -> Location -> Facilities -> SoftTissueFacilities
setSoftTissueFacilities csv location facilities =
    csv.records
        |> List.map (helperConvListToSoftTissueFacilityRecord facilities location)


helperConvListToSoftTissueFacilityRecord : Facilities -> Location -> List String -> SoftTissueFacility
helperConvListToSoftTissueFacilityRecord facilities location list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , joshi = getAt 2 list |> maybeStringtoInt
    , kashi = getAt 3 list |> maybeStringtoInt
    , taikan = getAt 4 list |> maybeStringtoInt
    , saihatsushoshin = getAt 5 list |> maybeStringtoInt
    , ope = getAt 6 list |> maybeStringtoInt
    , housyasen = getAt 7 list |> maybeStringtoInt
    , yakubutsu = getAt 8 list |> maybeStringtoInt
    , secondopinion = getAt 9 list |> maybeStringtoInt
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = location
    , selected = False
    }


maybeStringtoInt : Maybe String -> Int
maybeStringtoInt string =
    string |> Maybe.withDefault "-1" |> String.toInt |> Maybe.withDefault -1


helperGetDistance : String -> Facilities -> Maybe Int
helperGetDistance facilityId facilities =
    facilities
        |> List.map
            (\facility ->
                if (facility.id |> Maybe.withDefault "") == facilityId then
                    facility.distance |> Maybe.withDefault 0

                else
                    0
            )
        |> List.maximum


configSoftTissue : Table.Config SoftTissueFacility Msg
configSoftTissue =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "施設名" .name
            , Table.intColumn "距離(km)" .distance
            , Table.intColumn "上肢" .joshi
            , Table.intColumn "下肢" .kashi
            , Table.intColumn "体幹" .taikan
            , Table.intColumn "再発後初診" .saihatsushoshin
            , Table.intColumn "手術件数" .ope
            , Table.intColumn "放射線治療" .housyasen
            , Table.intColumn "薬物療法" .yakubutsu
            , Table.intColumn "セカンドオピニオン" .secondopinion
            ]
        , customizations =
            { defaultCustomizations | rowAttrs = toRowAttrs }
        }


toRowAttrs : SoftTissueFacility -> List (Attribute Msg)
toRowAttrs facility =
    [ onClick (ToggleSoftTissueSelected facility.id)
    , style "background"
        (if facility.selected then
            "#CEFAF8"

         else
            "white"
        )
    ]


configGeneralCancer : Table.Config GeneralCancerFacility Msg
configGeneralCancer =
    Table.config
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "施設名" .name
            , Table.intColumn "距離(km)" .distance
            , Table.intColumn "件数" .count
            , Table.stringColumn "診断" .diagnosis
            , Table.stringColumn "治療" .treatment
            ]
        }


setFacilitiesDistance : CurrentLocation -> Facilities -> Facilities
setFacilitiesDistance currentLocation facilities =
    facilities
        |> List.map (\facility -> setFacilityDistance currentLocation facility)


setFacilityDistance : CurrentLocation -> Facility -> Facility
setFacilityDistance currentLocation facility =
    { facility | distance = Just (Distance.distance (facility.lat |> Maybe.withDefault 0) (facility.lng |> Maybe.withDefault 0) (currentLocation.lat |> Maybe.withDefault 0) (currentLocation.lng |> Maybe.withDefault 0)) }


toListTableHead : List String -> List (Html Msg)
toListTableHead myListItems =
    myListItems
        |> List.map (\th_ -> th [] [ text th_ ])


toListTableRow : List (List String) -> List (Html Msg)
toListTableRow myListItems =
    myListItems
        |> List.map
            (\tr_ ->
                tr_
                    |> List.map (\td_ -> td [] [ text td_ ])
                    |> tr []
            )


initFunction : Cmd Msg
initFunction =
    Cmd.batch
        [ getFacilitiesCsv
        ]


getFacilitiesCsv : Cmd Msg
getFacilitiesCsv =
    Http.get
        { url = "http://localhost:8000/csv/Facilities.csv"
        , expect = Http.expectString GotFacilitiesCsv
        }


getSoftTissueCsv : Cmd Msg
getSoftTissueCsv =
    Http.get
        { url = "http://localhost:8000/csv/SoftTissue.csv"
        , expect = Http.expectString GotSoftTissueCsv
        }


getIntraocularCsv : Cmd Msg
getIntraocularCsv =
    Http.get
        { url = "http://localhost:8000/csv/Intraocular.csv"
        , expect = Http.expectString GotCsv
        }


getKeratoconjunctivalCsv : Cmd Msg
getKeratoconjunctivalCsv =
    Http.get
        { url = "http://localhost:8000/csv/Keratoconjunctival.csv"
        , expect = Http.expectString GotCsv
        }


getOrbitalCsv : Cmd Msg
getOrbitalCsv =
    Http.get
        { url = "http://localhost:8000/csv/Orbital.csv"
        , expect = Http.expectString GotCsv
        }


getEyelidCsv : Cmd Msg
getEyelidCsv =
    Http.get
        { url = "http://localhost:8000/csv/Eyelid.csv"
        , expect = Http.expectString GotCsv
        }



-- 距離が入力されている四肢軟部肉腫の結果をTableに出力する


selectOption : String -> String -> Html Msg
selectOption inputValue inputText =
    option [ value inputValue ] [ text inputText ]



-- 眼内腫瘍選択時のセレクトボックス生成


htmlSelectIntraocular : Html Msg
htmlSelectIntraocular =
    select [ id "cancerpart", on "change" (Json.map ChangedCancerPart targetValue) ]
        [ option [ selected True ] [ text "選択してください" ]
        , selectOption "Retinoblastoma" "網膜芽細胞腫"
        , selectOption "UvealMalignantMelanoma" "ぶどう膜悪性黒色腫"
        , selectOption "IntraocularLymphoma" "眼内リンパ腫"
        ]



-- 角結膜腫瘍選択時のセレクトボックス生成


htmlSelectKeratoconjunctival : Html Msg
htmlSelectKeratoconjunctival =
    select [ id "cancerpart", on "change" (Json.map ChangedCancerPart targetValue) ]
        [ option [ selected True ] [ text "選択してください" ]
        , selectOption "ConjunctivalMalignantLymphoma" "結膜悪性リンパ腫"
        , selectOption "KeratoconjunctivalSquamousCellCarcinoma" "角結膜扁平上皮癌"
        , selectOption "ConjunctivalMalignantMelanoma" "結膜悪性黒色腫"
        ]



-- 眼窩腫瘍選択時のセレクトボックス生成


htmlSelectOrbital : Html Msg
htmlSelectOrbital =
    select [ id "cancerpart", on "change" (Json.map ChangedCancerPart targetValue) ]
        [ option [ selected True ] [ text "選択してください" ]
        , selectOption "OrbitalMalignantLymphoma" "眼窩悪性リンパ腫"
        , selectOption "LacrimalGlandCancer" "涙腺がん"
        ]



-- 眼瞼腫瘍選択時のセレクトボックス生成


htmlSelectEyelid : Html Msg
htmlSelectEyelid =
    select [ id "cancerpart", on "change" (Json.map ChangedCancerPart targetValue) ]
        [ option [ selected True ] [ text "選択してください" ]
        , selectOption "BasalCellCarcinoma" "基底細胞がん"
        , selectOption "SebaceousGlandCancer" "脂腺がん"
        , selectOption "SquamousCellCarcinoma" "扁平上皮がん"
        ]


unique : String -> Html msg -> Html msg
unique identifier html =
    Keyed.node "span" [] [ ( identifier, html ) ]


type alias Csv =
    { headers : List String
    , records : List (List String)
    }



-- MODEL


type alias Model =
    { input : String
    , memos : List String
    , location : CurrentLocation
    , facilities : Facilities
    , resultSoftTissueFacilities : SoftTissueFacilities
    , searchMode : SearchMode
    , zipcode : String
    , selectedCancerType : String --選択されたがんの種類
    , selectedCancerPart : String --選択されたがんの詳細
    , resultCsv : String
    , parseCsv : Csv
    , onChange : String
    , rawCsv : String
    , currentLocation : CurrentLocation
    , parseFacilitesCsv : Csv
    , tableState : Table.State
    }


init : CurrentLocation -> ( Model, Cmd Msg )
init flags =
    ( { input = ""
      , location = flags
      , memos = []
      , facilities = []
      , resultSoftTissueFacilities = []
      , searchMode = Geolocation
      , resultCsv = ""
      , zipcode = ""
      , selectedCancerType = ""
      , selectedCancerPart = ""
      , onChange = ""
      , parseCsv = Csv [] []
      , rawCsv = ""
      , currentLocation = CurrentLocation Nothing Nothing
      , parseFacilitesCsv = Csv [] []
      , tableState = Table.initialSort "id"
      }
    , initFunction
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    updateCurrentLocation UpdateCurrentLocation



-- UPDATE


type Msg
    = ModeGeolocation
    | ModeZipcode
    | SubmitZipcode String
    | ChangedCancerType String
    | ChangedCancerPart String
    | Change String
    | GotCsv (Result Http.Error String)
    | GotSoftTissueCsv (Result Http.Error String)
    | GotFacilitiesCsv (Result Http.Error String)
    | UpdateCurrentLocation CurrentLocation
    | SetTableState Table.State
    | ToggleSoftTissueSelected String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModeZipcode ->
            ( { model | searchMode = Zipcode }, Cmd.none )

        ModeGeolocation ->
            ( { model | searchMode = Geolocation }, Cmd.none )

        SubmitZipcode string ->
            ( { model | zipcode = string }, Cmd.none )

        ChangedCancerType cancerType ->
            let
                facilitiesMaster =
                    setFacilitiesDistance model.currentLocation model.facilities
            in
            case cancerType of
                "SoftTissue" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , getSoftTissueCsv
                    )

                "Intraocular" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , getIntraocularCsv
                    )

                "Keratoconjunctival" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , getKeratoconjunctivalCsv
                    )

                "Orbital" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , getOrbitalCsv
                    )

                "Eyelid" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , getEyelidCsv
                    )

                _ ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , Cmd.none
                    )

        ChangedCancerPart cancerPart ->
            ( { model | selectedCancerPart = cancerPart }, Cmd.none )

        Change value ->
            ( { model | onChange = Debug.log "log label" value }, Cmd.none )

        GotCsv (Ok repo) ->
            ( { model | resultCsv = repo, parseCsv = Csv.parse repo }, Cmd.none )

        GotCsv (Err error) ->
            ( { model | resultCsv = Debug.toString error }, Cmd.none )

        GotSoftTissueCsv (Ok repo) ->
            ( { model | resultSoftTissueFacilities = setSoftTissueFacilities (Csv.parse repo) model.currentLocation model.facilities }, Cmd.none )

        GotSoftTissueCsv (Err error) ->
            ( { model | resultCsv = Debug.toString error }, Cmd.none )

        GotFacilitiesCsv (Ok repo) ->
            ( { model | parseFacilitesCsv = Csv.parse repo, facilities = setFacilities (Csv.parse repo) }, Cmd.none )

        GotFacilitiesCsv (Err error) ->
            ( { model | resultCsv = Debug.toString error }, Cmd.none )

        UpdateCurrentLocation location ->
            ( { model | currentLocation = location }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        ToggleSoftTissueSelected id ->
            ( { model | resultSoftTissueFacilities = List.map (toggleSoftTissue id) model.resultSoftTissueFacilities }
            , Cmd.none
            )


toggleSoftTissue : String -> SoftTissueFacility -> SoftTissueFacility
toggleSoftTissue id facility =
    if facility.id == id then
        { facility | selected = not facility.selected }

    else
        facility



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ section [ class "section" ]
            [ div [ class "tabs" ]
                [ ul []
                    [ li
                        [ if model.searchMode == Geolocation then
                            class "is-active"

                          else
                            class "none"
                        ]
                        [ a [ onClick ModeGeolocation ]
                            [ text "現在地から探す" ]
                        ]
                    , li
                        [ if model.searchMode == Zipcode then
                            class "is-active"

                          else
                            class "none"
                        ]
                        [ a [ onClick ModeZipcode ]
                            [ text "郵便番号から探す" ]
                        ]
                    ]
                ]
            ]
        , div [ id "inputField" ]
            [ case model.searchMode of
                Zipcode ->
                    input [ placeholder "郵便番号を7桁で入力してください", onInput SubmitZipcode ] []

                _ ->
                    div [] []
            , select [ on "change" (Json.map ChangedCancerType targetValue) ]
                [ option [ selected True ] [ text "選択してください" ]
                , selectOption "SoftTissue" "四肢軟部肉腫"
                , selectOption "Intraocular" "眼内腫瘍"
                , selectOption "Keratoconjunctival" "角結膜腫瘍"
                , selectOption "Orbital" "眼窩腫瘍"
                , selectOption "Eyelid" "眼瞼腫瘍"
                ]
            , unique model.selectedCancerType <|
                case model.selectedCancerType of
                    "Intraocular" ->
                        htmlSelectIntraocular

                    "Keratoconjunctival" ->
                        htmlSelectKeratoconjunctival

                    "Orbital" ->
                        htmlSelectOrbital

                    "Eyelid" ->
                        htmlSelectEyelid

                    _ ->
                        div [] []
            ]
        , Table.view configSoftTissue model.tableState model.resultSoftTissueFacilities
        ]
