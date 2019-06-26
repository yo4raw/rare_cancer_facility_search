port module Main exposing (CancerPart, CancerType(..), Csv, EyelidType(..), IntraocularType(..), KeratoconjunctivalType(..), Model, Msg(..), OrbitalType(..), SearchMode(..), getEyelidCsv, getIntraocularCsv, getKeratoconjunctivalCsv, getOrbitalCsv, getSoftTissueCsv, htmlSelectEyelid, htmlSelectIntraocular, htmlSelectKeratoconjunctival, htmlSelectOrbital, init, main, selectOption, unique, update, view)

import Browser
import Csv exposing (..)
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Json
import List.Extra exposing (find, getAt)


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


type alias Location =
    { lat : Float
    , lng : Float
    }


type alias CurrentLocation =
    { lat : Maybe Float
    , lng : Maybe Float
    }


type alias Facility =
    { id : Maybe String
    , name : Maybe String
    , lat : Maybe Float
    , lng : Maybe Float
    , distance : Maybe Float
    }


type alias Facilities =
    List Facility


type alias SoftTissueFacility =
    { id : Maybe String
    , name : Maybe String
    , joshi : Maybe Int
    , kashi : Maybe Int
    , taikan : Maybe Int
    , saihatsushoshin : Maybe Int
    , ope : Maybe Int
    , housyasen : Maybe Int
    , yakubutsu : Maybe Int
    , secondopinion : Maybe Int
    , distance : Maybe Float
    }


type alias SoftTissueFacilities =
    List SoftTissueFacility


setFacilities : Csv -> Facilities
setFacilities csv =
    csv.records
        |> List.map helperConvListtoFacilityRecord


helperConvListtoFacilityRecord : List String -> Facility
helperConvListtoFacilityRecord list =
    { id = getAt 0 list
    , name = getAt 1 list
    , lat = getAt 2 list |> Maybe.withDefault "0" |> String.toFloat
    , lng = getAt 3 list |> Maybe.withDefault "0" |> String.toFloat
    , distance = Nothing
    }



--setFacilityDistance : Facilities -> Location -> List a -> List a
--setSoftTissueFacilityDistance facilities location targetFacilites =
--    targetFacilites
--        |> List.map ( \targetFacility ->
--                        if getFacilityLocation facilities targetFacility.name then
--                            {targetFacility | name = "aaa"}
--                        else
--                            {targetFacility | name ="bbb"}
--
--        )
--
--getFacilityLocation : Facilities -> String -> Location
--getFacilityLocation facilities facilityName =
--    facilities
--        |> List.map
--            (\facility ->
--                if isTargetFacility facility facilityName then
--                    { facility.lat, facility.lng }
--                else
--                    Nothing
--            )
--        |> List.head


isTargetFacility : String -> Facility -> Bool
isTargetFacility facilityId facility =
    if (facility.id |> Maybe.withDefault "none") == facilityId then
        True

    else
        False


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
        , expect = Http.expectString GotCsv
        }


getSoftTissueCsv : Cmd Msg
getSoftTissueCsv =
    Http.get
        { url = "http://localhost:8000/csv/SoftTissue.csv"
        , expect = Http.expectString GotCsv
        }


makeSoftTissueTable : List String -> List (List String) -> Html Msg
makeSoftTissueTable headers records =
    table [ class "table" ]
        [ thead [] <|
            toListTableHead headers
        , tbody [] <|
            toListTableRow records
        ]


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
    , searchMode : SearchMode
    , zipcode : String
    , selectedCancerType : String --選択されたがんの種類
    , selectedCancerPart : String --選択されたがんの詳細
    , resultCsv : String
    , parseCsv : Csv
    , onChange : String
    , rawCsv : String
    , currentLocation : CurrentLocation
    }


init : CurrentLocation -> ( Model, Cmd Msg )
init flags =
    ( { input = ""
      , location = flags
      , memos = []
      , facilities = []
      , searchMode = Geolocation
      , resultCsv = ""
      , zipcode = ""
      , selectedCancerType = ""
      , selectedCancerPart = ""
      , onChange = ""
      , parseCsv =
            { headers = []
            , records = []
            }
      , rawCsv = ""
      , currentLocation = CurrentLocation Nothing Nothing
      }
    , initFunction
    )



-- UPDATE


type Msg
    = ModeGeolocation
    | ModeZipcode
    | SubmitZipcode String
    | ChangedCancerType String
    | ChangedCancerPart String
    | Change String
    | GotCsv (Result Http.Error String)
    | UpdateCurrentLocation CurrentLocation


subscriptions : Model -> Sub Msg
subscriptions model =
    updateCurrentLocation UpdateCurrentLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModeZipcode ->
            ( { model | searchMode = Zipcode, facilities = setFacilities model.parseCsv }, Cmd.none )

        ModeGeolocation ->
            ( { model | searchMode = Geolocation, facilities = setFacilities model.parseCsv }, Cmd.none )

        SubmitZipcode string ->
            ( { model | zipcode = string }, Cmd.none )

        ChangedCancerType cancerType ->
            case cancerType of
                "SoftTissue" ->
                    ( { model | selectedCancerType = cancerType }, getSoftTissueCsv )

                "Intraocular" ->
                    ( { model | selectedCancerType = cancerType }, getIntraocularCsv )

                "Keratoconjunctival" ->
                    ( { model | selectedCancerType = cancerType }, getKeratoconjunctivalCsv )

                "Orbital" ->
                    ( { model | selectedCancerType = cancerType }, getOrbitalCsv )

                "Eyelid" ->
                    ( { model | selectedCancerType = cancerType }, getEyelidCsv )

                _ ->
                    ( { model | selectedCancerType = cancerType }, Cmd.none )

        ChangedCancerPart cancerPart ->
            ( { model | selectedCancerPart = cancerPart }, Cmd.none )

        Change value ->
            ( { model | onChange = Debug.log "log label" value }, Cmd.none )

        GotCsv (Ok repo) ->
            ( { model | resultCsv = repo, parseCsv = Csv.parse repo }, Cmd.none )

        GotCsv (Err error) ->
            ( { model | resultCsv = Debug.toString error }, Cmd.none )

        UpdateCurrentLocation location ->
            ( { model | currentLocation = location }, Cmd.none )



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
        , div [ id "resultTable" ]
            [ table [ class "table" ]
                [ case model.selectedCancerType of
                    "SoftTissue" ->
                        makeSoftTissueTable model.parseCsv.headers model.parseCsv.records

                    "Intraocular" ->
                        makeSoftTissueTable model.parseCsv.headers model.parseCsv.records

                    "Keratoconjunctival" ->
                        makeSoftTissueTable model.parseCsv.headers model.parseCsv.records

                    "Orbital" ->
                        makeSoftTissueTable model.parseCsv.headers model.parseCsv.records

                    "Eyelid" ->
                        makeSoftTissueTable model.parseCsv.headers model.parseCsv.records

                    _ ->
                        div [] []
                ]
            ]
        ]
