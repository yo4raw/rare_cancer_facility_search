port module Main exposing (CancerPart, CancerType(..), Csv, IntraocularType(..), KeratoconjunctivalType(..), Model, Msg(..), OrbitalType(..), SearchMode(..), getEyelidCsv, getIntraocularCsv, getKeratoconjunctivalCsv, getOrbitalCsv, getSoftTissueCsv, htmlSelectEyelid, htmlSelectIntraocular, htmlSelectKeratoconjunctival, htmlSelectOrbital, init, main, selectOption, unique, update, view)

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



-- JavaScript to Elm


port updateCurrentLocation : (Location -> msg) -> Sub msg



-- Elm to Javascript


port setMapMaker : ToJsLocation -> Cmd msg


main : Program Location Model Msg
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


type alias CancerPart =
    { value : String
    , name : String
    }


type alias ToJsLocation =
    { id : String
    , lat : Maybe Float
    , lng : Maybe Float
    }


type alias Location =
    { lat : Maybe Float
    , lng : Maybe Float
    }


type alias Facility =
    { id : Maybe String
    , name : Maybe String
    , location : Location
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


type alias RetinoblastomaFacilities =
    List GeneralCancerFacility


type alias UvealMalignantMelanomaFacilities =
    List GeneralCancerFacility


type alias IntraocularLymphomaFacilities =
    List GeneralCancerFacility


type alias ConjunctivalMalignantLymphomaFacilities =
    List GeneralCancerFacility


type alias KeratoconjunctivalSquamousCellCarcinomaFacilities =
    List GeneralCancerFacility


type alias ConjunctivalMalignantMelanomaFacilities =
    List GeneralCancerFacility


type alias OrbitalMalignantLymphomaFacilities =
    List GeneralCancerFacility


type alias LacrimalGlandCancerFacilities =
    List GeneralCancerFacility


type alias EyelidFacilities =
    List GeneralCancerFacility


setFacilities : Csv -> Facilities
setFacilities csv =
    csv.records
        |> List.map helperConvListtoFacilityRecord


helperConvListtoFacilityRecord : List String -> Facility
helperConvListtoFacilityRecord list =
    { id = getAt 0 list
    , name = getAt 1 list
    , location =
        { lat = getAt 3 list |> Maybe.withDefault "0" |> String.toFloat
        , lng = getAt 2 list |> Maybe.withDefault "0" |> String.toFloat
        }
    , distance = Nothing
    }



{--
    
parse済みのcsvをresultに挿入する関数

--}


setSoftTissueFacilities : Csv -> Facilities -> SoftTissueFacilities
setSoftTissueFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToSoftTissueFacility facilities)


setRetinoblastomaFacilities : Csv -> Facilities -> RetinoblastomaFacilities
setRetinoblastomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToRetinoblastomaFacility facilities)


setUvealMalignantMelanomaFacilities : Csv -> Facilities -> UvealMalignantMelanomaFacilities
setUvealMalignantMelanomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToUvealMalignantMelanomaFacility facilities)


setIntraocularLymphomaFacilities : Csv -> Facilities -> IntraocularLymphomaFacilities
setIntraocularLymphomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToIntraocularLymphomaFacility facilities)


setConjunctivalMalignantLymphomaFacilities : Csv -> Facilities -> ConjunctivalMalignantLymphomaFacilities
setConjunctivalMalignantLymphomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToConjunctivalMalignantLymphomaFacility facilities)


setKeratoconjunctivalSquamousCellCarcinomaFacilities : Csv -> Facilities -> KeratoconjunctivalSquamousCellCarcinomaFacilities
setKeratoconjunctivalSquamousCellCarcinomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToKeratoconjunctivalSquamousCellCarcinomaFacility facilities)


setConjunctivalMalignantMelanomaFacilities : Csv -> Facilities -> ConjunctivalMalignantMelanomaFacilities
setConjunctivalMalignantMelanomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToConjunctivalMalignantMelanomaFacility facilities)


setOrbitalMalignantLymphomaFacilities : Csv -> Facilities -> OrbitalMalignantLymphomaFacilities
setOrbitalMalignantLymphomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToOrbitalMalignantLymphomaFacility facilities)


setLacrimalGlandCancerFacilities : Csv -> Facilities -> LacrimalGlandCancerFacilities
setLacrimalGlandCancerFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToLacrimalGlandCancerFacility facilities)


setEyelidFacilities : Csv -> Facilities -> EyelidFacilities
setEyelidFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToEyelidFacility facilities)



{--

パースしたcsvのどの項目が該当するかを返却するヘルパー関数群

--}


helperConvListToSoftTissueFacility : Facilities -> List String -> SoftTissueFacility
helperConvListToSoftTissueFacility facilities list =
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
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    , selected = False
    }


helperConvListToRetinoblastomaFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToRetinoblastomaFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 2 list |> maybeStringtoInt
    , diagnosis = getAt 3 list |> Maybe.withDefault "未設定"
    , treatment = getAt 4 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToUvealMalignantMelanomaFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToUvealMalignantMelanomaFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 5 list |> maybeStringtoInt
    , diagnosis = getAt 6 list |> Maybe.withDefault "未設定"
    , treatment = getAt 7 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToIntraocularLymphomaFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToIntraocularLymphomaFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 8 list |> maybeStringtoInt
    , diagnosis = getAt 9 list |> Maybe.withDefault "未設定"
    , treatment = getAt 10 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToConjunctivalMalignantLymphomaFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToConjunctivalMalignantLymphomaFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 2 list |> maybeStringtoInt
    , diagnosis = getAt 3 list |> Maybe.withDefault "未設定"
    , treatment = getAt 4 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToKeratoconjunctivalSquamousCellCarcinomaFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToKeratoconjunctivalSquamousCellCarcinomaFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 5 list |> maybeStringtoInt
    , diagnosis = getAt 6 list |> Maybe.withDefault "未設定"
    , treatment = getAt 7 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToConjunctivalMalignantMelanomaFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToConjunctivalMalignantMelanomaFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 8 list |> maybeStringtoInt
    , diagnosis = getAt 9 list |> Maybe.withDefault "未設定"
    , treatment = getAt 10 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToOrbitalMalignantLymphomaFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToOrbitalMalignantLymphomaFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 2 list |> maybeStringtoInt
    , diagnosis = getAt 3 list |> Maybe.withDefault "未設定"
    , treatment = getAt 4 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToLacrimalGlandCancerFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToLacrimalGlandCancerFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 5 list |> maybeStringtoInt
    , diagnosis = getAt 6 list |> Maybe.withDefault "未設定"
    , treatment = getAt 7 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
    }


helperConvListToEyelidFacility : Facilities -> List String -> GeneralCancerFacility
helperConvListToEyelidFacility facilities list =
    { id = getAt 0 list |> Maybe.withDefault "00000"
    , name = getAt 1 list |> Maybe.withDefault "未設定"
    , count = getAt 2 list |> maybeStringtoInt
    , diagnosis = getAt 3 list |> Maybe.withDefault "未設定"
    , treatment = getAt 4 list |> Maybe.withDefault "未設定"
    , distance = helperGetDistance (getAt 0 list |> Maybe.withDefault "0") facilities |> Maybe.withDefault -1
    , location = helperGetLocation (getAt 0 list |> Maybe.withDefault "0") facilities
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


helperGetLocation : String -> Facilities -> Location
helperGetLocation facilityId facilities =
    facilities
        |> List.map
            (\facility ->
                if (facility.id |> Maybe.withDefault "") == facilityId then
                    { lat = facility.location.lat, lng = facility.location.lng }

                else
                    { lat = Nothing, lng = Nothing }
            )
        |> List.filter
            (\location ->
                (location.lat /= Nothing) && (location.lng /= Nothing)
            )
        |> List.head
        |> Maybe.withDefault { lat = Nothing, lng = Nothing }


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


toRowAttrs : SoftTissueFacility -> List (Attribute Msg)
toRowAttrs facility =
    [ onClick (ToggleSoftTissueSelected facility.id facility.location)
    , style "background"
        (if facility.selected then
            "#CEFAF8"

         else
            "white"
        )
    ]


setFacilitiesDistance : Location -> Facilities -> Facilities
setFacilitiesDistance currentLocation facilities =
    facilities
        |> List.map (\facility -> setFacilityDistance currentLocation facility)


setFacilityDistance : Location -> Facility -> Facility
setFacilityDistance currentLocation facility =
    { facility
        | distance =
            Just
                (Distance.distance
                    (facility.location.lat |> Maybe.withDefault 0)
                    (facility.location.lng |> Maybe.withDefault 0)
                    (currentLocation.lat |> Maybe.withDefault 0)
                    (currentLocation.lng |> Maybe.withDefault 0)
                )
    }


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


type CsvType
    = SoftTissueCSV
    | IntraocularCSV


getCsv : CsvType -> String -> Cmd Msg
getCsv csvType path =
    let
        target_url =
            "http://http://localhost:8000/csv/" ++ path
    in
    case csvType of
        SoftTissueCSV ->
            Http.get
                { url = target_url
                , expect = Http.expectString GotSoftTissueCsv
                }

        _ ->
            Cmd.none


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
    , location : Location
    , facilities : Facilities
    , resultSoftTissueFacilities : SoftTissueFacilities
    , resultUvealMalignantMelanomaFacilities : UvealMalignantMelanomaFacilities
    , resultIntraocularLymphomaFacilities : IntraocularLymphomaFacilities
    , resultConjunctivalMalignantLymphomaFacilities : ConjunctivalMalignantLymphomaFacilities
    , resultKeratoconjunctivalSquamousCellCarcinomaFacilities : KeratoconjunctivalSquamousCellCarcinomaFacilities
    , resultConjunctivalMalignantMelanomaFacilities : ConjunctivalMalignantMelanomaFacilities
    , resultOrbitalMalignantLymphomaFacilities : OrbitalMalignantLymphomaFacilities
    , resultLacrimalGlandCancerFacilities : LacrimalGlandCancerFacilities
    , resultEyelidFacilities : EyelidFacilities
    , searchMode : SearchMode
    , zipcode : String
    , selectedCancerType : String --選択されたがんの種類
    , selectedCancerPart : String --選択されたがんの詳細
    , resultCsv : String
    , parseCsv : Csv
    , onChange : String
    , rawCsv : String
    , currentLocation : Location
    , parseFacilitesCsv : Csv
    , tableState : Table.State
    }


init : Location -> ( Model, Cmd Msg )
init flags =
    ( { input = ""
      , location = flags
      , memos = []
      , facilities = []
      , resultSoftTissueFacilities = []
      , resultUvealMalignantMelanomaFacilities = []
      , resultIntraocularLymphomaFacilities = []
      , resultConjunctivalMalignantLymphomaFacilities = []
      , resultKeratoconjunctivalSquamousCellCarcinomaFacilities = []
      , resultConjunctivalMalignantMelanomaFacilities = []
      , resultOrbitalMalignantLymphomaFacilities = []
      , resultLacrimalGlandCancerFacilities = []
      , resultEyelidFacilities = []
      , searchMode = Geolocation
      , resultCsv = ""
      , zipcode = ""
      , selectedCancerType = ""
      , selectedCancerPart = ""
      , onChange = ""
      , parseCsv = Csv [] []
      , rawCsv = ""
      , currentLocation = Location Nothing Nothing
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
    | UpdateCurrentLocation Location
    | SetTableState Table.State
    | ToggleSoftTissueSelected String Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModeZipcode ->
            ( { model | searchMode = Zipcode }, Cmd.none )

        ModeGeolocation ->
            ( { model | searchMode = Geolocation }, Cmd.none )

        SubmitZipcode string ->
            ( { model | zipcode = string }, Cmd.none )

        -- 癌の種類のドロップダウン変更時
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
            ( { model | resultSoftTissueFacilities = setSoftTissueFacilities (Csv.parse repo) model.facilities }, Cmd.none )

        GotSoftTissueCsv (Err error) ->
            ( { model | resultCsv = Debug.toString error }, Cmd.none )

        --基準点の変更
        UpdateCurrentLocation location ->
            ( { model | currentLocation = location }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        ToggleSoftTissueSelected id location ->
            ( { model
                | resultSoftTissueFacilities =
                    List.map (toggleSoftTissueFacility id) model.resultSoftTissueFacilities
              }
            , setMapMaker { id = id, lat = location.lat, lng = location.lng }
            )

        _ ->
            ( model, Cmd.none )


toggleSoftTissueFacility : String -> SoftTissueFacility -> SoftTissueFacility
toggleSoftTissueFacility id facility =
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
            ]
        , Table.view configSoftTissue model.tableState model.resultSoftTissueFacilities
        ]
