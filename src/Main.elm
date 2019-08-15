port module Main exposing (CancerPart, CancerType(..), Csv, IntraocularType(..), KeratoconjunctivalType(..), Model, Msg(..), OrbitalType(..), SearchMode(..), htmlSelectEyelid, htmlSelectIntraocular, htmlSelectKeratoconjunctival, htmlSelectOrbital, init, main, selectOption, unique, update, view)

import Browser
import Csv exposing (..)
import Distance exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Json
import List.Extra exposing (find, getAt, isPermutationOf)
import MultiSelect exposing (..)
import Table exposing (defaultCustomizations)



-- JavaScript to Elm


port updateCurrentLocation : (Location -> msg) -> Sub msg



-- Elm to Javascript


port setMapMaker : ToJsLocation -> Cmd msg


port openNewWindow : String -> Cmd msg


port changeCurrentLocationFromAddress : String -> Cmd msg


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
    , prefecture : Maybe String
    , region : Maybe String
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
    , prefecture : Maybe String
    , region : Maybe String
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
    , prefecture : Maybe String --都道府県
    , region : Maybe String --地域
    }


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
    , prefecture = getAt 4 list
    , region = getAt 5 list
    }



{--

都道府県によるフィルターをかける関数

--}


filterSoftTissueFacilityPrefecture : List String -> List SoftTissueFacility -> List SoftTissueFacility
filterSoftTissueFacilityPrefecture prefectures facilities =
    if List.isEmpty prefectures then
        facilities

    else
        facilities
            |> List.filter
                (\facility -> List.member (facility.prefecture |> Maybe.withDefault "") prefectures)


filterGeneralCancerFacilityPrefecture : List String -> List GeneralCancerFacility -> List GeneralCancerFacility
filterGeneralCancerFacilityPrefecture prefectures facilities =
    if List.isEmpty prefectures then
        facilities

    else
        facilities
            |> List.filter
                (\facility -> List.member (facility.prefecture |> Maybe.withDefault "") prefectures)



{--

parse済みのcsvをresultに挿入する関数

--}


setSoftTissueFacilities : Csv -> Facilities -> SoftTissueFacilities
setSoftTissueFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToSoftTissueFacility facilities)


setRetinoblastomaFacilities : Csv -> Facilities -> List GeneralCancerFacility
setRetinoblastomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToRetinoblastomaFacility facilities)


setUvealMalignantMelanomaFacilities : Csv -> Facilities -> List GeneralCancerFacility
setUvealMalignantMelanomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToUvealMalignantMelanomaFacility facilities)


setIntraocularLymphomaFacilities : Csv -> Facilities -> List GeneralCancerFacility
setIntraocularLymphomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToIntraocularLymphomaFacility facilities)


setConjunctivalMalignantLymphomaFacilities : Csv -> Facilities -> List GeneralCancerFacility
setConjunctivalMalignantLymphomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToConjunctivalMalignantLymphomaFacility facilities)


setKeratoconjunctivalSquamousCellCarcinomaFacilities : Csv -> Facilities -> List GeneralCancerFacility
setKeratoconjunctivalSquamousCellCarcinomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToKeratoconjunctivalSquamousCellCarcinomaFacility facilities)


setConjunctivalMalignantMelanomaFacilities : Csv -> Facilities -> List GeneralCancerFacility
setConjunctivalMalignantMelanomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToConjunctivalMalignantMelanomaFacility facilities)


setOrbitalMalignantLymphomaFacilities : Csv -> Facilities -> List GeneralCancerFacility
setOrbitalMalignantLymphomaFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToOrbitalMalignantLymphomaFacility facilities)


setLacrimalGlandCancerFacilities : Csv -> Facilities -> List GeneralCancerFacility
setLacrimalGlandCancerFacilities csv facilities =
    csv.records
        |> List.map (helperConvListToLacrimalGlandCancerFacility facilities)


setEyelidFacilities : Csv -> Facilities -> List GeneralCancerFacility
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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
    , prefecture = helperGetPrefecture (getAt 0 list |> Maybe.withDefault "0") facilities
    , region = helperGetRegion (getAt 0 list |> Maybe.withDefault "0") facilities
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


helperGetPrefecture : String -> Facilities -> Maybe String
helperGetPrefecture facilityId facilities =
    facilities
        |> List.map
            (\facility ->
                if (facility.id |> Maybe.withDefault "") == facilityId then
                    facility.prefecture

                else
                    Nothing
            )
        |> List.filter
            (\prefecture ->
                prefecture /= Nothing
            )
        |> List.head
        |> Maybe.withDefault Nothing


helperGetRegion : String -> Facilities -> Maybe String
helperGetRegion facilityId facilities =
    facilities
        |> List.map
            (\facility ->
                if (facility.id |> Maybe.withDefault "") == facilityId then
                    facility.region

                else
                    Nothing
            )
        |> List.filter
            (\region ->
                region /= Nothing
            )
        |> List.head
        |> Maybe.withDefault Nothing


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
            { defaultCustomizations | rowAttrs = toRowAttrsSoftTissue }
        }


configGeneralCancer : Table.Config GeneralCancerFacility Msg
configGeneralCancer =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "施設名" .name
            , Table.intColumn "距離(km)" .distance
            , Table.intColumn "件数" .count
            , Table.stringColumn "診断" .diagnosis
            , Table.stringColumn "治療" .treatment
            ]
        , customizations =
            { defaultCustomizations | rowAttrs = toRowAttrsGeneralCancer }
        }


toRowAttrsSoftTissue : SoftTissueFacility -> List (Attribute Msg)
toRowAttrsSoftTissue facility =
    [ onClick (OpenNewWindow ("https://hospdb.ganjoho.jp/kyotendb.nsf/xpLeaflet.xsp?hospId=" ++ facility.id))
    ]


toRowAttrsGeneralCancer : GeneralCancerFacility -> List (Attribute Msg)
toRowAttrsGeneralCancer facility =
    [ onClick (ToggleGenelalCancerSelected facility.id facility.location)
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
    | RetinoblastomaCSV
    | UvealMalignantMelanomaCSV
    | IntraocularLymphomaCSV
    | ConjunctivalMalignantLymphomaCSV
    | KeratoconjunctivalSquamousCellCarcinomaCSV
    | ConjunctivalMalignantMelanomaCSV
    | OrbitalMalignantLymphomaCSV
    | LacrimalGlandCancerCSV


targetUrl : String
targetUrl =
    "http://localhost:8000/"


getLocationFromZipcode : String -> Cmd Msg
getLocationFromZipcode zipcode =
    Http.get
        { url = "https://geoapi.heartrails.com/api/json?method=searchByPostal&postal=" ++ zipcode
        , expect = Http.expectString GotLocationFromZipcode
        }


getCsv : CsvType -> Cmd Msg
getCsv csvType =
    case csvType of
        SoftTissueCSV ->
            Http.get
                { url = targetUrl ++ "csv/SoftTissue.csv"
                , expect = Http.expectString GotSoftTissueCsv
                }

        RetinoblastomaCSV ->
            Http.get
                { url = targetUrl ++ "csv/Intraocular.csv"
                , expect = Http.expectString GotRetinoblastomaCSV
                }

        UvealMalignantMelanomaCSV ->
            Http.get
                { url = targetUrl ++ "csv/Intraocular.csv"
                , expect = Http.expectString GotUvealMalignantMelanomaCSV
                }

        IntraocularLymphomaCSV ->
            Http.get
                { url = targetUrl ++ "csv/Intraocular.csv"
                , expect = Http.expectString GotIntraocularLymphomaCSV
                }

        ConjunctivalMalignantLymphomaCSV ->
            Http.get
                { url = targetUrl ++ "csv/Keratoconjunctival.csv"
                , expect = Http.expectString GotConjunctivalMalignantLymphomaCSV
                }

        KeratoconjunctivalSquamousCellCarcinomaCSV ->
            Http.get
                { url = targetUrl ++ "csv/Keratoconjunctival.csv"
                , expect = Http.expectString GotKeratoconjunctivalSquamousCellCarcinomaCSV
                }

        ConjunctivalMalignantMelanomaCSV ->
            Http.get
                { url = targetUrl ++ "csv/Keratoconjunctival.csv"
                , expect = Http.expectString GotConjunctivalMalignantMelanomaCSV
                }

        OrbitalMalignantLymphomaCSV ->
            Http.get
                { url = targetUrl ++ "csv/Orbital.csv"
                , expect = Http.expectString GotOrbitalMalignantLymphomaCSV
                }

        LacrimalGlandCancerCSV ->
            Http.get
                { url = targetUrl ++ "csv/Orbital.csv"
                , expect = Http.expectString GotLacrimalGlandCancerCSV
                }


getFacilitiesCsv : Cmd Msg
getFacilitiesCsv =
    Http.get
        { url = targetUrl ++ "csv/Facilities.csv"
        , expect = Http.expectString GotFacilitiesCsv
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


multiSelectTodofukenOptions : MultiSelect.Options Msg
multiSelectTodofukenOptions =
    let
        defaultOptions =
            MultiSelect.defaultOptions ChangeTodofuken
    in
    { defaultOptions
        | items =
            [ { value = "北海道", text = "北海道", enabled = True }
            , { value = "青森県", text = "青森県", enabled = True }
            , { value = "岩手県", text = "岩手県", enabled = True }
            , { value = "宮城県", text = "宮城県", enabled = True }
            , { value = "秋田県", text = "秋田県", enabled = True }
            , { value = "山形県", text = "山形県", enabled = True }
            , { value = "福島県", text = "福島県", enabled = True }
            , { value = "茨城県", text = "茨城県", enabled = True }
            , { value = "栃木県", text = "栃木県", enabled = True }
            , { value = "群馬県", text = "群馬県", enabled = True }
            , { value = "埼玉県", text = "埼玉県", enabled = True }
            , { value = "千葉県", text = "千葉県", enabled = True }
            , { value = "東京都", text = "東京都", enabled = True }
            , { value = "神奈川県", text = "神奈川県", enabled = True }
            , { value = "新潟県", text = "新潟県", enabled = True }
            , { value = "富山県", text = "富山県", enabled = True }
            , { value = "石川県", text = "石川県", enabled = True }
            , { value = "福井県", text = "福井県", enabled = True }
            , { value = "山梨県", text = "山梨県", enabled = True }
            , { value = "長野県", text = "長野県", enabled = True }
            , { value = "岐阜県", text = "岐阜県", enabled = True }
            , { value = "静岡県", text = "静岡県", enabled = True }
            , { value = "愛知県", text = "愛知県", enabled = True }
            , { value = "三重県", text = "三重県", enabled = True }
            , { value = "滋賀県", text = "滋賀県", enabled = True }
            , { value = "京都府", text = "京都府", enabled = True }
            , { value = "大阪府", text = "大阪府", enabled = True }
            , { value = "兵庫県", text = "兵庫県", enabled = True }
            , { value = "奈良県", text = "奈良県", enabled = True }
            , { value = "和歌山県", text = "和歌山県", enabled = True }
            , { value = "鳥取県", text = "鳥取県", enabled = True }
            , { value = "島根県", text = "島根県", enabled = True }
            , { value = "岡山県", text = "岡山県", enabled = True }
            , { value = "広島県", text = "広島県", enabled = True }
            , { value = "山口県", text = "山口県", enabled = True }
            , { value = "徳島県", text = "徳島県", enabled = True }
            , { value = "香川県", text = "香川県", enabled = True }
            , { value = "愛媛県", text = "愛媛県", enabled = True }
            , { value = "高知県", text = "高知県", enabled = True }
            , { value = "福岡県", text = "福岡県", enabled = True }
            , { value = "佐賀県", text = "佐賀県", enabled = True }
            , { value = "長崎県", text = "長崎県", enabled = True }
            , { value = "熊本県", text = "熊本県", enabled = True }
            , { value = "大分県", text = "大分県", enabled = True }
            , { value = "宮崎県", text = "宮崎県", enabled = True }
            , { value = "鹿児島県", text = "鹿児島県", enabled = True }
            , { value = "沖縄県", text = "沖縄県", enabled = True }
            ]
    }


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
    , address : String
    , resultSoftTissueFacilities : SoftTissueFacilities
    , originSoftTissueFacilities : SoftTissueFacilities
    , resultRetinoblastomaFacilities : List GeneralCancerFacility
    , originRetinoblastomaFacilities : List GeneralCancerFacility
    , resultUvealMalignantMelanomaFacilities : List GeneralCancerFacility
    , originUvealMalignantMelanomaFacilities : List GeneralCancerFacility
    , resultIntraocularLymphomaFacilities : List GeneralCancerFacility
    , originIntraocularLymphomaFacilities : List GeneralCancerFacility
    , resultConjunctivalMalignantLymphomaFacilities : List GeneralCancerFacility
    , originConjunctivalMalignantLymphomaFacilities : List GeneralCancerFacility
    , resultKeratoconjunctivalSquamousCellCarcinomaFacilities : List GeneralCancerFacility
    , originKeratoconjunctivalSquamousCellCarcinomaFacilities : List GeneralCancerFacility
    , resultConjunctivalMalignantMelanomaFacilities : List GeneralCancerFacility
    , originConjunctivalMalignantMelanomaFacilities : List GeneralCancerFacility
    , resultOrbitalMalignantLymphomaFacilities : List GeneralCancerFacility
    , originOrbitalMalignantLymphomaFacilities : List GeneralCancerFacility
    , resultLacrimalGlandCancerFacilities : List GeneralCancerFacility
    , originLacrimalGlandCancerFacilities : List GeneralCancerFacility
    , resultEyelidFacilities : List GeneralCancerFacility
    , originEyelidFacilities : List GeneralCancerFacility
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
    , getCsvError : String
    , selectedTodofuken : List String
    , temp : String
    }


init : Location -> ( Model, Cmd Msg )
init flags =
    ( { input = ""
      , location = flags
      , address = ""
      , memos = []
      , facilities = []
      , resultSoftTissueFacilities = []
      , originSoftTissueFacilities = []
      , resultRetinoblastomaFacilities = []
      , originRetinoblastomaFacilities = []
      , resultUvealMalignantMelanomaFacilities = []
      , originUvealMalignantMelanomaFacilities = []
      , resultIntraocularLymphomaFacilities = []
      , originIntraocularLymphomaFacilities = []
      , resultConjunctivalMalignantLymphomaFacilities = []
      , originConjunctivalMalignantLymphomaFacilities = []
      , resultKeratoconjunctivalSquamousCellCarcinomaFacilities = []
      , originKeratoconjunctivalSquamousCellCarcinomaFacilities = []
      , resultConjunctivalMalignantMelanomaFacilities = []
      , originConjunctivalMalignantMelanomaFacilities = []
      , resultOrbitalMalignantLymphomaFacilities = []
      , originOrbitalMalignantLymphomaFacilities = []
      , resultLacrimalGlandCancerFacilities = []
      , originLacrimalGlandCancerFacilities = []
      , resultEyelidFacilities = []
      , originEyelidFacilities = []
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
      , getCsvError = ""
      , selectedTodofuken = []
      , temp = ""
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
    | GotLocationFromZipcode (Result Http.Error String)
    | GotCsv (Result Http.Error String)
    | GotFacilitiesCsv (Result Http.Error String)
    | GotSoftTissueCsv (Result Http.Error String)
    | GotRetinoblastomaCSV (Result Http.Error String)
    | GotUvealMalignantMelanomaCSV (Result Http.Error String)
    | GotIntraocularLymphomaCSV (Result Http.Error String)
    | GotConjunctivalMalignantLymphomaCSV (Result Http.Error String)
    | GotKeratoconjunctivalSquamousCellCarcinomaCSV (Result Http.Error String)
    | GotConjunctivalMalignantMelanomaCSV (Result Http.Error String)
    | GotOrbitalMalignantLymphomaCSV (Result Http.Error String)
    | GotLacrimalGlandCancerCSV (Result Http.Error String)
    | UpdateCurrentLocation Location
    | SetTableState Table.State
    | ToggleSoftTissueSelected String Location
    | ToggleGenelalCancerSelected String Location
    | OpenNewWindow String
    | UpdateZipcode String
    | ChangeTodofuken (List String)


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
                    , getCsv SoftTissueCSV
                    )

                "Intraocular" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , Cmd.none
                    )

                "Keratoconjunctival" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , Cmd.none
                    )

                "Orbital" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , Cmd.none
                    )

                "Eyelid" ->
                    ( { model
                        | selectedCancerType = cancerType
                        , facilities = facilitiesMaster
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- 癌の詳細のドロップダウン変更時
        ChangedCancerPart cancerPart ->
            case cancerPart of
                "Retinoblastoma" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv RetinoblastomaCSV )

                "UvealMalignantMelanoma" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv UvealMalignantMelanomaCSV )

                "IntraocularLymphoma" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv IntraocularLymphomaCSV )

                "ConjunctivalMalignantLymphoma" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv ConjunctivalMalignantLymphomaCSV )

                "KeratoconjunctivalSquamousCellCarcinoma" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv KeratoconjunctivalSquamousCellCarcinomaCSV )

                "ConjunctivalMalignantMelanoma" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv ConjunctivalMalignantMelanomaCSV )

                "OrbitalMalignantLymphoma" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv OrbitalMalignantLymphomaCSV )

                "LacrimalGlandCancer" ->
                    ( { model | selectedCancerPart = cancerPart }, getCsv LacrimalGlandCancerCSV )

                _ ->
                    ( { model | selectedCancerPart = cancerPart }, Cmd.none )

        GotLocationFromZipcode (Ok repo) ->
            ( { model | temp = repo }, Cmd.none )

        GotLocationFromZipcode (Err error) ->
            ( model, Cmd.none )

        GotCsv (Ok repo) ->
            ( { model | resultCsv = repo, parseCsv = Csv.parse repo }, Cmd.none )

        GotCsv (Err error) ->
            ( model, Cmd.none )

        GotSoftTissueCsv (Ok repo) ->
            ( { model
                | resultSoftTissueFacilities = setSoftTissueFacilities (Csv.parse repo) model.facilities
                , originSoftTissueFacilities = setSoftTissueFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotSoftTissueCsv (Err error) ->
            ( model, Cmd.none )

        GotRetinoblastomaCSV (Ok repo) ->
            ( { model
                | resultRetinoblastomaFacilities = setRetinoblastomaFacilities (Csv.parse repo) model.facilities
                , originRetinoblastomaFacilities = setRetinoblastomaFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotRetinoblastomaCSV (Err error) ->
            ( model, Cmd.none )

        GotUvealMalignantMelanomaCSV (Ok repo) ->
            ( { model
                | resultUvealMalignantMelanomaFacilities = setUvealMalignantMelanomaFacilities (Csv.parse repo) model.facilities
                , originUvealMalignantMelanomaFacilities = setUvealMalignantMelanomaFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotUvealMalignantMelanomaCSV (Err error) ->
            ( model, Cmd.none )

        GotIntraocularLymphomaCSV (Ok repo) ->
            ( { model
                | resultIntraocularLymphomaFacilities = setIntraocularLymphomaFacilities (Csv.parse repo) model.facilities
                , originIntraocularLymphomaFacilities = setIntraocularLymphomaFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotIntraocularLymphomaCSV (Err error) ->
            ( model, Cmd.none )

        GotConjunctivalMalignantLymphomaCSV (Ok repo) ->
            ( { model
                | resultConjunctivalMalignantLymphomaFacilities = setConjunctivalMalignantLymphomaFacilities (Csv.parse repo) model.facilities
                , originConjunctivalMalignantLymphomaFacilities = setConjunctivalMalignantLymphomaFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotConjunctivalMalignantLymphomaCSV (Err error) ->
            ( model, Cmd.none )

        GotKeratoconjunctivalSquamousCellCarcinomaCSV (Ok repo) ->
            ( { model
                | resultKeratoconjunctivalSquamousCellCarcinomaFacilities = setKeratoconjunctivalSquamousCellCarcinomaFacilities (Csv.parse repo) model.facilities
                , originKeratoconjunctivalSquamousCellCarcinomaFacilities = setKeratoconjunctivalSquamousCellCarcinomaFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotKeratoconjunctivalSquamousCellCarcinomaCSV (Err error) ->
            ( model, Cmd.none )

        GotConjunctivalMalignantMelanomaCSV (Ok repo) ->
            ( { model
                | resultConjunctivalMalignantMelanomaFacilities = setConjunctivalMalignantMelanomaFacilities (Csv.parse repo) model.facilities
                , originConjunctivalMalignantMelanomaFacilities = setConjunctivalMalignantMelanomaFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotConjunctivalMalignantMelanomaCSV (Err error) ->
            ( model, Cmd.none )

        GotOrbitalMalignantLymphomaCSV (Ok repo) ->
            ( { model
                | resultOrbitalMalignantLymphomaFacilities = setOrbitalMalignantLymphomaFacilities (Csv.parse repo) model.facilities
                , originOrbitalMalignantLymphomaFacilities = setOrbitalMalignantLymphomaFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotOrbitalMalignantLymphomaCSV (Err error) ->
            ( model, Cmd.none )

        GotLacrimalGlandCancerCSV (Ok repo) ->
            ( { model
                | resultLacrimalGlandCancerFacilities = setLacrimalGlandCancerFacilities (Csv.parse repo) model.facilities
                , originLacrimalGlandCancerFacilities = setLacrimalGlandCancerFacilities (Csv.parse repo) model.facilities
              }
            , Cmd.none
            )

        GotLacrimalGlandCancerCSV (Err error) ->
            ( model, Cmd.none )

        GotFacilitiesCsv (Ok repo) ->
            ( { model
                | parseFacilitesCsv = Csv.parse repo
                , facilities = setFacilities (Csv.parse repo)
              }
            , Cmd.none
            )

        GotFacilitiesCsv (Err error) ->
            ( model, Cmd.none )

        --基準点の変更
        UpdateCurrentLocation location ->
            ( { model | currentLocation = location }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        ToggleSoftTissueSelected id location ->
            ( model
            , Cmd.none
            )

        ToggleGenelalCancerSelected id location ->
            ( model
            , Cmd.none
            )

        OpenNewWindow string ->
            ( model
            , openNewWindow string
            )

        UpdateZipcode zipcode ->
            ( { model | zipcode = zipcode }, getLocationFromZipcode zipcode )

        ChangeTodofuken todofuken ->
            ( { model
                | selectedTodofuken = todofuken
                , resultSoftTissueFacilities = model.originSoftTissueFacilities |> filterSoftTissueFacilityPrefecture todofuken
                , resultRetinoblastomaFacilities = model.originRetinoblastomaFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultUvealMalignantMelanomaFacilities = model.originUvealMalignantMelanomaFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultIntraocularLymphomaFacilities = model.originIntraocularLymphomaFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultConjunctivalMalignantLymphomaFacilities = model.originConjunctivalMalignantLymphomaFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultKeratoconjunctivalSquamousCellCarcinomaFacilities = model.originKeratoconjunctivalSquamousCellCarcinomaFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultConjunctivalMalignantMelanomaFacilities = model.originConjunctivalMalignantMelanomaFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultOrbitalMalignantLymphomaFacilities = model.originOrbitalMalignantLymphomaFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultLacrimalGlandCancerFacilities = model.originLacrimalGlandCancerFacilities |> filterGeneralCancerFacilityPrefecture todofuken
                , resultEyelidFacilities = model.originEyelidFacilities |> filterGeneralCancerFacilityPrefecture todofuken
              }
            , Cmd.none
            )


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
                    input [ placeholder "郵便番号を7桁で入力してください", onInput UpdateZipcode ] []

                _ ->
                    text ""
            , text "がん選択:"
            , select [ on "change" (Json.map ChangedCancerType targetValue) ]
                [ option [ selected True ] [ text "選択してください" ]
                , selectOption "SoftTissue" "四肢軟部肉腫"
                , selectOption "Intraocular" "眼内腫瘍"
                , selectOption "Keratoconjunctival" "角結膜腫瘍"
                , selectOption "Orbital" "眼窩腫瘍"
                , selectOption "Eyelid" "眼瞼腫瘍"
                ]
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
                    text ""
        , MultiSelect.multiSelect
            multiSelectTodofukenOptions
            []
            model.selectedTodofuken
        , case model.selectedCancerType of
            "SoftTissue" ->
                Table.view configSoftTissue model.tableState model.resultSoftTissueFacilities

            _ ->
                case model.selectedCancerPart of
                    "Retinoblastoma" ->
                        Table.view configGeneralCancer model.tableState model.resultRetinoblastomaFacilities

                    "UvealMalignantMelanoma" ->
                        Table.view configGeneralCancer model.tableState model.resultUvealMalignantMelanomaFacilities

                    "IntraocularLymphoma" ->
                        Table.view configGeneralCancer model.tableState model.resultIntraocularLymphomaFacilities

                    "ConjunctivalMalignantLymphoma" ->
                        Table.view configGeneralCancer model.tableState model.resultConjunctivalMalignantLymphomaFacilities

                    "KeratoconjunctivalSquamousCellCarcinoma" ->
                        Table.view configGeneralCancer model.tableState model.resultKeratoconjunctivalSquamousCellCarcinomaFacilities

                    "ConjunctivalMalignantMelanoma" ->
                        Table.view configGeneralCancer model.tableState model.resultConjunctivalMalignantMelanomaFacilities

                    "OrbitalMalignantLymphoma" ->
                        Table.view configGeneralCancer model.tableState model.resultOrbitalMalignantLymphomaFacilities

                    "LacrimalGlandCancer" ->
                        Table.view configGeneralCancer model.tableState model.resultLacrimalGlandCancerFacilities

                    _ ->
                        text ""
        ]
