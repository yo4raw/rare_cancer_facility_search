module Main exposing (CancerPart, CancerType(..), Csv, EyelidType(..), IntraocularType(..), KeratoconjunctivalType(..), Model, Msg(..), OrbitalType(..), SearchMode(..), htmlSelectEyelid, htmlSelectIntraocular, htmlSelectKeratoconjunctival, htmlSelectOrbital, init, main, selectOption, unique, update, urlDownload, view)

import Browser
import Csv exposing (..)
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Json


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
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



-- SerchModeでcurrentLocation選択時の病院情報
-- SerchModeでzipCode選択時の病院情報


urlDownload : Cmd Msg
urlDownload =
    Http.get
        { url = "http://localhost:8000/csv/eye_cancer_facilities.csv"
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
    , facilities : List String
    , searchMode : SearchMode
    , zipcode : String
    , selectedCancerType : String --選択されたがんの種類
    , selectedCancerPart : String --選択されたがんの詳細
    , resultCsv : String
    , parseCsv : Csv
    , onChange : String
    , rawCsv : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        _ =
            Debug.log "model" "aaa"
    in
    ( { input = ""
      , memos = []
      , facilities = []
      , searchMode = Zipcode
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
      }
    , Cmd.none
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
            case cancerType of
                "SoftTissue" ->
                    ( { model | selectedCancerType = cancerType }, urlDownload )

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



-- VIEW


view : Model -> Html Msg
view model =
    div []
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
            , select [ on "change" (Json.map ChangedCancerType targetValue) ]
                [ selectOption "SoftTissue" "四肢軟部肉腫"
                , selectOption "Intraocular" "眼内腫瘍"
                , selectOption "Keratoconjunctival" "角結膜腫瘍"
                , selectOption "Orbital" "眼窩腫瘍"
                , selectOption "Eyelid" "眼瞼腫瘍"
                ]
            ]
        ]
