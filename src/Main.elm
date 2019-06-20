import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

main : Program () Model Msg
main =
 Browser.element
     { init = init
     , update = update
     , view = view
     , subscriptions = \_ -> Sub.none
     }

--検索モード
type SearchMode = Geolocation
                | Zipcode

--がん部位
type CancerType = SoftTissue      --四肢軟部肉腫
             | Intraocular        --眼内腫瘍
             | Keratoconjunctival --角結膜腫瘍
             | Orbital            --眼窩腫瘍
             | Eyelid             --眼瞼腫瘍

-- 眼内腫瘍の種類
type IntraocularType = Retinoblastoma      --網膜芽細胞腫
                  | UvealMalignantMelanoma --ぶどう膜悪性黒色腫
                  | IntraocularLymphoma    --眼内リンパ腫

-- 角結膜腫瘍の種類
type KeratoconjunctivalType = ConjunctivalMalignantLymphoma        --結膜悪性リンパ腫
                         | KeratoconjunctivalSquamousCellCarcinoma --角結膜扁平上皮癌
                         | ConjunctivalMalignantMelanoma           --結膜悪性黒色腫

-- 眼窩腫瘍の種類
type OrbitalType = OrbitalMalignantLymphoma --眼窩悪性リンパ腫
              | LacrimalGlandCancer         --涙腺がん

-- 眼瞼腫瘍の種類
type EyelidType = BasalCellCarcinoma        --基底細胞がん
             | SebaceousGlandCancer         --脂腺がん
             | SquamousCellCarcinoma        --扁平上皮がん

type alias CancerPart =
    { value: String
    , name: String
    }


-- SerchModeでcurrentLocation選択時の病院情報

-- SerchModeでzipCode選択時の病院情報



selectOption : String -> String -> Html Msg
selectOption inputValue inputText =
    option [value inputValue] [text inputText]



-- MODEL
type alias Model =
     {input : String
     , memos : List String
     , facilities : List String
     , searchMode : SearchMode
     , zipcode : String
     , selectedCancerType : String --選択されたがんの種類
     , cancerParts :List CancerPart --選択できるがんの詳細
     , selectedCancerPart : String --選択されたがんの詳細
     , result : String
     }

init : () -> ( Model, Cmd Msg)
init _ =
    (     {input = ""
          , memos = []
          , facilities = []
          , searchMode = Zipcode
          , result = "none"
          , zipcode = ""
          , selectedCancerType = ""
          , cancerParts = []
          , selectedCancerPart = ""
          }, Cmd.none)


-- UPDATE
type Msg = ModeGeolocation
          | ModeZipcode
          | SubmitZipcode String
          | ChengedCancerType String
          | ChangedCancerPart String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModeZipcode ->
            ({model | searchMode = Zipcode}, Cmd.none)
        ModeGeolocation ->
            ({model | searchMode = Geolocation}, Cmd.none)
        SubmitZipcode string ->
            ({model | zipcode = string}, Cmd.none)
        ChengedCancerType cancerType ->
            ({model | selectedCancerType = cancerType}, Cmd.none)
        ChangedCancerPart cancerPart ->
            case cancerPart of
                "SoftTissue" ->
                    ({model | cancerParts = []}, Cmd.none)
                "Intraocular" ->
                    ({model | cancerParts = [ CancerPart "Retinoblastoma" "網膜芽細胞腫"
                                            , CancerPart "UvealMalignantMelanoma" "ぶどう膜悪性黒色腫"
                                            , CancerPart "IntraocularLymphoma" "眼内リンパ腫"
                                            ]}, Cmd.none)
                "Keratoconjunctival" ->
                    ({model | cancerParts = [ CancerPart "ConjunctivalMalignantLymphoma" "結膜悪性リンパ腫"
                                            , CancerPart "KeratoconjunctivalSquamousCellCarcinoma" "角結膜扁平上皮癌"
                                            , CancerPart "ConjunctivalMalignantMelanoma" "結膜悪性黒色腫"
                                            ]}, Cmd.none)
                "Orbital" ->
                    ({model | cancerParts = [ CancerPart "OrbitalMalignantLymphoma" "眼窩悪性リンパ腫"
                                            , CancerPart "LacrimalGlandCancer" "涙腺がん"
                                            ]}, Cmd.none)
                "Eyelid" ->
                    ({model | cancerParts = [ CancerPart "BasalCellCarcinoma" "基底細胞がん"
                                             , CancerPart "SebaceousGlandCancer" "脂腺がん"
                                             , CancerPart "SquamousCellCarcinoma" "扁平上皮がん"
                                             ]}, Cmd.none)
                _ ->
                    ({model | cancerParts = []}, Cmd.none)





-- VIEW
view : Model -> Html Msg
view model =
    div [] [
        section [ class "section" ]
            [ div [ class "tabs" ]
                [ ul []
                    [ li [ if model.searchMode == Geolocation then class "is-active" else class "none"]
                        [ a [onClick ModeGeolocation]
                            [ text "現在地から探す" ]
                        ]
                    , li [ if model.searchMode == Zipcode then class "is-active" else class "none" ]
                        [ a [onClick ModeZipcode]
                            [ text "郵便番号から探す" ]
                        ]
                    ]
                ]
            ]
    ,div [id "inputField"]
         [
            case model.searchMode of
                Zipcode ->
                    input [placeholder "郵便番号を7桁で入力してください", onInput SubmitZipcode] []
                _ ->
                    div [] []

            ,select [id "cancerType", onInput ChangedCancerPart ] [selectOption "SoftTissue" "四肢軟部肉腫"
                                      ,selectOption "Intraocular" "眼内腫瘍"
                                      ,selectOption "Keratoconjunctival" "角結膜腫瘍"
                                      ,selectOption "Orbital" "眼窩腫瘍"
                                      ,selectOption "Eyelid" "眼瞼腫瘍"
                                     ]
            ,select [id "cancerPart"] []
         ]
    ]