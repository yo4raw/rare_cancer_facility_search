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
type SearchMode = CurrentLocation | Zipcode

--がん部位
type CancerType = SoftTissue           --四肢軟部肉腫
                  | Intraocular        --眼内腫瘍
                  | Keratoconjunctival --角結膜腫瘍
                  | Orbital            --眼窩腫瘍
                  | Eyelid             --眼瞼腫瘍

-- 眼内腫瘍のタイプ
type IntraocularType = Retinoblastoma         --網膜芽細胞腫
                     | UvealMalignantMelanoma --ぶどう膜悪性黒色腫
                     | IntraocularLymphoma    --眼内リンパ腫

-- 角結膜腫瘍のタイプ
type KeratoconjunctivalType = ConjunctivalMalignantLymphoma    --結膜悪性リンパ腫
                     | KeratoconjunctivalSquamousCellCarcinoma --角結膜扁平上皮癌
                     | ConjunctivalMalignantMelanoma           --結膜悪性黒色腫

-- 眼内腫瘍のタイプ
type Intrimport Browser
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
         type SearchMode = CurrentLocation | Zipcode

         --がん部位
         type CancerType = SoftTissue         --四肢軟部肉腫
                         | Intraocular        --眼内腫瘍
                         | Keratoconjunctival --角結膜腫瘍
                         | Orbital            --眼窩腫瘍
                         | Eyelid             --眼瞼腫瘍

         -- 眼内腫瘍の種類
         type IntraocularType = Retinoblastoma         --網膜芽細胞腫
                              | UvealMalignantMelanoma --ぶどう膜悪性黒色腫
                              | IntraocularLymphoma    --眼内リンパ腫

         -- 角結膜腫瘍の種類
         type KeratoconjunctivalType = ConjunctivalMalignantLymphoma           --結膜悪性リンパ腫
                                     | KeratoconjunctivalSquamousCellCarcinoma --角結膜扁平上皮癌
                                     | ConjunctivalMalignantMelanoma           --結膜悪性黒色腫

         -- 眼窩腫瘍の種類
         type OrbitalType = OrbitalMalignantLymphoma --眼窩悪性リンパ腫
                          | LacrimalGlandCancer      --涙腺がん

         -- 眼瞼腫瘍の種類
         type EyelidType = BasalCellCarcinoma        --基底細胞がん
                         | SebaceousGlandCancer      --脂腺がん
                         | SquamousCellCarcinoma     --扁平上皮がん

         -- SerchModeでcurrentLocation選択時の病院情報

         -- SerchModeでzipCode選択時の病院情報


         -- MODEL
         type alias Model =
             {input : String
             , memos : List String
             , facilities : List String
             , mode : String
             }

         init : Model
         init =
             {input = ""
             , memos = []
             , facilities = []
             , mode = "Zipcode"
             }


         -- UPDATE
         type Msg =
             Msg


         update : Msg -> Model -> Model
         update msg model =
             model

         -- VIEW
         view : Model -> Html Msg
         view model =
             div [id "map"] [button [] [text "aaa"]]aocularType = Retinoblastoma         --網膜芽細胞腫
                     | UvealMalignantMelanoma --ぶどう膜悪性黒色腫
                     | IntraocularLymphoma    --眼内リンパ腫

-- 眼内腫瘍のタイプ
type IntraocularType = Retinoblastoma         --網膜芽細胞腫
                     | UvealMalignantMelanoma --ぶどう膜悪性黒色腫
                     | IntraocularLymphoma    --眼内リンパ腫

-- SerchModeでcurrentLocation選択時の病院情報

-- SerchModeでzipCode選択時の病院情報


-- MODEL
type alias Model =
    {input : String
    , memos : List String
    , facilities : List String
    , mode : String
    }

init : Model
init =
    {input = ""
    , memos = []
    , facilities = []
    , mode = "Zipcode"
    }


-- UPDATE
type Msg =
    Msg


update : Msg -> Model -> Model
update msg model =
    model

-- VIEW
view : Model -> Html Msg
view model =
    div [id "map"] [button [] [text "aaa"]]