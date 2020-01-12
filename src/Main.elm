port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict as D exposing (Dict, get)
import Element exposing (Attr, Element, alignRight, centerX, column, el, fill, height, html, htmlAttribute, link, maximum, minimum, none, padding, paddingXY, paragraph, pointer, rgb, rgb255, row, scrollbarY, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, multiline)
import Element.Lazy exposing (lazy, lazy3)
import EncodeString exposing (encode)
import Html
import Html.Attributes exposing (class, href, name, selected, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, Value, at, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (requiredAt)
import List exposing (concat, drop, filter, head, length, map)
import Maybe as M exposing (andThen, withDefault)
import String exposing (dropLeft, dropRight, fromFloat, fromInt, split, toInt)
import Task
import Tuple exposing (first, second)
import Url
import Url.Builder exposing (absolute)
import Url.Parser as UrlParser exposing ((</>), Parser, int, oneOf, parse, s, top)



{-
   scroll drawer to top on click
   scroll to ayat on page load
   find surah without basmalah
   ayats with star end messed up data
   get more data
   dropdown select surah
   server load index.html
-}


tempSurahNumber =
    36


type Route
    = SurahPage Int
    | RootModal Location
    | ExportPage
    | Null


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Null top
        , UrlParser.map SurahPage int
        , UrlParser.map ExportPage <| s "export"
        , UrlParser.map (\a b c -> RootModal ( a, b, c )) (int </> int </> int)
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Just url ->
            M.withDefault Null (parse route url)

        Nothing ->
            Null


decodeProgress : Decoder Known
decodeProgress =
    let
        dataDecode =
            Decode.map2
                (\x learned ->
                    ( x
                    , if learned then
                        Learned

                      else
                        Learning
                    )
                )
                (field "root" string)
                (field "learned" Decode.bool)
                |> list
    in
    Decode.map D.fromList dataDecode


textList : Decoder (List String)
textList =
    list (field "text" string)


decodeTranslations : Decoder (List (List String))
decodeTranslations =
    at [ "data", "surahs" ] (list <| at [ "ayahs" ] textList)


{--
[
  {
    "root": "أ ب ب",
    "id": 5000,
    "wordCount": 1,
    "level": 14,
    "categories": [
      {
        "name": "Noun - grass, herbage, pasture",
        "id": 3000,
        "data": [
          {
            "wordNumber": null,
            "word": "وَأَبًّا",
            "translation": "and grass",
            "transliteration": "wa-abban",
            "root": "أ ب ب",
            "location": "(80:31:2)",
            "rootId": 5000
          }
        ],
        "wordCount": 1
      }
    ]
  }
]
--}

decodeLocations : Decoder RootInfoDict
decodeLocations =
    let
        stringToLoc : String -> Location
        stringToLoc s =
            dropLeft 1 s
                |> dropRight 1
                |> split ":"
                |> map (toInt >> M.withDefault 0)
                |> toLocation

        mkWordInfo : String -> String -> String -> String -> WordInfo
        mkWordInfo a b c d =
            WordInfo (stringToLoc a) b c d

        wordDecoder : Decoder WordInfo
        wordDecoder =
            Decode.map4 mkWordInfo
                (field "location" string)
                (field "transliteration" string)
                (field "translation" string)
                (field "word" string)

        dataDecode =
            Decode.map2 (\a b -> (a,b))
                (field "root" string)
                (field "categories"
                    (list ( Decode.map3 WordGroup
                              (field "name" string)
                              (Decode.succeed True)
                              (field "data" (list wordDecoder))
                        )
                    )
                )
    in
    Decode.map D.fromList <| (list dataDecode)


decodeSurah : Int -> SurahInfoDict -> Decoder SurahInfoDict
decodeSurah surahNum surahData =
    succeed (mkSurahData surahNum surahData)
        |> requiredAt [ "data", "englishName" ] string
        |> requiredAt [ "data", "ayahs" ] textList


decodeAllSurahs : Decoder SurahInfoDict
decodeAllSurahs =
    succeed (\x -> List.foldl (\( surahNum, strArr ) surahData -> mkSurahData surahNum surahData "" strArr) D.empty <| indexBy1 <| List.indexedMap Tuple.pair x)
        |> requiredAt [ "data", "surahs" ] (list <| at [ "ayahs" ] textList)


mkSurahData : Int -> SurahInfoDict -> String -> List String -> SurahInfoDict
mkSurahData surahNum surahData name listOfSurahRoots =
    D.insert surahNum ( name, formatSurahText listOfSurahRoots ) surahData


formatSurahText : List String -> List String
formatSurahText arr =
    let
        removeBas : String -> String
        removeBas x =
            String.join "" <|
                (\y ->
                    if length y > 1 then
                        drop 1 y

                    else
                        y
                )
                <|
                    String.split "بِسْمِ اللَّهِ الرَّحْمَٰنِ الرَّحِيمِ " x
    in
    List.indexedMap
        (\i x ->
            if i == 0 then
                removeBas x

            else
                x
        )
        arr


toLocation : List Int -> Location
toLocation l =
    case l of
        a :: b :: c :: _ ->
            ( a, b, c )

        _ ->
            ( 0, 0, 0 )



-- Ajax Requests


rootsToLocationsUrl =
    "https://raw.githubusercontent.com/sbakht/corpus-2.0/master/src/corpus-parser/output.json"


surahsUrl =
    "https://api.alquran.cloud/surah/"


translationUrl s =
    "https://api.alquran.cloud/quran/en.sahih"


allSurahsUrl =
    "https://api.alquran.cloud/quran/quran-uthmani"


getSurahRequestUrl : Int -> String
getSurahRequestUrl surahNumber =
    surahsUrl ++ fromInt surahNumber


wordsCmd : Cmd Msg
wordsCmd =
    decodeLocations
        |> Http.get rootsToLocationsUrl
        |> Http.send LoadRootsInfo


surahCmd : Int -> SurahInfoDict -> Cmd Msg
surahCmd surahNum surahData =
    decodeSurah surahNum surahData
        |> Http.get (getSurahRequestUrl surahNum)
        |> Http.send LoadSurah


translationCmd : List Int -> Cmd Msg
translationCmd surahNums =
    decodeTranslations
        |> Http.get (translationUrl surahNums)
        |> Http.send LoadTranslations


allSurahsCmd : Cmd Msg
allSurahsCmd =
    decodeAllSurahs
        |> Http.get allSurahsUrl
        |> Http.send LoadSurah



-- Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , surahs : SurahInfoDict
    , surahRoots : SurahRoots
    , roots : RootInfoDict
    , known : Known
    , surahNumber : Int
    , surahName : String
    , activeWordDetails : ActiveWordDetails
    , page : String
    , translations : TranslationInfoDict
    }


init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init sessionProgress url key =
    ( { key = key
      , url = url
      , surahs = D.empty
      , surahRoots = SurahRoots D.empty
      , roots = D.empty
      , known = Result.withDefault D.empty <| Decode.decodeValue decodeProgress sessionProgress
      , surahNumber = tempSurahNumber
      , surahName = "Test Name"
      , activeWordDetails = Nothing
      , page = "Home"
      , translations = D.empty
      }
    , surahCmd tempSurahNumber D.empty
    )


type SurahRoots
    = SurahRoots (Dict Index AyatRoots)


type AyatRoots
    = AyatRoots (Dict Index TokenRoots)


type TokenRoots
    = TokenRoots (Dict Index Root)


type Progress
    = Unknown
    | Learning
    | Learned


type alias SurahInfoDict =
    Dict Int ( String, List String )


type alias TranslationInfoDict =
    Dict Int (List String)


type alias RootInfoDict =
    Dict Root (List WordGroup)


type alias WordGroup =
    { name : String, collapsed : Bool, words : List WordInfo }


type alias WordInfo =
    { location : Location, transliteration : String, translation : String, word : String }


type alias ActiveWordDetails =
    Maybe ( String, Location )


type alias Root =
    String


type alias Index =
    Int


type alias Location =
    ( Int, Int, Int )


type alias Known =
    Dict String Progress


port saveProgress : List Tracker -> Cmd msg


port loadProgress : (Value -> msg) -> Sub msg


type alias Tracker =
    { root : Root, learned : Bool }


type Msg
    = SetKnown Root Progress
    | LoadSurah (Result Http.Error SurahInfoDict)
    | LoadRootsInfo (Result Http.Error RootInfoDict)
    | LoadTranslations (Result Http.Error (List (List String)))
    | SetActiveDetails ( Root, Location )
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleOverlayGroup Int Root
    | SelectSurah String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetKnown root isKnown ->
            let
                learn : Root -> Progress -> Known -> Known
                learn rootLetters progress learned =
                    if progress == Unknown then
                        learned
                        -- this shouldn't happen, need to find way to deal with this

                    else
                        D.insert rootLetters progress learned

                forget : Root -> Known -> Known
                forget rootLetters learned =
                    D.remove rootLetters learned

                cmd known =
                    saveProgress
                        (map
                            (\( k, v ) ->
                                Tracker k <|
                                    if v == Learned then
                                        True

                                    else
                                        False
                            )
                         <|
                            D.toList known
                        )
            in
            if root == "" then
                ( model, Cmd.none )

            else
                case isKnown of
                    Learned ->
                        ( { model | known = learn root Learned model.known }, cmd <| learn root Learned model.known )

                    Learning ->
                        ( { model | known = learn root Learning model.known }, cmd <| learn root Learning model.known )

                    Unknown ->
                        ( { model | known = forget root model.known }, cmd <| forget root model.known )

        SetActiveDetails ( root, loc ) ->
            if root == "" then
                ( { model | activeWordDetails = Nothing }, Cmd.none )

            else
                ( { model | activeWordDetails = Just ( root, loc ) }, Cmd.none )

        LoadSurah (Ok surahData) ->
            if D.isEmpty model.roots then
                ( { model | surahs = surahData }, wordsCmd )

            else
                case model.activeWordDetails of
                    Just ( _, loc ) ->
                        ( { model | surahs = surahData }, scrollToWord loc )

                    Nothing ->
                        ( { model | surahs = surahData }, Cmd.none )

        LoadSurah _ ->
            ( model, Cmd.none )

        LoadRootsInfo (Ok roots) ->
            updateRoots (Debug.log "decode roots" roots) model

        LoadRootsInfo e ->
            ( Debug.log (Debug.toString e) <| model, Cmd.none )

        LoadTranslations (Ok translations) ->
            ( { model | translations = D.fromList <| indexBy1 <| List.indexedMap Tuple.pair translations }, Cmd.none )

        LoadTranslations _ ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case toRoute (Url.toString url) of
                SurahPage surahNum ->
                    if D.member surahNum model.surahs then
                        ( { model | surahNumber = surahNum }, Cmd.none )

                    else
                        ( { model | surahNumber = surahNum }, surahCmd surahNum model.surahs )

                RootModal ( surahNum, ai, wi ) ->
                    let
                        tokens : TokenRoots
                        tokens =
                            getTokenRoots surahNum ai model.surahRoots

                        root =
                            getRootFromToken wi tokens
                    in
                    if D.member surahNum model.surahs then
                        if surahNum == model.surahNumber then
                            ( { model | activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, Cmd.none )

                        else
                            ( { model | surahNumber = surahNum, activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, scrollToWord ( surahNum, ai, wi ) )

                    else
                        ( { model | surahNumber = surahNum, activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, surahCmd surahNum model.surahs )

                ExportPage ->
                    ( { model | page = "Export" }, Cmd.batch [ translationCmd <| surahListWithCompleteAyats model.surahRoots model.known, allSurahsCmd ] )

                Null ->
                    ( model, Cmd.none )

        SelectSurah url ->
            ( { model | activeWordDetails = Nothing }, Nav.pushUrl model.key url )

        ToggleOverlayGroup index root ->
            let
                oldWordsGroups : List WordGroup
                oldWordsGroups =
                    get root model.roots
                        |> M.withDefault []

                toggleIthGroup : List WordGroup
                toggleIthGroup =
                    List.indexedMap
                        (\i group ->
                            if i == index then
                                { group | collapsed = not group.collapsed }

                            else
                                group
                        )
                        oldWordsGroups

                newRootsData : RootInfoDict
                newRootsData =
                    D.insert root toggleIthGroup model.roots
            in
            ( { model | roots = newRootsData }, Cmd.none )


addRoot : Root -> Location -> SurahRoots -> SurahRoots
addRoot root ( si, ai, wi ) (SurahRoots surahs) =
    let
        newSurah : SurahRoots
        newSurah =
            SurahRoots (D.insert si (newAyat D.empty) surahs)

        newAyat ayats =
            AyatRoots (D.insert ai (newToken D.empty) ayats)

        newToken tokens =
            TokenRoots (D.insert wi root tokens)

        insert : Dict Index TokenRoots -> Dict Index Root -> AyatRoots
        insert ayats tokens =
            AyatRoots (D.insert ai (newToken tokens) ayats)
    in
    case get si surahs of
        Just (AyatRoots ayats) ->
            case get ai ayats of
                Just (TokenRoots tokens) ->
                    case get wi tokens of
                        Just _ ->
                            SurahRoots surahs

                        Nothing ->
                            SurahRoots (D.insert si (insert ayats tokens) surahs)

                Nothing ->
                    SurahRoots (D.insert si (newAyat ayats) surahs)

        Nothing ->
            newSurah


updateRoots : RootInfoDict -> Model -> ( Model, Cmd Msg )
updateRoots roots model =
    let
        stuff : Root -> List WordGroup -> SurahRoots -> SurahRoots
        stuff root wordsGroups dic =
            List.foldl (startToAddRoot root) dic <| joinAllWordGroups wordsGroups

        startToAddRoot : Root -> WordInfo -> SurahRoots -> SurahRoots
        startToAddRoot root wordInfo surahs =
            addRoot root wordInfo.location surahs
    in
    ( { model | surahRoots = D.foldl stuff (SurahRoots D.empty) roots, roots = roots }, Cmd.none )


englishFontSize =
    Font.size 14


arabicFontSize =
    Font.size 30


pageBackground =
    Background.color <| rgb255 244 241 222

colorGreen =
    Font.color <| rgb 0 255 0


contentHeight =
    900


view : Model -> Browser.Document Msg
view model =
    { title = "Learn Quran Roots"
    , body =
        [ Element.layout [ paddingXY 10 0, pageBackground ] <|
            column [ width fill ]
                [ viewHeader model
                , case model.page of
                    "Export" ->
                        viewCSV model

                    _ ->
                        viewHome model
                ]
        ]
    }


viewHeader : Model -> Element Msg
viewHeader model =
    let
        totalWords =
            D.foldl (\_ v accum -> length v + accum) 0 model.roots

        totalOccurrences root =
            get root model.roots
                |> M.withDefault []
                |> length

        totalKnown =
            D.foldr (\root _ accum -> totalOccurrences root + accum) 0 model.known

        percentage =
            toFloat totalKnown
                / toFloat totalWords
                * 10000
                |> floor
                |> toFloat
                |> (\x -> x / 100)

        updateUrl url =
            SelectSurah <| "/" ++ url

        viewSurahName : Int -> SurahInfoDict -> Element Msg
        viewSurahName si surahData =
            el [ centerX, colorGreen ] <| text (M.withDefault "" <| M.map first <| get si surahData)
    in
    row [ width fill, paddingXY 0 10, spacing 10 ] <|
        [ el [] <| text "Home"
        , link [] <| { url = "/known", label = text "Known" }
        , link [] <| { url = "/export", label = text "Export" }
        , el [] <| html <| Html.select [ onInput updateUrl ] <| map (\x -> Html.option [ value (fromInt x), selected (x == model.surahNumber) ] [ Html.a [ href ("/" ++ fromInt x) ] [ Html.text ("Surah " ++ fromInt x) ] ]) <| List.range 1 144
        , viewSurahName model.surahNumber model.surahs
        , el [ alignRight, colorGreen ] <| text (fromFloat percentage ++ "%")
        , el [ alignRight, colorGreen ] <| text (fromInt <| countKnownAyats model.surahNumber model.surahRoots model.known)
        ]


viewCSV : Model -> Element Msg
viewCSV model =
    let
        surahsToDo : List Int
        surahsToDo =
            surahListWithCompleteAyats model.surahRoots model.known

        surahData : Index -> List ( Index, String )
        surahData i =
            getKnownAyats i model.surahRoots model.surahs model.known

        getKnownAyats : Int -> SurahRoots -> SurahInfoDict -> Known -> List ( Index, String )
        getKnownAyats si surahRoots surahsData known =
            let
                filterKnownAyats : AyatRoots -> AyatRoots
                filterKnownAyats (AyatRoots ayats) =
                    AyatRoots <| D.filter isKnownAyat ayats

                isKnownAyat : Int -> TokenRoots -> Bool
                isKnownAyat _ (TokenRoots tokens) =
                    tokens == D.filter isKnownRoot tokens

                isKnownRoot : Int -> Root -> Bool
                isKnownRoot _ root =
                    D.member root known

                toAyatStrings : AyatRoots -> List ( Index, String )
                toAyatStrings (AyatRoots ayats) =
                    filter (\( i, _ ) -> D.member i ayats) <| indexBy1 <| List.indexedMap Tuple.pair <| M.withDefault [] <| M.map second <| get si surahsData
            in
            toAyatStrings <| filterKnownAyats (getAyatsRoots si surahRoots)

        translationBySurah surahIndex =
            M.withDefault [] <| get surahIndex <| model.translations

        translation surahIndex ai =
            replaceMultiAyatQuote <| M.withDefault "" <| Array.get (ai - 1) <| Array.fromList <| translationBySurah surahIndex

        replaceMultiAyatQuote str =
            if String.startsWith "\"" str && (remainderBy 2 (length <| String.indexes "\"" str) > 0) then
                "'" ++ String.dropLeft 1 str

            else
                str

        outputPerSurah : Index -> List ( Index, String ) -> List String
        outputPerSurah surahIndex list =
            map (\( ai, ayat ) -> String.join "\t" [ ayat, translation surahIndex ai, fromInt ai, "Surah" ++ fromInt surahIndex ]) list
    in
    column [ width fill, padding 10 ]
        [ multiline [ height (fill |> minimum 500) ]
            { onChange = \_ -> NoOp
            , text = String.join "\n" <| concat <| map (\i -> outputPerSurah i (surahData i)) <| List.range 1 144
            , placeholder = Nothing
            , label = labelAbove [] (text "Import To Anki Flashcard Program:")
            , spellcheck = False
            }
        , paragraph [ centerX, paddingXY 10 20, width (fill |> maximum 900) ] [ text """These are all the ayat that you have marked all their words' roots as learning or learned. This can be imported into a
            flashcard program like Anki so you can practice translating. This is a csv (comma separated values) file, separated by tabs. The 3rd column
            is the ayat number and the 4th is Surah number which can be used as tags in Anki.""" ]
        ]


viewHome : Model -> Element Msg
viewHome model =
    row [ height fill, width fill, paddingXY 10 10, spacingXY 10 0, englishFontSize ]
        [ viewOverlay model
        , viewSurah model
        ]


viewSurah : Model -> Element Msg
viewSurah model =
    let
        viewBasmalah : Element Msg
        viewBasmalah =
            el [ centerX, width (fill |> maximum 500), arabicFontSize, Font.family [ Font.typeface "KFGQPC Uthman Taha Naskh" ] ] (text "بِسْمِ اللَّهِ الرَّحْمَٰنِ الرَّحِيم")
    in
    column [ height (fill |> maximum contentHeight), width fill, spacing 20, paddingXY 0 10, scrollbarY ]
        (case get model.surahNumber model.surahs of
            Just surah ->
                (if model.surahNumber /= 9 || model.surahNumber /= 1 then
                    viewBasmalah

                 else
                    none
                )
                    :: (surah
                            |> second
                            |> List.indexedMap Tuple.pair
                            |> indexBy1
                            |> map (viewAyat model)
                       )

            Nothing ->
                []
        )


viewAyat : Model -> ( Int, String ) -> Element Msg
viewAyat model ( ai, ayatString ) =
    row [ spacingXY 5 20 ] [ lazy printAyatNumber <| ai, printAyat model ( ai, ayatString ) ]


printAyatNumber : Int -> Element Msg
printAyatNumber ai =
    el [] <| text (fromInt ai ++ ".")


printAyat : Model -> ( Int, String ) -> Element Msg
printAyat model ( ai, ayatString ) =
    let
        tokens : TokenRoots
        tokens =
            getTokenRoots model.surahNumber ai model.surahRoots

        joinedYaa : List String -> List String
        joinedYaa =
            List.foldr addWhenYaa []
    in
    ayatString
        |> String.split " "
        |> joinedYaa
        |> Array.fromList
        |> Array.toIndexedList
        |> indexBy1
        |> map (viewWord model.surahNumber model.activeWordDetails model.known tokens ai)
        |> Element.paragraph [ arabicFontSize, spacingXY 0 20, Font.family [ Font.typeface "KFGQPC Uthman Taha Naskh" ] ]


addWhenYaa : String -> List String -> List String
addWhenYaa curr accum =
    --    if curr == encode then
    if List.member curr encode then
        case accum of
            x :: xs ->
                (curr ++ " " ++ x) :: xs

            _ ->
                curr :: accum

    else
        curr :: accum


indexBy1 : List ( Int, a ) -> List ( Int, a )
indexBy1 =
    map (\( i, a ) -> ( i + 1, a ))


wordColor : Progress -> Bool -> Element.Attribute Msg
wordColor isKnown isLearnable =
    htmlAttribute <|
        class <|
            case ( isKnown, isLearnable ) of
                ( Learned, _ ) ->
                    "learned-word"

                ( Learning, True ) ->
                    "learning-word"

                ( Unknown, True ) ->
                    "unknown-word"

                ( _, _ ) ->
                    "no-root-word"


viewWord : Int -> ActiveWordDetails -> Known -> TokenRoots -> Int -> ( Int, String ) -> Element Msg
viewWord surahNumber activeWordDetails known tokens ai ( wi, w ) =
    let
        root =
            getRootFromToken wi tokens

        isKnown =
            getProgress root known

        isLearnable =
            root /= ""

        path =
            locationToUrl ( surahNumber, ai, wi )

        backgroundColor =
            htmlAttribute <|
                class <|
                    case activeWordDetails of
                        Just ( r, ( a, b, c ) ) ->
                            if b == ai && c == wi then
                                "clicked-active-root"

                            else if r == root then
                                "active-root"

                            else
                                ""

                        _ ->
                            ""

        stuff knowned learnable bgColor =
            if isLearnable == True then
                Element.link [ wordColor knowned learnable, bgColor, padding 5 ] <|
                    { url = path, label = text (w ++ " " ++ "") }

            else
                el [ padding 2 ] <| text (w ++ " " ++ "")
    in
    lazy3 stuff isKnown isLearnable backgroundColor


getRootFromToken : Index -> TokenRoots -> Root
getRootFromToken index (TokenRoots token) =
    get index token |> M.withDefault ""


getProgress : Root -> Known -> Progress
getProgress root known =
    get root known
        |> withDefault Unknown



------ OVERLAY ------


viewOverlay : Model -> Element Msg
viewOverlay { roots, known, activeWordDetails } =
    column [ height (fill |> maximum contentHeight), width (fill |> maximum 600), scrollbarY ] <|
        case activeWordDetails of
            Just ( root, loc ) ->
                [ viewOverlayWord root loc roots
                , viewProgressOptions root <| getProgress root known
                , viewOtherWordsWithSameRoot root loc roots
                ]

            Nothing ->
                [ el [ height fill ] <| text "Click on a word to view its info" ]


viewOverlayWord : Root -> Location -> RootInfoDict -> Element Msg
viewOverlayWord root loc roots =
    let
        filterToWord : List WordInfo -> Maybe WordInfo
        filterToWord =
            head << filter (\{ location } -> location == loc)

        printWord : WordInfo -> Element Msg
        printWord { word, translation, location } =
            column [ width fill, spacingXY 0 10 ]
                [ el [ arabicFontSize, centerX ] <| text ("(" ++ word ++ " (" ++ root)
                , el [ centerX ] <| text translation
                , el [ centerX ] <| text (locationToString location)
                ]
    in
    get root roots
        |> M.map joinAllWordGroups
        |> andThen filterToWord
        |> M.map printWord
        |> withDefault none


viewProgressOptions : Root -> Progress -> Element Msg
viewProgressOptions root progress =
    el [ centerX, paddingXY 0 10 ] <|
        Input.radio [ spacing 10 ]
            { onChange = SetKnown root
            , selected = Just progress
            , label = Input.labelAbove [ paddingXY 0 5 ] (text "Set your learning progress on this root:")
            , options =
                [ Input.option Unknown (text "Not Learned")
                , Input.option Learning (text "Learning")
                , Input.option Learned (text "Learned")
                ]
            }


viewOtherWordsWithSameRoot : Root -> Location -> RootInfoDict -> Element Msg
viewOtherWordsWithSameRoot root location roots =
    let
        filterOutActiveWord : List WordInfo -> List WordInfo
        filterOutActiveWord =
            filter (\wordInfo -> wordInfo.location /= location)

        printTable : List WordInfo -> List (Element Msg)
        printTable =
            map (lazy printWordDetails)

        numWords : WordGroup -> String
        numWords group =
            "(" ++ (fromInt <| length group.words) ++ ")"

        printHeader : Int -> WordGroup -> Element Msg
        printHeader i group =
            row [ width fill, paddingXY 0 10, pointer, onClick <| ToggleOverlayGroup i root ]
                [ el [ centerX, Font.bold ] <| text (group.name ++ " " ++ numWords group) ]

        printGroup : Int -> WordGroup -> List (Element Msg)
        printGroup i group =
            case group.collapsed of
                True ->
                    [ printHeader i group ]

                False ->
                    printHeader i group :: printTable group.words

        printGroups : List WordGroup -> List (Element Msg)
        printGroups =
            concat << List.indexedMap printGroup
    in
    get root roots
        |> M.map (column [ width fill ] << printGroups)
        |> withDefault none


printWordDetails : WordInfo -> Element Msg
printWordDetails { location, translation, word } =
    row [ width fill ]
        [ link [ Font.color <| rgb 0 0 255 ]
            { url = locationToUrl location
            , label = text <| locationToString location
            }
        , lazy (el [ centerX ] << text) translation
        , lazy (el [ alignRight, arabicFontSize ] << text) word
        ]


locationToString : Location -> String
locationToString ( a, b, c ) =
    fromInt a ++ ":" ++ fromInt b ++ ":" ++ fromInt c


scrollToWord : Location -> Cmd Msg
scrollToWord loc =
    Dom.getElement (locationToUrl loc)
        |> Task.andThen (\info -> Dom.setViewport 0 (info.element.y - (info.viewport.height / 2)))
        --    |>  Dom.getViewportOf id
        --      |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


locationToUrl : Location -> String
locationToUrl ( a, b, c ) =
    absolute (map fromInt [ a, b, c ]) []


joinAllWordGroups : List WordGroup -> List WordInfo
joinAllWordGroups =
    List.foldl (\w accum -> List.append accum w.words) []


getAyatsRoots : Index -> SurahRoots -> AyatRoots
getAyatsRoots index (SurahRoots surahs) =
    get index surahs |> M.withDefault (AyatRoots D.empty)


getTokenRoots : Index -> Index -> SurahRoots -> TokenRoots
getTokenRoots si ai surahRoots =
    case getAyatsRoots si surahRoots of
        AyatRoots ayats ->
            get ai ayats |> M.withDefault (TokenRoots D.empty)


surahListWithCompleteAyats : SurahRoots -> Known -> List Int
surahListWithCompleteAyats (SurahRoots surahs) known =
    let
        completeAyat : Index -> AyatRoots -> Bool
        completeAyat i (AyatRoots ayats) =
            (D.size <| D.filter isKnownAyat ayats) > 0

        -- improve to break on first find
        isKnownAyat : Int -> TokenRoots -> Bool
        isKnownAyat _ (TokenRoots tokens) =
            tokens == D.filter isKnownRoot tokens

        isKnownRoot : Int -> Root -> Bool
        isKnownRoot _ root =
            D.member root known
    in
    D.filter completeAyat surahs
        |> D.keys


countKnownAyats : Int -> SurahRoots -> Known -> Int
countKnownAyats si (SurahRoots surahs) known =
    let
        filterKnownAyats : AyatRoots -> Int
        filterKnownAyats (AyatRoots ayats) =
            D.size <| D.filter isKnownAyat ayats

        isKnownAyat : Int -> TokenRoots -> Bool
        isKnownAyat _ (TokenRoots tokens) =
            tokens == D.filter isKnownRoot tokens

        isKnownRoot : Int -> Root -> Bool
        isKnownRoot _ root =
            D.member root known
    in
    D.values surahs
        |> map filterKnownAyats
        |> List.sum

main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
