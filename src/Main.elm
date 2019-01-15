port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Attr, Element, alignRight, centerX, column, el, fill, fillPortion, height, html, htmlAttribute, link, maximum, minimum, mouseOver, none, padding, paddingXY, paragraph, pointer, rgb, rgb255, row, scrollbarY, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, multiline)
import Element.Lazy exposing (lazy, lazy3)
import EncodeString exposing (encode)
import Html
import Html.Attributes exposing (checked, class, classList, for, href, id, name, selected, type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, Value, at, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt)
import List exposing (concat, drop, filter, head, length, map)
import String exposing (dropLeft, dropRight, fromChar, fromFloat, fromInt, split, toInt)
import Task
import Tuple exposing (first, mapFirst, mapSecond, second)
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
    | KnownPage
    | ExportPage
    | Null


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Null top
        , UrlParser.map SurahPage int
        , UrlParser.map KnownPage <| s "known"
        , UrlParser.map ExportPage <| s "export"
        , UrlParser.map (\a b c -> RootModal ( a, b, c )) (int </> int </> int)
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Just url ->
            Maybe.withDefault Null (parse route url)

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
    Decode.map Dict.fromList dataDecode


textList : Decoder (List String)
textList =
    list (field "text" string)


decodeTranslations : Decoder (List (List String))
decodeTranslations =
    at [ "data", "surahs" ] (list <| at [ "ayahs" ] textList)


decodeLocations : Decoder RootsData
decodeLocations =
    let
        stringToLoc : String -> Location
        stringToLoc s =
            dropLeft 1 s
                |> dropRight 1
                |> split ":"
                |> map (toInt >> Maybe.withDefault 0)
                |> toTuple3

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
            list wordDecoder
                |> field "data"
                |> Decode.map3 WordsGroup (field "name" string) (Decode.succeed True)
                |> list
    in
    Decode.map Dict.fromList <| Decode.keyValuePairs dataDecode


decodeSurah : Int -> SurahData -> Decoder SurahData
decodeSurah surahNum surahData =
    succeed (mkSurahData surahNum surahData)
        |> requiredAt [ "data", "englishName" ] string
        |> requiredAt [ "data", "ayahs" ] textList


decodeAllSurahs : Decoder SurahData
decodeAllSurahs =
    succeed (\x -> List.foldl (\( surahNum, strArr ) surahData -> mkSurahData surahNum surahData "" strArr) Dict.empty <| indexBy1 <| List.indexedMap Tuple.pair x)
        |> requiredAt [ "data", "surahs" ] (list <| at [ "ayahs" ] textList)


mkSurahData : Int -> SurahData -> String -> List String -> SurahData
mkSurahData surahNum surahData name listOfSurahRoots =
    if surahNum == 1 then
        Dict.insert surahNum ( name, drop 1 <| formatSurahText listOfSurahRoots ) surahData

    else
        Dict.insert surahNum ( name, formatSurahText listOfSurahRoots ) surahData


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


dropBasmalah : List String -> List String
dropBasmalah =
    drop 1


toTuple3 : List Int -> Location
toTuple3 l =
    case l of
        a :: b :: c :: _ ->
            ( a, b, c )

        _ ->
            ( 0, 0, 0 )



-- Ajax Requests


rootsToLocationsUrl =
    "https://raw.githubusercontent.com/sbakht/corpus-2.0/master/src/corpus-parser/output.json"


surahsUrl =
    --    "https://raw.githubusercontent.com/semarketir/quranjson/master/source/surah/surah_"
    "https://api.alquran.cloud/surah/"


translationUrl s =
    --    "http://localhost:3001/surah?surahNum=" ++ (String.join "," <| map fromInt s)
    "https://api.alquran.cloud/quran/en.sahih"


allSurahsUrl =
    "https://api.alquran.cloud/quran/quran-uthmani"


getSurahRequestUrl : Int -> String
getSurahRequestUrl surahNumber =
    --    surahsUrl ++ fromInt surahNumber ++ ".json"
    surahsUrl ++ fromInt surahNumber


wordsCmd : Cmd Msg
wordsCmd =
    decodeLocations
        |> Http.get rootsToLocationsUrl
        |> Http.send LoadRootsData


surahCmd : Int -> SurahData -> Cmd Msg
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
    , surahs : SurahData
    , surahRoots : SurahRoots
    , rootsData : RootsData
    , known : Known
    , surahNumber : Int
    , surahName : String
    , activeWordDetails : ActiveWordDetails
    , page : String
    , translations : TranslationsData
    }


init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init sessionProgress url key =
    ( { key = key
      , url = url
      , surahs = Dict.empty
      , surahRoots = SurahRoots Dict.empty
      , rootsData = Dict.empty
      , known = Result.withDefault Dict.empty <| Decode.decodeValue decodeProgress sessionProgress
      , surahNumber = tempSurahNumber
      , surahName = "Test Name"
      , activeWordDetails = Nothing
      , page = "Home"
      , translations = Dict.empty
      }
    , surahCmd tempSurahNumber Dict.empty
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


type alias SurahData =
    Dict Int ( String, List String )


type alias TranslationsData =
    Dict Int (List String)


type alias RootsData =
    Dict Root (List WordsGroup)


type alias WordsGroup =
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


rootsDataToSurahRoots : RootsData -> SurahRoots
rootsDataToSurahRoots rootsData =
    let
        stuff : Root -> List WordsGroup -> SurahRoots -> SurahRoots
        stuff root wordsGroups dic =
            List.foldl (moreStuff root) dic (List.foldl (\w accum -> List.append accum w.words) [] wordsGroups)

        moreStuff : Root -> WordInfo -> SurahRoots -> SurahRoots
        moreStuff root wordInfo surahs =
            addRoot root wordInfo.location surahs
    in
    Dict.foldl stuff (SurahRoots Dict.empty) rootsData


addRoot : Root -> Location -> SurahRoots -> SurahRoots
addRoot root ( si, ai, wi ) (SurahRoots surahs) =
    let
        newSurah : SurahRoots
        newSurah =
            SurahRoots (Dict.insert si (newAyat Dict.empty) surahs)

        newAyat ayats =
            AyatRoots (Dict.insert ai (newToken Dict.empty) ayats)

        newToken tokens =
            TokenRoots (Dict.insert wi root tokens)

        insert : Dict Index TokenRoots -> Dict Index Root -> AyatRoots
        insert ayats tokens =
            AyatRoots (Dict.insert ai (newToken tokens) ayats)
    in
    case Dict.get si surahs of
        Just (AyatRoots ayats) ->
            case Dict.get ai ayats of
                Just (TokenRoots tokens) ->
                    case Dict.get wi tokens of
                        Just _ ->
                            SurahRoots surahs

                        Nothing ->
                            SurahRoots (Dict.insert si (insert ayats tokens) surahs)

                Nothing ->
                    SurahRoots (Dict.insert si (newAyat ayats) surahs)

        Nothing ->
            newSurah


port saveProgress : List Tracker -> Cmd msg


port loadProgress : (Value -> msg) -> Sub msg


type alias Tracker =
    { root : Root, learned : Bool }


type Msg
    = SetKnown Root Progress
    | LoadSurah (Result Http.Error SurahData)
    | LoadRootsData (Result Http.Error RootsData)
    | LoadTranslations (Result Http.Error (List (List String)))
    | SetActiveDetails ( Root, Location )
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleOverlayGroup Int Root
    | PushUrl String
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
                        learned -- this shouldn't happen, need to find way to deal with this
                    else
                        Dict.insert rootLetters progress learned

                forget : Root -> Known -> Known
                forget rootLetters learned =
                    Dict.remove rootLetters learned

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
                            Dict.toList known
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
            if Dict.isEmpty model.rootsData then
                ( { model | surahs = surahData }, wordsCmd )

            else
                case model.activeWordDetails of
                    Just ( _, loc ) ->
                        ( { model | surahs = surahData }, scrollToWord loc )

                    Nothing ->
                        ( { model | surahs = surahData }, Cmd.none )

        LoadSurah _ ->
            ( model, Cmd.none )

        LoadRootsData (Ok rootsData) ->
            ( { model | surahRoots = rootsDataToSurahRoots rootsData, rootsData = rootsData }, Cmd.none )

        LoadRootsData _ ->
            ( model, Cmd.none )

        LoadTranslations (Ok translations) ->
            ( { model | translations = Dict.fromList <| indexBy1 <| List.indexedMap Tuple.pair translations }, Cmd.none )

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
                    if Dict.member surahNum model.surahs then
                        ( { model | surahNumber = surahNum }, Cmd.none )

                    else
                        ( { model | surahNumber = surahNum }, surahCmd surahNum model.surahs )

                RootModal ( surahNum, ai, wi ) ->
                    let
                        ayats : AyatRoots
                        ayats =
                            getAyatsRoots surahNum model.surahRoots

                        tokens : TokenRoots
                        tokens =
                            getTokenRoots ai ayats

                        root =
                            getRootFromToken wi tokens
                    in
                    if Dict.member surahNum model.surahs then
                        if surahNum == model.surahNumber then
                            ( { model | activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, Cmd.none )

                        else
                            ( { model | surahNumber = surahNum, activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, scrollToWord ( surahNum, ai, wi ) )

                    else
                        ( { model | surahNumber = surahNum, activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, surahCmd surahNum model.surahs )

                KnownPage ->
                    ( { model | page = "Known" }, Cmd.none )

                ExportPage ->
                    ( { model | page = "Export" }, Cmd.batch [ translationCmd <| surahListWithCompleteAyats model.surahRoots model.known, allSurahsCmd ] )

                Null ->
                    ( model, Cmd.none )

        PushUrl url ->
            ( model, Nav.pushUrl model.key url )

        ToggleOverlayGroup index root ->
            let
                oldWordsGroups : List WordsGroup
                oldWordsGroups =
                    case Dict.get root model.rootsData of
                        Just data ->
                            data

                        Nothing ->
                            []

                newWordsGroups : List WordsGroup
                newWordsGroups =
                    List.indexedMap
                        (\i group ->
                            if i == index then
                                { group | collapsed = not group.collapsed }

                            else
                                group
                        )
                        oldWordsGroups

                newRootsData : RootsData
                newRootsData =
                    Dict.insert root newWordsGroups model.rootsData
            in
            ( { model | rootsData = newRootsData }, Cmd.none )


englishFontSize =
    Font.size 14


arabicFontSize =
    Font.size 30


pageBackground =
    Background.color <| rgb255 244 241 222


learnedColor =
    Font.color <| rgb255 61 64 91


learningColor =
    Font.color <| rgb255 0 0 255


notLearnedColor =
    Font.color <| rgb255 224 122 95


hoverBackground =
    Background.color <| rgb255 242 204 143


hoverClickedBackground =
    Background.color <| rgb255 242 204 143


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
                    "Known" ->
                        viewKnown model

                    "Export" ->
                        viewCSV model

                    _ ->
                        viewHome model
                ]
        ]
    }


viewHome : Model -> Element Msg
viewHome model =
    row [ height fill, width fill, paddingXY 10 10, spacingXY 10 0, englishFontSize ]
        [ viewOverlay model
        , viewSurah model
        ]


viewKnown : Model -> Element Msg
viewKnown model =
    let
        totalWords =
            Dict.foldl (\_ v accum -> length v + accum) 0 model.rootsData

        totalOccurrences root =
            Dict.get root model.rootsData
                |> Maybe.withDefault []
                |> length

        percentage root =
            toFloat (totalOccurrences root)
                / toFloat totalWords
                * 10000
                |> floor
                |> toFloat
                |> (\x -> x / 100)
                |> fromFloat
                |> (\x -> x ++ "%")
    in
    column [ spacing 20 ] <|
        (Dict.map (\root _ -> root) model.known
            |> Dict.toList
            |> List.indexedMap
                (\i tuple ->
                    row [ spacing 5 ]
                        [ text <| fromInt (i + 1) ++ "."
                        , text <| second tuple
                        , text <| percentage (second tuple)
                        ]
                )
        )


viewCSV : Model -> Element Msg
viewCSV model =
    let
        surahsToDo : List Int
        surahsToDo =
            surahListWithCompleteAyats model.surahRoots model.known

        surahData : Index -> List ( Index, String )
        surahData i =
            getKnownAyats i model.surahRoots model.surahs model.known

        translationBySurah surahIndex =
            Maybe.withDefault [] <| Dict.get surahIndex <| model.translations

        translation surahIndex ai =
            replaceMultiAyatQuote <| Maybe.withDefault "" <| Array.get (ai - 1) <| Array.fromList <| translationBySurah surahIndex

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


viewHeader : Model -> Element Msg
viewHeader model =
    let
        totalWords =
            Dict.foldl (\_ v accum -> length v + accum) 0 model.rootsData

        totalOccurrences root =
            Dict.get root model.rootsData
                |> Maybe.withDefault []
                |> length

        totalKnown =
            Dict.foldr (\root _ accum -> totalOccurrences root + accum) 0 model.known

        percentage =
            toFloat totalKnown
                / toFloat totalWords
                * 10000
                |> floor
                |> toFloat
                |> (\x -> x / 100)

        updateUrl url =
            PushUrl <| "/" ++ url
    in
    row [ width fill, paddingXY 0 10, spacing 10 ] <|
        [ el [] <| text "Home"
        , link [] <| { url = "/known", label = text "Known" }
        , link [] <| { url = "/export", label = text "Export" }
        , el [] <| text "Options"
        , el [] <| html <| Html.select [ onInput updateUrl ] <| map (\x -> Html.option [ value (fromInt x), selected (x == model.surahNumber) ] [ Html.a [ href ("/" ++ fromInt x) ] [ Html.text ("Surah " ++ fromInt x) ] ]) <| List.range 1 144
        , viewSurahName model.surahNumber model.surahs
        , el [ alignRight, colorGreen ] <| text (fromFloat percentage ++ "%")
        , el [ alignRight, colorGreen ] <| text (fromInt <| countKnownAyats model.surahNumber model.surahRoots model.known)
        ]


viewSurahName : Int -> SurahData -> Element Msg
viewSurahName si surahData =
    el [ centerX, colorGreen ] <| text (Maybe.withDefault "" <| Maybe.map first <| Dict.get si surahData)


viewSurah : Model -> Element Msg
viewSurah model =
    column [ height (fill |> maximum contentHeight), width fill, spacing 20, paddingXY 0 10, scrollbarY ]
        (case Dict.get model.surahNumber model.surahs of
            Just surah ->
                (if model.surahNumber /= 9 then
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


viewBasmalah : Element Msg
viewBasmalah =
    el [ centerX, width (fill |> maximum 500), arabicFontSize, Font.family [ Font.typeface "KFGQPC Uthman Taha Naskh" ] ] (text "بِسْمِ اللَّهِ الرَّحْمَٰنِ الرَّحِيم")


viewAyat : Model -> ( Int, String ) -> Element Msg
viewAyat model ( ai, ayatString ) =
    row [ spacingXY 5 20 ] [ lazy printAyatNumber <| ai, printAyat model ( ai, ayatString ) ]


printAyat : Model -> ( Int, String ) -> Element Msg
printAyat model ( ai, ayatString ) =
    let
        ayats : AyatRoots
        ayats =
            getAyatsRoots model.surahNumber model.surahRoots

        tokens : TokenRoots
        tokens =
            getTokenRoots ai ayats

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


printAyatNumber : Int -> Element Msg
printAyatNumber ai =
    el [] <| text (fromInt ai ++ ".")


getAyatsRoots : Index -> SurahRoots -> AyatRoots
getAyatsRoots index (SurahRoots surahs) =
    Dict.get index surahs |> Maybe.withDefault (AyatRoots Dict.empty)


getTokenRoots : Index -> AyatRoots -> TokenRoots
getTokenRoots index (AyatRoots ayats) =
    Dict.get index ayats |> Maybe.withDefault (TokenRoots Dict.empty)


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
                    "no-root-word" -- do we need to use this?


viewWord : Int -> ActiveWordDetails -> Known -> TokenRoots -> Int -> ( Int, String ) -> Element Msg
viewWord surahNumber activeWordDetails known tokens ai ( wi, w ) =
    let
        root =
            getRootFromToken wi tokens

        isKnown =
            isLearned root known

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
    Dict.get index token |> Maybe.withDefault ""


isLearned : Root -> Known -> Progress
isLearned root known =
    Maybe.withDefault Unknown <| Dict.get root known


viewLearnableWord : Root -> Progress -> Element Msg
viewLearnableWord root progress =
    el [ centerX, paddingXY 0 10 ] <|
        Input.radio [ spacing 10 ]
            { onChange = SetKnown root
            , selected = Just progress
            , label = Input.labelAbove [ paddingXY 0 5 ] (text "Set your learning progress on this root:")
            , options =
                [ Input.option Unknown (text "Not Learned")
                , Input.option (Learning) (text "Learning")
                , Input.option (Learned) (text "Learned")
                ]
            }


viewOverlay : Model -> Element Msg
viewOverlay model =
    column [ height (fill |> maximum contentHeight), width (fill |> maximum 600), scrollbarY ] <|
        case model.activeWordDetails of
            Just ( root, loc ) ->
                [ viewSelectedWordInfo model.rootsData root loc
                , viewLearnableWord root (isLearned root model.known)
                , viewOtherWordsWithSameRoot model.rootsData root loc
                ]

            Nothing ->
                [ el [ height fill ] <| text "Click on a word to view its info" ]


viewSelectedWordInfo : RootsData -> String -> Location -> Element Msg
viewSelectedWordInfo rootsData root location =
    let
        filterToActiveWord : List WordInfo -> Maybe WordInfo
        filterToActiveWord =
            head << filter (\wordInfo -> wordInfo.location == location)

        printActiveWordDetails : Maybe WordInfo -> Element Msg
        printActiveWordDetails wordInfoM =
            case wordInfoM of
                Just wordInfo ->
                    column [ width fill, spacingXY 0 10 ]
                        [ el [ arabicFontSize, centerX ] <| text ("(" ++ wordInfo.word ++ " (" ++ root)
                        , el [ centerX ] <| text wordInfo.translation
                        , el [ centerX ] <| text (locationToString wordInfo.location)
                        ]

                Nothing ->
                    none
    in
    case Dict.get root rootsData of
        Just wordsGroups ->
            List.foldl (\w accum -> List.append w.words accum) [] wordsGroups
                |> filterToActiveWord
                |> printActiveWordDetails

        Nothing ->
            none


viewOtherWordsWithSameRoot : RootsData -> String -> Location -> Element Msg
viewOtherWordsWithSameRoot rootsData root location =
    let
        filterOutActiveWord : List WordInfo -> List WordInfo
        filterOutActiveWord =
            filter (\wordInfo -> wordInfo.location /= location)

        printTable : List WordInfo -> List (Element Msg)
        printTable =
            map printWordDetails

        --              |> filterOutActiveWord location
        count : WordsGroup -> String
        count group =
            "(" ++ (fromInt <| length group.words) ++ ")"

        header : Int -> WordsGroup -> Element Msg
        header index group =
            row [ width fill, paddingXY 0 10, pointer, onClick <| ToggleOverlayGroup index root ] [ el [ centerX, Font.bold ] <| text (group.name ++ " " ++ count group) ]

        printGroup : Int -> WordsGroup -> List (Element Msg)
        printGroup index group =
            if group.collapsed == True then
                [ header index group ]

            else
                header index group :: printTable group.words

        rows : List WordsGroup -> List (Element Msg)
        rows wordsGroups =
            concat <| List.indexedMap printGroup wordsGroups
    in
    case Dict.get root rootsData of
        Just wordsGroups ->
            column [ width fill ] <| rows wordsGroups

        Nothing ->
            none


printWordDetails : WordInfo -> Element Msg
printWordDetails wordInfo =
    row [ width fill ]
        [ link [ Font.color <| rgb 0 0 255 ]
            { url = locationToUrl wordInfo.location
            , label = text <| locationToString wordInfo.location
            }
        , lazy (el [ centerX ] << text) wordInfo.translation
        , lazy (el [ alignRight, arabicFontSize ] << text) wordInfo.word
        ]


locationToString : Location -> String
locationToString ( a, b, c ) =
    fromInt a ++ ":" ++ fromInt b ++ ":" ++ fromInt c


locationToUrl : Location -> String
locationToUrl ( a, b, c ) =
    absolute (map fromInt [ a, b, c ]) []


scrollToWord : Location -> Cmd Msg
scrollToWord loc =
    Dom.getElement (locationToUrl loc)
        |> Task.andThen (\info -> Dom.setViewport 0 (info.element.y - (info.viewport.height / 2)))
        --    |>  Dom.getViewportOf id
        --      |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


getKnownAyats : Int -> SurahRoots -> SurahData -> Known -> List ( Index, String )
getKnownAyats si (SurahRoots surahs) surahsData known =
    let
        getAyats : AyatRoots
        getAyats =
            Maybe.withDefault (AyatRoots Dict.empty) <| Dict.get si surahs

        filterKnownAyats : AyatRoots -> AyatRoots
        filterKnownAyats (AyatRoots ayats) =
            AyatRoots <| Dict.filter isKnownAyat ayats

        isKnownAyat : Int -> TokenRoots -> Bool
        isKnownAyat _ (TokenRoots tokens) =
            tokens == Dict.filter isKnownRoot tokens

        isKnownRoot : Int -> Root -> Bool
        isKnownRoot _ root =
            Dict.member root known

        toAyatStrings : AyatRoots -> List ( Index, String )
        toAyatStrings (AyatRoots ayats) =
            filter (\( i, _ ) -> Dict.member i ayats) <| indexBy1 <| List.indexedMap Tuple.pair <| Maybe.withDefault [] <| Maybe.map second <| Dict.get si surahsData
    in
    toAyatStrings <| filterKnownAyats getAyats


surahListWithCompleteAyats : SurahRoots -> Known -> List Int
surahListWithCompleteAyats (SurahRoots surahs) known =
    let
        filterKnownAyats : Index -> AyatRoots -> Bool
        filterKnownAyats i (AyatRoots ayats) =
            (Dict.size <| Dict.filter isKnownAyat ayats) > 0

        -- improve to break on first find
        isKnownAyat : Int -> TokenRoots -> Bool
        isKnownAyat _ (TokenRoots tokens) =
            tokens == Dict.filter isKnownRoot tokens

        isKnownRoot : Int -> Root -> Bool
        isKnownRoot _ root =
            Dict.member root known
    in
    Dict.keys <| Dict.filter filterKnownAyats surahs


countKnownAyats : Int -> SurahRoots -> Known -> Int
countKnownAyats si (SurahRoots surahs) known =
    let
        filterKnownAyats : AyatRoots -> Int
        filterKnownAyats (AyatRoots ayats) =
            Dict.size <| Dict.filter isKnownAyat ayats

        isKnownAyat : Int -> TokenRoots -> Bool
        isKnownAyat _ (TokenRoots tokens) =
            tokens == Dict.filter isKnownRoot tokens

        isKnownRoot : Int -> Root -> Bool
        isKnownRoot _ root =
            Dict.member root known
    in
    List.sum <| map filterKnownAyats <| Dict.values surahs



--  Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)


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
