port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Attr, Element, alignRight, centerX, column, el, fill, fillPortion, height, html, htmlAttribute, link, maximum, mouseOver, none, padding, paddingXY, paragraph, pointer, rgb, rgb255, row, scrollbarY, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy3)
import EncodeString exposing (encode)
import Html
import Html.Attributes exposing (checked, class, classList, for, href, id, name, type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, Value, field, int, list, string)
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
   basmalah is ayat on first surah
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


decodeTranslations : Decoder (List (List String))
decodeTranslations =
    field "data" <| field "surahs" <| list <| field "ayahs" <| list <| field "text" string


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
    --    Decode.keyValuePairs string
    --        |> field "verse"
    --        |> Decode.map (map second)
    --        |> Decode.map (mkSurahData surahNum surahData)
    (field "data" <| field "ayahs" <| list <| field "text" string)
        |> Decode.map (mkSurahData surahNum surahData)


decodeAllSurahs : Decoder SurahData
decodeAllSurahs =
    (field "data" <| field "surahs" <| list <| field "ayahs" <| list <| field "text" string)
        |> Decode.map (\x -> List.foldl (\( surahNum, strArr ) surahData -> mkSurahData surahNum surahData strArr) Dict.empty <| indexBy1 <| List.indexedMap Tuple.pair x)


mkSurahData : Int -> SurahData -> List String -> SurahData
mkSurahData surahNum surahData listOfSurahRoots =
    Dict.insert surahNum (formatSurahText listOfSurahRoots) surahData


formatSurahText : List String -> List String
formatSurahText arr =
    let
        removeBas : String -> List String
        removeBas x =
            String.split "بِسْمِ اللَّهِ الرَّحْمَٰنِ الرَّحِيمِ " x
    in
    List.indexedMap
        (\i x ->
            if i == 0 then
                Maybe.withDefault "" <| head <| drop 1 <| removeBas x

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
    , activeWordDetails : ActiveWordDetails
    , page : String
    , translations : SurahData
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
      , activeWordDetails = Nothing
      , page = "Home"
      , translations = Dict.empty
      }
    , surahCmd tempSurahNumber Dict.empty
    )


type SurahRoots
    = SurahRoots (Dict Index Ayats)


type Ayats
    = Ayats (Dict Index Tokens)


type Tokens
    = Tokens (Dict Index Root)


type Progress
    = Learning
    | Learned


type alias SurahData =
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
            Ayats (Dict.insert ai (newToken Dict.empty) ayats)

        newToken tokens =
            Tokens (Dict.insert wi root tokens)

        insert : Dict Index Tokens -> Dict Index Root -> Ayats
        insert ayats tokens =
            Ayats (Dict.insert ai (newToken tokens) ayats)
    in
    case Dict.get si surahs of
        Just (Ayats ayats) ->
            case Dict.get ai ayats of
                Just (Tokens tokens) ->
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
    = SetKnown Root (Maybe Progress)
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
                    Just Learned ->
                        ( { model | known = learn root Learned model.known }, cmd <| learn root Learned model.known )

                    Just Learning ->
                        ( { model | known = learn root Learning model.known }, cmd <| learn root Learning model.known )

                    Nothing ->
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
                        ayats : Ayats
                        ayats =
                            getAyatsRoots surahNum model.surahRoots

                        tokens : Tokens
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
            Maybe.withDefault "" <| Array.get (ai - 1) <| Array.fromList <| translationBySurah surahIndex

        outputPerSurah : Index -> List ( Index, String ) -> List (Element Msg)
        outputPerSurah surahIndex list =
            map (\( ai, ayat ) -> paragraph [] (List.intersperse (text "\t") [ text ayat, text (translation surahIndex ai), text (fromInt ai), text ("Surah" ++ fromInt surahIndex) ])) list
    in
    column [] <| concat <| map (\i -> outputPerSurah i (surahData i)) <| List.range 1 144


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

        updateUrl url = PushUrl <| "/" ++ url
    in
    row [ width fill, paddingXY 0 10, spacing 10 ] <|
        [ el [] <| text "Home"
        , link [] <| { url = "/known", label = text "Known" }
        , link [] <| { url = "/export", label = text "Export" }
        , el [] <| text "Options"
        , el [  ] <| html <| Html.select [onInput updateUrl] <| map (\x -> Html.option [value (fromInt x)] [Html.a [ href ("/" ++ fromInt x)] [Html.text ("Surah " ++ fromInt x)]]) <| List.range 1 144
        , el [ alignRight, Font.color <| rgb 0 255 0 ] <| text (fromFloat percentage ++ "%")
        , el [ alignRight, Font.color <| rgb 0 255 0 ] <| text (fromInt <| countKnownAyats model.surahNumber model.surahRoots model.known)
        ]


viewSurah : Model -> Element Msg
viewSurah model =
    column [ height (fill |> maximum contentHeight), width fill, spacing 20, scrollbarY ]
        (Dict.get model.surahNumber model.surahs
            |> Maybe.withDefault []
            |> List.indexedMap Tuple.pair
            |> indexBy1
            |> map (viewAyat model)
        )


viewAyat : Model -> ( Int, String ) -> Element Msg
viewAyat model ( ai, ayatString ) =
    row [ spacing 5 ] [ lazy printAyatNumber <| ai, printAyat model ( ai, ayatString ) ]


printAyat : Model -> ( Int, String ) -> Element Msg
printAyat model ( ai, ayatString ) =
    let
        ayats : Ayats
        ayats =
            getAyatsRoots model.surahNumber model.surahRoots

        tokens : Tokens
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
        |> Element.paragraph [ arabicFontSize, Font.family [ Font.typeface "KFGQPC Uthman Taha Naskh" ] ]


printAyatNumber : Int -> Element Msg
printAyatNumber ai =
    el [] <| text (fromInt ai ++ ".")


getAyatsRoots : Index -> SurahRoots -> Ayats
getAyatsRoots index (SurahRoots surahs) =
    Dict.get index surahs |> Maybe.withDefault (Ayats Dict.empty)


getTokenRoots : Index -> Ayats -> Tokens
getTokenRoots index (Ayats ayats) =
    Dict.get index ayats |> Maybe.withDefault (Tokens Dict.empty)


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


wordColor : Maybe Progress -> Bool -> Element.Attribute Msg
wordColor isKnown isLearnable =
    htmlAttribute <| class <|
        case ( isKnown, isLearnable ) of
            ( Just Learned, _ ) ->
                "learned-word"

            ( Just Learning, True ) ->
                "learning-word"

            ( Nothing, True ) ->
                "unknown-word"

            ( _, _ ) ->
                "no-root-word"


viewWord : Int -> ActiveWordDetails -> Known -> Tokens -> Int -> ( Int, String ) -> Element Msg
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
            htmlAttribute <| class <|
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
                Element.link [ wordColor knowned learnable,  bgColor, padding 5 ] <|
                    { url = path, label = text (w ++ " " ++ "") }

            else
                el [ padding 2 ] <| text (w ++ " " ++ "")
    in
    lazy3 stuff isKnown isLearnable backgroundColor


getRootFromToken : Index -> Tokens -> Root
getRootFromToken index (Tokens token) =
    Dict.get index token |> Maybe.withDefault ""


isLearned : Root -> Known -> Maybe Progress
isLearned root known =
    Dict.get root known


viewLearnableWord : Root -> Maybe Progress -> Element Msg
viewLearnableWord root progress =
    el [ centerX, paddingXY 0 10 ] <|
        Input.radio [ spacing 10 ]
            { onChange = SetKnown root
            , selected = Just progress
            , label = Input.labelAbove [ paddingXY 0 5 ] (text "Set your learning progress on this root:")
            , options =
                [ Input.option Nothing (text "Not Learned")
                , Input.option (Just Learning) (text "Learning")
                , Input.option (Just Learned) (text "Learned")
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
        getAyats : Ayats
        getAyats =
            Maybe.withDefault (Ayats Dict.empty) <| Dict.get si surahs

        filterKnownAyats : Ayats -> Ayats
        filterKnownAyats (Ayats ayats) =
            Ayats <| Dict.filter isKnownAyat ayats

        isKnownAyat : Int -> Tokens -> Bool
        isKnownAyat _ (Tokens tokens) =
            tokens == Dict.filter isKnownRoot tokens

        isKnownRoot : Int -> Root -> Bool
        isKnownRoot _ root =
            Dict.member root known

        toAyatStrings : Ayats -> List ( Index, String )
        toAyatStrings (Ayats ayats) =
            filter (\( i, _ ) -> Dict.member i ayats) <| indexBy1 <| List.indexedMap Tuple.pair <| Maybe.withDefault [] (Dict.get si surahsData)
    in
    toAyatStrings <| filterKnownAyats getAyats


surahListWithCompleteAyats : SurahRoots -> Known -> List Int
surahListWithCompleteAyats (SurahRoots surahs) known =
    let
        filterKnownAyats : Index -> Ayats -> Bool
        filterKnownAyats i (Ayats ayats) =
            (Dict.size <| Dict.filter isKnownAyat ayats) > 0

        -- improve to break on first find
        isKnownAyat : Int -> Tokens -> Bool
        isKnownAyat _ (Tokens tokens) =
            tokens == Dict.filter isKnownRoot tokens

        isKnownRoot : Int -> Root -> Bool
        isKnownRoot _ root =
            Dict.member root known
    in
    Dict.keys <| Dict.filter filterKnownAyats surahs


countKnownAyats : Int -> SurahRoots -> Known -> Int
countKnownAyats si (SurahRoots surahs) known =
    let
        filterKnownAyats : Ayats -> Int
        filterKnownAyats (Ayats ayats) =
            Dict.size <| Dict.filter isKnownAyat ayats

        isKnownAyat : Int -> Tokens -> Bool
        isKnownAyat _ (Tokens tokens) =
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
