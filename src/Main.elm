module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Attr, Element, alignRight, centerX, column, el, fill, fillPortion, height, link, maximum, mouseOver, none, padding, paddingXY, paragraph, rgb, rgb255, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import EncodeString exposing (encode)
import Html.Attributes exposing (checked, class, classList, for, href, id, name, type_)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
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
    | Null


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Null top
        , UrlParser.map SurahPage int
        , UrlParser.map KnownPage <| s "known"
        , UrlParser.map (\a b c -> RootModal ( a, b, c )) (int </> int </> int)
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Just url ->
            Maybe.withDefault Null (parse route url)

        Nothing ->
            Null


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
                |> Decode.map2 WordsGroup (field "name" string)
                |> list
    in
    Decode.map Dict.fromList <| Decode.keyValuePairs dataDecode


decodeSurah : Int -> SurahData -> Decoder SurahData
decodeSurah surahNum surahData =
    Decode.keyValuePairs string
        |> field "verse"
        |> Decode.map (map second)
        |> Decode.map (mkSurahData surahNum surahData)


mkSurahData : Int -> SurahData -> List String -> SurahData
mkSurahData surahNum surahData listOfSurahRoots =
    Dict.insert surahNum (formatSurahText listOfSurahRoots) surahData


formatSurahText : List String -> List String
formatSurahText =
    dropBasmalah


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
    "https://raw.githubusercontent.com/semarketir/quranjson/master/source/surah/surah_"


getSurahRequestUrl : Int -> String
getSurahRequestUrl surahNumber =
    surahsUrl ++ fromInt surahNumber ++ ".json"


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



-- Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , surahs : SurahData
    , surahRoots : SurahRoots
    , rootsData : RootsData
    , known : Known
    , surahNumber : Int
    , activeWordDetails : Maybe ( String, Location )
    , page : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , surahs = Dict.empty
      , surahRoots = SurahRoots Dict.empty
      , rootsData = Dict.empty
      , known = Dict.empty
      , surahNumber = tempSurahNumber
      , activeWordDetails = Nothing
      , page = "Home"
      }
    , surahCmd tempSurahNumber Dict.empty
    )


type SurahRoots
    = SurahRoots (Dict Index Ayats)


type Ayats
    = Ayats (Dict Index Tokens)


type Tokens
    = Tokens (Dict Index Root)


type alias SurahData =
    Dict Int (List String)


type alias RootsData =
    Dict Root (List WordsGroup)


type alias WordsGroup =
    { name : String, words : List WordInfo }


type alias WordInfo =
    { location : Location, transliteration : String, translation : String, word : String }


type alias Root =
    String


type alias Index =
    Int


type alias Location =
    ( Int, Int, Int )


type alias Known =
    Dict String ()


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


type Msg
    = SetKnown Root Bool
    | LoadSurah (Result Http.Error SurahData)
    | LoadRootsData (Result Http.Error RootsData)
    | SetActiveDetails ( Root, Location )
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetKnown root isKnown ->
            let
                learn : Root -> Dict String () -> Dict String ()
                learn rootLetters learned =
                    Dict.insert rootLetters () learned

                forget : Root -> Dict String () -> Dict String ()
                forget rootLetters learned =
                    Dict.remove rootLetters learned
            in
            if root == "" then
                ( model, Cmd.none )

            else if isLearned root model.known then
                ( { model | known = forget root model.known }, Cmd.none )

            else
                ( { model | known = learn root model.known }, Cmd.none )

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

                Null ->
                    ( model, Cmd.none )


overlaySize =
    1


surahSize =
    5


englishFontSize =
    Font.size 14


arabicFontSize =
    Font.size 30


pageBackground =
    Background.color <| rgb255 244 241 222


learnedColor =
    Font.color <| rgb255 61 64 91


notLearnedColor =
    Font.color <| rgb255 224 122 95


hoverBackground =
    Background.color <| rgb255 242 204 143


hoverClickedBackground =
    Background.color <| rgb255 242 204 143


view : Model -> Browser.Document Msg
view model =
    { title = "Learn Quran Roots"
    , body =
        [ Element.layout [ paddingXY 10 0, pageBackground ] <|
            column []
                [ viewHeader model
                , case model.page of
                    "Known" ->
                        viewKnown model

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
    in
    row [ width fill, paddingXY 0 10, spacing 10 ] <|
        [ el [] <| text "Home"
        , link [] <| { url = "/known", label = text "Known" }
        , el [] <| text "Options"
        , el [ alignRight, Font.color <| rgb 0 255 0 ] <| text (fromFloat percentage ++ "%")
        ]


viewSurah : Model -> Element Msg
viewSurah model =
    column [ height fill, width fill, spacing 20 ]
        (Dict.get model.surahNumber model.surahs
            |> Maybe.withDefault []
            |> List.indexedMap Tuple.pair
            |> indexBy1
            |> map (viewAyat model)
        )


viewAyat : Model -> ( Int, String ) -> Element Msg
viewAyat model ( ai, ayatString ) =
    row [ spacing 5 ] [ printAyatNumber ai, printAyat model ( ai, ayatString ) ]


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
        |> map (viewWord model tokens ai)
        |> Element.paragraph [ arabicFontSize ]


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


indexBy1 : List ( Int, String ) -> List ( Int, String )
indexBy1 =
    map (\( i, s ) -> ( i + 1, s ))


wordColor : Bool -> Bool -> Attr decorative Msg
wordColor isKnown isLearnable =
    case ( isKnown, isLearnable ) of
        ( True, _ ) ->
            learnedColor

        ( _, True ) ->
            notLearnedColor

        ( _, _ ) ->
            Font.color <| Element.rgb 0 0 0


viewWord : Model -> Tokens -> Int -> ( Int, String ) -> Element Msg
viewWord model tokens ai ( wi, w ) =
    let
        root =
            getRootFromToken wi tokens

        isKnown =
            isLearned root model.known

        isLearnable =
            root /= ""

        path =
            locationToUrl ( model.surahNumber, ai, wi )

        backgroundColor =
            case model.activeWordDetails of
                Just ( r, ( a, b, c ) ) ->
                    if b == ai && c == wi then
                        hoverClickedBackground

                    else if r == root then
                        hoverBackground

                    else
                        pageBackground

                _ ->
                    pageBackground
    in
    if isLearnable == True then
        Element.link [ wordColor isKnown isLearnable, mouseOver [ hoverClickedBackground ], backgroundColor, padding 5 ] <|
            { url = path, label = text (w ++ " " ++ "") }

    else
        el [ padding 2 ] <| text (w ++ " " ++ "")


getRootFromToken : Index -> Tokens -> Root
getRootFromToken index (Tokens token) =
    Dict.get index token |> Maybe.withDefault ""


isLearned : Root -> Known -> Bool
isLearned root known =
    Dict.member root known


viewLearnableWord : Root -> Bool -> Element Msg
viewLearnableWord root learned =
    el [ centerX ] <|
        Input.checkbox []
            { onChange = SetKnown root
            , icon =
                \checked ->
                    if checked then
                        text "YES"

                    else
                        text "NO"
            , checked = learned
            , label = Input.labelRight [] (text "Learned")
            }


viewOverlay : Model -> Element Msg
viewOverlay model =
    column [ height fill, width (fill |> maximum 600) ] <|
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
                    column [ width fill ]
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
    case Dict.get root rootsData of
        Just wordsGroups ->
            column [ width fill ]
                (List.foldl (\w accum -> List.append w.words accum) [] wordsGroups
                    |> filterOutActiveWord location
                    |> map printWordDetails
                )

        Nothing ->
            none


filterOutActiveWord : Location -> List WordInfo -> List WordInfo
filterOutActiveWord location =
    filter (\wordInfo -> wordInfo.location /= location)


printWordDetails : WordInfo -> Element Msg
printWordDetails wordInfo =
    row [ width fill ]
        [ link [ Font.color <| rgb 0 0 255 ]
            { url = locationToUrl wordInfo.location
            , label = text <| locationToString wordInfo.location
            }
        , el [ centerX ] <| text wordInfo.translation
        , el [ alignRight, arabicFontSize ] <| text wordInfo.word
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



--  Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
