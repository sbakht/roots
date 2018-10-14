module Main exposing (main)

import Array exposing (Array)
import Browser
import Task
import Browser.Navigation as Nav
import Browser.Dom as Dom
import Dict exposing (Dict)
import EncodeString exposing (encode)
import Html exposing (Html, a, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (checked, class, classList, for, href, id, name, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import List exposing (drop, filter, head, map)
import String exposing (dropLeft, dropRight, fromChar, fromInt, split, toInt)
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
    | Null


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Null top
        , UrlParser.map SurahPage int
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
    Decode.keyValuePairs
        (Decode.map4
            mkWordInfo
            (field "location" string)
            (field "transliteration" string)
            (field "translation" string)
            (field "word" string)
            |> list
        )
        |> Decode.map Dict.fromList


mkWordInfo : String -> String -> String -> String -> WordInfo
mkWordInfo a b c d =
    WordInfo (stringToLoc a) b c d


decodeSurah : Int -> SurahData -> Decoder SurahData
decodeSurah surahNum surahData =
    Decode.keyValuePairs string
        |> field "verse"
        |> Decode.map (map second)
        |> Decode.map (mkSurahData surahNum surahData)


mkSurahData : Int -> SurahData -> List String -> SurahData
mkSurahData surahNum surahData listOfSurahRoots =
    Dict.insert surahNum (formatSurahText listOfSurahRoots) surahData


formatSurahText : List String -> Array String
formatSurahText =
    dropBasmalah >> Array.fromList


dropBasmalah : List String -> List String
dropBasmalah =
    drop 1


stringToLoc : String -> Location
stringToLoc s =
    dropLeft 1 s
        |> dropRight 1
        |> split ":"
        |> map (toInt >> Maybe.withDefault 0)
        |> toTuple3


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
    Dict Int (Array String)


type alias RootsData =
    Dict Root (List WordInfo)


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
        stuff : Root -> List WordInfo -> SurahRoots -> SurahRoots
        stuff root locs dic =
            List.foldl (moreStuff root) dic locs

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
                            Debug.log "word collison" (SurahRoots surahs)

                        Nothing ->
                            SurahRoots (Dict.insert si (insert ayats tokens) surahs)

                Nothing ->
                    SurahRoots (Dict.insert si (newAyat ayats) surahs)

        Nothing ->
            newSurah


type Msg
    = SetKnown Root
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
            (model, Cmd.none)
        SetKnown root ->
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
                ( { model | activeWordDetails = Just ( root, loc ) }, scrollToWord)

        LoadSurah (Ok surahData) ->
            if Dict.isEmpty model.rootsData then
                ( { model | surahs = surahData }, wordsCmd )

            else
                ( { model | surahs = surahData }, Cmd.none )

        LoadSurah _ ->
            ( model, wordsCmd )

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
                        ( { model | activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, Cmd.none )

                    else
                        ( { model | surahNumber = surahNum, activeWordDetails = Just ( root, ( surahNum, ai, wi ) ) }, surahCmd surahNum model.surahs )

                Null ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Learn Quran Roots"
    , body =
        [ div []
            [ a [ href "/1" ] [ text "load surah" ]
            , a [ href "/2" ] [ text "load surah" ]
            , viewOverlay model
            , viewContent model
            ]
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div [ class "content" ] [ viewSurah model ]


viewSurah : Model -> Html Msg
viewSurah model =
    Dict.get model.surahNumber model.surahs
        |> Maybe.withDefault Array.empty
        |> Array.toIndexedList
        |> indexBy1
        |> map (viewAyat model)
        |> div []


viewAyat : Model -> ( Int, String ) -> Html Msg
viewAyat model ( ai, ayatString ) =
    p [] (printAyat model ( ai, ayatString ) ++ [ printAyatNumber ai ])


printAyat : Model -> ( Int, String ) -> List (Html Msg)
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


printAyatNumber : Int -> Html Msg
printAyatNumber ai =
    span [] [ text (fromInt ai) ]


getAyatsRoots : Index -> SurahRoots -> Ayats
getAyatsRoots index (SurahRoots surahs) =
    Dict.get index surahs |> Maybe.withDefault (Ayats Dict.empty)


getTokenRoots : Index -> Ayats -> Tokens
getTokenRoots index (Ayats ayats) =
    Dict.get index ayats |> Maybe.withDefault (Tokens Dict.empty)


addWhenYaa : String -> List String -> List String
addWhenYaa curr accum =
    if curr == encode then
        case accum of
            x :: xs ->
                (encode ++ " " ++ x) :: xs

            _ ->
                curr :: accum

    else
        curr :: accum


indexBy1 : List ( Int, String ) -> List ( Int, String )
indexBy1 =
    map (\( i, s ) -> ( i + 1, s ))


viewWord : Model -> Tokens -> Int -> ( Int, String ) -> Html Msg
viewWord model tokens ai ( wi, w ) =
    let
        root =
            getRootFromToken wi tokens

        isKnown =
            isLearned root model.known

        isLearnable =
            root /= ""
    in
    a
        [ classList
            [ ( "known", isKnown )
            , ( "learnable", isLearnable )
            ]
            , id (locationToUrl ( model.surahNumber, ai, wi ))
        , href (locationToUrl ( model.surahNumber, ai, wi ))
        ]
        [ text (w ++ " " ++ "") ]


getRootFromToken : Index -> Tokens -> Root
getRootFromToken index (Tokens token) =
    Dict.get index token |> Maybe.withDefault ""


viewLearnables : Model -> Html Msg
viewLearnables model =
    ul []
        (map
            (\w ->
                viewLearnableWord w (isLearned w model.known)
            )
            (Dict.keys model.rootsData)
        )


isLearned : Root -> Known -> Bool
isLearned root known =
    Dict.member root known


viewLearnableWord : Root -> Bool -> Html Msg
viewLearnableWord root learned =
    li []
        [ label []
            [ input
                [ type_ "checkbox"
                , onClick (SetKnown root)
                , checked learned
                ]
                []
            , text root
            ]
        ]


viewOverlay : Model -> Html Msg
viewOverlay model =
    case model.activeWordDetails of
        Just ( root, loc ) ->
            div [ id "drawer" ]
                [ viewSelectedWordInfo model.rootsData root loc
                , viewLearnableWord root (isLearned root model.known)
                , viewOtherWordsWithSameRoot model.rootsData root loc
                ]

        Nothing ->
            text ""


viewSelectedWordInfo : RootsData -> String -> Location -> Html Msg
viewSelectedWordInfo rootsData root location =
    case Dict.get root rootsData of
        Just wordsInfo ->
            div [ id "active-word" ]
                [ wordsInfo
                    |> filterToActiveWord location
                    |> printActiveWordDetails root
                ]

        Nothing ->
            text ""


filterToActiveWord : Location -> List WordInfo -> Maybe WordInfo
filterToActiveWord location =
    head << filter (\wordInfo -> wordInfo.location == location)


printActiveWordDetails : Root -> Maybe WordInfo -> Html Msg
printActiveWordDetails root wordInfoM =
    case wordInfoM of
        Just wordInfo ->
            div []
                [ p [ id "drawer-word" ] [ text ("(" ++ wordInfo.word ++ " (" ++ root) ]
                , p [ id "drawer-translation" ] [ text wordInfo.translation ]
                , p [ id "drawer-location" ] [ text (locationToString wordInfo.location) ]
                ]

        Nothing ->
            text ""


viewOtherWordsWithSameRoot : RootsData -> String -> Location -> Html Msg
viewOtherWordsWithSameRoot rootsData root location =
    case Dict.get root rootsData of
        Just wordsInfo ->
            div []
                (wordsInfo
                    |> filterOutActiveWord location
                    |> map printWordDetails
                )

        Nothing ->
            text ""


filterOutActiveWord : Location -> List WordInfo -> List WordInfo
filterOutActiveWord location =
    filter (\wordInfo -> wordInfo.location /= location)


printWordDetails : WordInfo -> Html Msg
printWordDetails wordInfo =
    div [ class "other-words" ]
        [ a [ href (locationToUrl wordInfo.location) ] [ text (locationToString wordInfo.location) ]
        , span [] [ text wordInfo.translation ]
        , span [] [ text wordInfo.word ]
        ]


locationToString : Location -> String
locationToString ( a, b, c ) =
    fromInt a ++ ":" ++ fromInt b ++ ":" ++ fromInt c


locationToUrl : Location -> String
locationToUrl ( a, b, c ) =
    absolute (map fromInt [ a, b, c ]) []


scrollToWord : Cmd Msg
scrollToWord =
    Dom.getElement "/36/70/3"
    |> Task.andThen (\info -> Dom.setViewport 0 info.element.y)
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
