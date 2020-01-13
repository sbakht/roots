port module Remake exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict, get)
import Element exposing (image, Attr, Element, alignRight, centerX, column, el, fill, height, html, htmlAttribute, link, maximum, minimum, none, padding, paddingXY, paragraph, pointer, rgb, rgb255, row, scrollbarY, spacing, spacingXY, text, width)
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


rootsToLocationsUrl = "http://localhost:8080/roots/"

type alias SurahNumber = Int
type alias AyatNumber = Int
type alias Level = Int


-- Model

type Msg
    = LoadLevel Level (Result Http.Error Surahs)
    | GoNextAyat
    | GoPreviousAyat
    | FetchRoots (Result Http.Error Roots)
    | SetRelatedAyahs Root
    | NoOp


type alias Surahs = List Surah
type alias Surah = { surahNumber : Int, ayats: List Ayah}
type alias Ayahs = List Ayah
type alias Ayah = {
    ayahNumber: Int
    , words: List Word,
    arabic: String,
    translation: String
 }
type alias Words = List Word
type alias Word = {
    wordNumber: Int,
    word : String,
    translation: String,
    root: String,
    location: Location,
    rootId: Int
 }
type alias Location = (Int, Int, Int)
type alias ActiveLocation = Maybe {surahNumber : SurahNumber, ayahNumber: AyatNumber}
type alias ActiveIndex = Maybe Int

type alias Roots = Dict Root Words
type alias Root = String

type alias Related = Maybe Ayahs
type alias Levels = Dict Level Surahs

type alias Model =
    {
        level: Level,
        levels: Levels,
        activeLocation : ActiveLocation,
        activeIndex : ActiveIndex,
        roots : Roots,
        related : Related
    }

----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- -----------------

isLast : (SurahNumber, AyatNumber) -> Surah -> Bool
isLast (x,y) surah = False

lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing

flattenLevels : Level -> Levels -> Ayahs
flattenLevels i levels = flattenSurahs <| getSurahsFromLevel i levels

activeIndexToLocation : ActiveIndex -> Surahs -> ActiveLocation
activeIndexToLocation index surahs =
    let
        activeAyah : Maybe Ayah
        activeAyah = case index of
            Just i ->
                Array.get i (Array.fromList (flattenSurahs surahs))
            Nothing ->
                Nothing

        containsAyah : Ayah -> Ayahs -> Bool
        containsAyah ayah ayahs = (length (filter (\a -> ayah == a) ayahs)) > 0

        activeSurah : Maybe Surah
        activeSurah = case activeAyah of
            Just ayah ->
                head <| filter (\surah -> containsAyah ayah surah.ayats) surahs
            Nothing ->
                Nothing
    in
        case activeSurah of
            Just surah ->
                case activeAyah of
                    Just ayah ->
                        Just {surahNumber = surah.surahNumber, ayahNumber = ayah.ayahNumber}
                    Nothing ->
                        Nothing
            Nothing ->
                Nothing

activeLocationToIndex : ActiveLocation -> Surahs -> ActiveIndex
activeLocationToIndex location surahs =
    let
        activeAyah : Surah -> Maybe Ayah
        activeAyah surah = case location of
            Just {ayahNumber} ->
                head <| filter (\ayah -> ayah.ayahNumber == ayahNumber) surah.ayats
            Nothing ->
                Nothing

        getIndex : Ayah -> Ayahs -> Maybe Int
        getIndex ayah ayahs = head <| map (\(i, _) -> i) <| filter (\(i,a) -> a == ayah) <|List.indexedMap Tuple.pair ayahs

        activeSurah : Maybe Surah
        activeSurah = case location of
            Just {surahNumber} ->
                head <| filter (\surah -> surah.surahNumber == surahNumber) surahs
            Nothing ->
                Nothing
    in
        case activeSurah of
            Just surah ->
                case activeAyah surah of
                    Just ayah ->
                         getIndex ayah (flattenSurahs surahs)
                    Nothing ->
                        Nothing
            Nothing ->
                Nothing

--Pulls all the ayahs into a list from given surahs
flattenSurahs : Surahs -> Ayahs
flattenSurahs = List.foldl (\surah list -> List.append list surah.ayats) []

getAyatsByLevel : Level -> Cmd Msg
getAyatsByLevel level =
    decodeSurahs
        |> Http.get ("http://localhost:8080/surahs/level/" ++ fromInt level)
        |> Http.send (LoadLevel level)

toLocation : List Int -> Location
toLocation l =
    case l of
        a :: b :: c :: _ ->
            ( a, b, c )

        _ ->
            ( 0, 0, 0 )

stringToLoc : String -> Location
stringToLoc s =
    dropLeft 1 s
        |> dropRight 1
        |> split ":"
        |> map (toInt >> M.withDefault 0)
        |> toLocation

decodedWord : Decoder Word
decodedWord =
    Decode.map6 (\a b c d e f -> Word (Maybe.withDefault -1 a) b c d (stringToLoc e) f)
        (field "wordNumber" (Decode.maybe int))
        (field "word" string)
        (field "translation" string)
        (field "root" string)
        (field "location" string)
        (field "rootId" int)

decodeSurahs : Decoder Surahs
decodeSurahs =
    let

        decodedAyats = list decodedAyat
        decodedWords = list decodedWord


        decodedSurah =
            Decode.map2 Surah
                (field "surahNumber" int)
                (field "ayats" decodedAyats)
        decodedAyat =
            Decode.map4 Ayah
                (field "ayatNumber" int)
                (field "words" decodedWords)
                (field "arabic" string)
                (field "translation" string)


    in
     list decodedSurah

fetchRoots : Cmd Msg
fetchRoots = decodeRoots
        |> Http.get ("http://localhost:8080/roots")
        |> Http.send FetchRoots

decodeRoots : Decoder Roots
decodeRoots =
    let

        decodeWords : Decoder Words
        decodeWords = list decodedWord

        joinListsOfWords : Decoder Words
        joinListsOfWords =
           Decode.map List.concat <| list (field "data" decodeWords)

        decodeRoot =
            Decode.map2 (\r w -> (r,w))
                (field "root" string)
                (field "categories" joinListsOfWords)
    in
    Decode.map Dict.fromList <| (list decodeRoot)

----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- -----------------

init : Value -> ( Model, Cmd Msg )
init sessionProgress  =
    ( {
        level = 1,
        levels = Dict.empty,
        activeLocation = Nothing,
        activeIndex = Nothing,
        roots = Dict.empty,
        related = Nothing
    }
    , Cmd.batch [getAyatsByLevel 1, fetchRoots]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    LoadLevel level (Ok surahs) ->
        let
           setActiveLocation : ActiveLocation
           setActiveLocation = case surahs of
               (surah::_) ->
                   case surah.ayats of
                       (ayat::_) ->
                           Just ({ surahNumber = surah.surahNumber, ayahNumber = ayat.ayahNumber})
                       _ ->
                          Nothing

               _ ->
                   Nothing
           newIndex = activeLocationToIndex setActiveLocation surahs
           newLevels = Dict.insert level surahs model.levels
        in
            ({ model | levels = newLevels, activeLocation = setActiveLocation, activeIndex = newIndex, level = level}, Cmd.none)
    LoadLevel _ _ ->
        (model, Cmd.none)
    GoNextAyat ->
        let
           surahs = getSurahsFromLevel model.level model.levels
           nextLevelSurahs = getSurahsFromLevel (model.level + 1) model.levels
           needToFetch = length nextLevelSurahs == 0

           inc : ActiveIndex -> ActiveIndex
           inc = Maybe.map (\i -> i+1)

           newIndex = inc model.activeIndex
           getNewLocation = activeIndexToLocation newIndex surahs

           loadNextLevel = case newIndex of
               Just i ->
                   i > (length <| flattenSurahs surahs)
               Nothing ->
                   False

        in
            if loadNextLevel then
                if needToFetch then
                    ({ model | activeLocation = getNewLocation,  activeIndex = newIndex, related = Nothing}, getAyatsByLevel (model.level + 1))
                else
                    ({ model | activeLocation = activeIndexToLocation (Just 0) nextLevelSurahs,  activeIndex = Just 0, level = model.level + 1, related = Nothing}, Cmd.none)
            else
                ({ model | activeLocation = getNewLocation,  activeIndex = newIndex, related = Nothing}, Cmd.none)
    GoPreviousAyat ->
        let
           surahs = getSurahsFromLevel model.level model.levels
           previousLevelSurahs = getSurahsFromLevel (model.level - 1) model.levels
           needToFetch = length previousLevelSurahs == 0

           dec : ActiveIndex -> ActiveIndex
--           dec = Maybe.map (\i -> if i == 0 then 0 else i-1)
           dec = Maybe.map (\i -> i-1)

           newIndex = dec model.activeIndex
           getNewLocation = activeIndexToLocation newIndex (getSurahsFromLevel model.level model.levels)

           loadPreviousLevel = case newIndex of
               Just i ->
                   i < 0
               Nothing ->
                   False

           lastIndexOfPreviousLevel = Just ((length previousLevelSurahs) - 1)

        in
            if loadPreviousLevel then
                if needToFetch then
                    ({ model | activeLocation = getNewLocation,  activeIndex = newIndex, related = Nothing}, getAyatsByLevel (model.level - 1))
                else
                    ({ model | activeLocation = (activeIndexToLocation lastIndexOfPreviousLevel previousLevelSurahs),  activeIndex = lastIndexOfPreviousLevel, level = model.level - 1, related = Nothing}, Cmd.none)
            else
                ({ model | activeLocation = getNewLocation,  activeIndex = newIndex, related = Nothing}, Cmd.none)
    FetchRoots (Ok roots) ->
        ({model | roots = roots}, Cmd.none)
    FetchRoots _ ->
        (model, Cmd.none)
    SetRelatedAyahs root ->
        ({model | related = Just (relatedAyahs model root)}, Cmd.none)
    _ ->
        (model, Cmd.none)

getSurahsFromLevel : Level -> Dict Level Surahs -> Surahs
getSurahsFromLevel level levels = Maybe.withDefault [] (Dict.get level levels)
----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- -----------------


relatedAyahs : Model -> Root -> Ayahs
relatedAyahs model root =
    let
        currentLevelPastAyahs = List.take (Maybe.withDefault 0 model.activeIndex) <| flattenLevels model.level model.levels
        ayahsToCheck = List.append (concat <| map (\i -> flattenLevels i model.levels) <| List.range 1 (model.level - 1)) currentLevelPastAyahs

        hasRoot r word = word.root == r
        isRelatedAyah ayah = length (filter (hasRoot root) ayah.words) > 0
    in
        filter isRelatedAyah ayahsToCheck

hasRelatedAyahs model root = length (relatedAyahs model root) > 0

getActiveAyah : Model -> Maybe Ayah
getActiveAyah model =
    case model.activeIndex of
        Just i ->
            Array.get i (Array.fromList (flattenSurahs (getSurahsFromLevel model.level model.levels)))
        Nothing ->
            Nothing

ayahImage : SurahNumber -> AyatNumber -> Element Msg
ayahImage x y = image [] {
            src = "http://www.everyayah.com/data/images_png/" ++ fromInt x ++ "_" ++ fromInt y ++ ".png",
            description = "ayat"
        }

formatWords : Ayah -> List (String, Maybe Word)
formatWords {arabic, words} =
    let
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
        dict = List.foldr (\ word arr -> Dict.insert word.wordNumber word arr) Dict.empty words

        joinedYaa : List String -> List String
        joinedYaa str = str
--        joinedYaa =
--            List.foldr addWhenYaa []
    in
        Debug.log "hmm" <| List.indexedMap (\i str -> (str, Dict.get (i+1) dict)) (joinedYaa <| String.split " " arabic)

viewAyah : Ayah -> Model -> Element Msg
viewAyah ayah model = (map (viewWord model) (formatWords ayah))
        |> Element.paragraph [ Font.size 30, spacingXY 0 20, Font.family [ Font.typeface "KFGQPC Uthman Taha Naskh" ] ]

viewWordByWord : Ayah -> Element Msg
viewWordByWord ayah = column [] <| map (\word ->
    row [] <| [el [] <| text word.word,
               text " - ",
               el [] <| text word.translation
               ]) ayah.words

viewActiveAyah : Model -> Element Msg
viewActiveAyah model = case getActiveAyah model of
    Just ayah ->
        column [] <| [viewAyah ayah model,
                      viewTranslation ayah,
                      viewWordByWord ayah]
    Nothing ->
        text ""

viewRelated : Maybe Ayahs -> Element Msg
viewRelated related =
    case related of
        Just ayahs ->
            column [] <| map (\ayah -> column [] <| [el [] <| text ayah.arabic, viewTranslation ayah, viewWordByWord ayah]) ayahs
        Nothing ->
            text ""

viewTranslation : Ayah -> Element Msg
viewTranslation ayah = el [] (text ayah.translation)

viewWord : Model -> (String, Maybe Word) -> Element Msg
viewWord model (arabicStr, mbWord) =
    case mbWord of
        Just word ->
            if hasRelatedAyahs model word.root then
                el [pointer, Font.bold, Font.color (rgb 0 0 255) , onClick (SetRelatedAyahs word.root)] <| text (word.word ++ " " ++ "")
            else
                el [] <| text (word.word ++ " " ++ "")
        Nothing ->
            el [] <| text (arabicStr ++ " " ++ "")

getSurahNum model = case model.activeLocation of
    Just {surahNumber} ->
        surahNumber
    Nothing ->
        0
getAyahNum model = case model.activeLocation of
    Just {ayahNumber} ->
        ayahNumber
    Nothing ->
        0

view : Model -> Browser.Document Msg
view model =
    { title = "Learn Quran Roots"
    , body =
        [ Element.layout [ paddingXY 10 0 ] <|
            column [ width fill ]
                [
                    el [] <| text "Home",
--                    ayahImage (getSurahNum model) (getAyahNum model),
                    viewActiveAyah model,
                    viewRelated model.related,
                    el [onClick GoNextAyat] <| text "Next",
                    el [onClick GoPreviousAyat] <| text "Previous"
                    ]
        ]
    }

main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
