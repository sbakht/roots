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

type alias Roots = Dict String Words

type alias Model =
    {
        level: Level,
        levels: Dict Level Surahs,
        activeLocation : ActiveLocation,
        activeIndex : ActiveIndex,
        roots : Roots
    }

----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- -----------------

isLast : (SurahNumber, AyatNumber) -> Surah -> Bool
isLast (x,y) surah = False

lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing

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
        roots = Dict.empty
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
                    ({ model | activeLocation = getNewLocation,  activeIndex = newIndex}, getAyatsByLevel (model.level + 1))
                else
                    ({ model | activeLocation = activeIndexToLocation (Just 0) nextLevelSurahs,  activeIndex = Just 0, level = model.level + 1}, Cmd.none)
            else
                ({ model | activeLocation = getNewLocation,  activeIndex = newIndex}, Cmd.none)
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
                    ({ model | activeLocation = getNewLocation,  activeIndex = newIndex}, getAyatsByLevel (model.level - 1))
                else
                    ({ model | activeLocation = (activeIndexToLocation lastIndexOfPreviousLevel previousLevelSurahs),  activeIndex = lastIndexOfPreviousLevel, level = model.level - 1}, Cmd.none)
            else
                ({ model | activeLocation = getNewLocation,  activeIndex = newIndex}, Cmd.none)
    FetchRoots (Ok roots) ->
        ({model | roots = roots}, Cmd.none)
    FetchRoots _ ->
        (model, Cmd.none)
    _ ->
        (model, Cmd.none)

getSurahsFromLevel : Level -> Dict Level Surahs -> Surahs
getSurahsFromLevel level levels = Maybe.withDefault [] (Dict.get level levels)
----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- ----------------- -----------------

ayahImage : SurahNumber -> AyatNumber -> Element msg
ayahImage x y = image [] {
            src = "http://www.everyayah.com/data/images_png/" ++ fromInt x ++ "_" ++ fromInt y ++ ".png",
            description = "ayat"
        }

view : Model -> Browser.Document Msg
view model =
    { title = "Learn Quran Roots"
    , body =
        [ Element.layout [ paddingXY 10 0 ] <|
            column [ width fill ]
                [
                    el [] <| text "Home",
                    ayahImage 1 2,
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
