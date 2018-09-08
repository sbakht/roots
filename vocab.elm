module Vocab exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (checked, classList, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import List exposing (drop, map, filter)
import String exposing (dropLeft, dropRight, fromChar, fromInt, split, toInt)
import Tuple exposing (mapFirst, mapSecond, second)



--type alias Model =


nUM =
   36 


testSurahsData : SurahData
testSurahsData =
    Dict.fromList [ ( 2, Array.fromList [ "a b c d", "cool c d a e" ] ) ]


wordsLocs : WordsLocations
wordsLocs =
    Dict.fromList
        [ ( ( 'a', 'a', 'a' ), [ ( 2, 1, 1 ), ( 2, 2, 4 ) ] )
        , ( ( 'b', 'b', 'b' ), [ ( 2, 1, 2 ), ( 1, 2, 1 ) ] )
        , ( ( 'c', 'c', 'c' ), [ ( 2, 1, 3 ), ( 1, 2, 2 ) ] )
        , ( ( 'd', 'd', 'd' ), [ ( 2, 1, 4 ), ( 1, 2, 3 ) ] )
        , ( ( 'e', 'e', 'e' ), [ ( 2, 2, 5 ) ] )
        ]


decodeLocations : Decoder WordsLocations
decodeLocations =
    Decode.keyValuePairs (list string) |> Decode.map (toRoots >> Dict.fromList)


decodeSurah : Decoder SurahData
decodeSurah =
    field "verse" (Decode.keyValuePairs string) |> Decode.map (map second) |> myDic


myDic : Decoder (List String) -> Decoder SurahData
myDic deco =
    Decode.map (\list -> Dict.insert nUM (Array.fromList (drop 1 list)) Dict.empty) deco


toRoots : List ( String, List String ) -> List ( ( Char, Char, Char ), List Location )
toRoots list =
    map (\( s, l ) -> ( toRoot s, map toLocs l )) list


toRoot : String -> ( Char, Char, Char )
toRoot s =
    Debug.log "root" s |> String.toList |> filter (\c -> c /= ' ') |> Array.fromList |> (\arr -> ( Array.get 0 arr, Array.get 1 arr, Array.get 2 arr )) |> justit


justit ( a, b, c ) =
    ( Maybe.withDefault ' ' a, Maybe.withDefault ' ' b, Maybe.withDefault ' ' c )


toLocs : String -> Location
toLocs s =
    dropLeft 1 s |> dropRight 1 |> split ":" |> map (toInt >> Maybe.withDefault 0) |> toTuple3


toTuple3 : List Int -> Location
toTuple3 l =
    case l of
        a :: b :: c :: _ ->
            ( a, b, c )

        _ ->
            ( 0, 0, 0 )


wordsCmd : Cmd Msg
wordsCmd =
    decodeLocations
        |> Http.get "https://sbakht.github.io/corpus-2.0/output.json"
        |> Http.send LoadWordLocations


surahCmd : Cmd Msg
surahCmd =
    decodeSurah
        |> Http.get (getSurahRequestUrl nUM)
        |> Http.send LoadSurah


init : () -> ( Model, Cmd Msg )
init _ =
    ( { surahs = testSurahsData
      , locationsWord = toRootsBySurah wordsLocs
      , wordsLocations = wordsLocs
      , known = Dict.empty
      , surahNumber = nUM
      }
    , surahCmd
    )


type alias Model =
    { surahs : Dict Int (Array String), locationsWord : Surahs, wordsLocations : Dict ( Char, Char, Char ) (List Location), known : Known, surahNumber : Int }


type Surahs
    = Surahs (Dict Index Ayats)


type Ayats
    = Ayats (Dict Index Tokens)


type Tokens
    = Tokens (Dict Index Root)


type alias SurahData =
    Dict Int (Array String)


type alias WordsLocations =
    Dict ( Char, Char, Char ) (List Location)


type Root
    = Root ( Char, Char, Char )


type alias Index =
    Int


type alias Location =
    ( Int, Int, Int )


type alias Known =
    Dict ( Char, Char, Char ) ()


getSurahRequestUrl : Int -> String
getSurahRequestUrl surahNumber =
    "https://raw.githubusercontent.com/semarketir/quranjson/master/source/surah/surah_" ++ fromInt surahNumber ++ ".json"


wordToTokens : ( Char, Char, Char ) -> Location -> Tokens -> Tokens
wordToTokens root ( si, ai, wi ) (Tokens tokens) =
    Tokens <| Dict.insert wi (Root root) tokens


toRootsBySurah : WordsLocations -> Surahs
toRootsBySurah words =
    Dict.foldl
        (\root locs dic ->
            List.foldl (\loc surahs -> addRoot (Root root) loc surahs) dic locs
        )
        (Surahs Dict.empty)
        words


addRoot : Root -> Location -> Surahs -> Surahs
addRoot (Root root) ( si, ai, wi ) (Surahs surahs) =
    let
        newSurah : Surahs
        newSurah =
            Surahs (Dict.insert si (newAyat Dict.empty) surahs)

        newAyat ayats =
            Ayats (Dict.insert ai (newToken Dict.empty) ayats)

        newToken tokens =
            Tokens (Dict.insert wi (Root root) tokens)

        test : Dict Index Tokens -> Dict Index Root -> Ayats
        test ayats tokens =
            Ayats <| Dict.insert ai (newToken tokens) ayats
    in
    case Dict.get si surahs of
        Just (Ayats ayats) ->
            case Dict.get ai ayats of
                Just (Tokens tokens) ->
                    case Dict.get wi tokens of
                        Just _ ->
                            Debug.log "word collison" (Surahs surahs)

                        Nothing ->
                            Surahs (Dict.insert si (test ayats tokens) surahs)

                Nothing ->
                    Surahs (Dict.insert si (newAyat ayats) surahs)

        Nothing ->
            newSurah


ayatsFrom : Index -> Surahs -> Ayats
ayatsFrom index (Surahs surahs) =
    Dict.get index surahs |> Maybe.withDefault (Ayats Dict.empty)


tokensFrom : Index -> Ayats -> Tokens
tokensFrom index (Ayats ayats) =
    Dict.get index ayats |> Maybe.withDefault (Tokens Dict.empty)


type Msg
    = SetKnown Root
    | LoadSurah (Result Http.Error SurahData)
    | LoadWordLocations (Result Http.Error WordsLocations)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetKnown root ->
            let
                learn : Root -> Dict ( Char, Char, Char ) () -> Dict ( Char, Char, Char ) ()
                learn (Root rootLetters) learned =
                    Dict.insert rootLetters () learned

                forget : Root -> Dict ( Char, Char, Char ) () -> Dict ( Char, Char, Char ) ()
                forget (Root rootLetters) learned =
                    Dict.remove rootLetters learned
            in
            if root == Root (' ', ' ', ' ') then
                (model, Cmd.none)
            else
                if isLearned root model.known then
                    ( { model | known = forget root model.known }, Cmd.none )

                else
                    ( { model | known = learn root model.known }, Cmd.none )

        LoadSurah (Ok surahData) ->
            ( { model | surahs = surahData }, wordsCmd )

        LoadSurah _ ->
            ( model, Cmd.none )

        LoadWordLocations (Ok locations) ->
            ( { model | locationsWord = toRootsBySurah locations, wordsLocations = locations }, Cmd.none )

        LoadWordLocations _ ->
            ( model, Cmd.none )


viewSurah : Model -> Html Msg
viewSurah model =
    let
        ayats =
            ayatsFrom model.surahNumber model.locationsWord
    in
    Dict.get model.surahNumber model.surahs
        |> Maybe.withDefault Array.empty
        |> Array.toIndexedList
        |> indexBy1
        |> map (viewAyat model ayats)
        |> div []


viewAyat : Model -> Ayats -> ( Int, String ) -> Html Msg
viewAyat model ayats ( ai, s ) =
    let
        tokens =
            tokensFrom ai ayats

        joinYaa list =
            List.foldr addWhenYaa [] list

        encode = "\u{064A}\u{064E}\u{0627}"

        addWhenYaa curr accum =
            case curr of
                "\u{064A}\u{064E}\u{0627}" ->
                    case accum of
                        (x::xs) ->
                            (encode ++ " " ++ x) :: xs
                        _ ->
                            curr :: accum

                _ ->
                    curr :: accum
    in
    p []
        ((s
            |> String.split " "
            |> joinYaa
            |> Array.fromList
            |> Array.toIndexedList
            |> indexBy1
            |> map (viewWord model tokens)
         )
            ++ [ span [] [ text <| fromInt ai ] ]
        )


viewWord : Model -> Tokens -> ( Int, String ) -> Html Msg
viewWord model tokens ( wi, w ) =
    let
        root = (getRootFromWord wi tokens)
        isKnown =
            isLearned root model.known
        isLearnable = root /= Root (' ', ' ', ' ')
    in
    span [ classList [ ( "known", isKnown ), ("learnable", isLearnable)
        ], onClick (SetKnown root) ] [ text (w ++ " " ++ "") ]


view : Model -> Html Msg
view model =
    div []
        [ viewSurah model, viewLearnables model ]


indexBy1 : List ( Int, String ) -> List ( Int, String )
indexBy1 =
    map (\( i, s ) -> ( i + 1, s ))


getRootFromWord : Index -> Tokens -> Root
getRootFromWord index (Tokens token) =
    Dict.get index token |> Maybe.withDefault (Root ( ' ', ' ', ' ' ))


viewLearnables : Model -> Html Msg
viewLearnables model =
    ul []
        (map
            (\w ->
                viewLearnableWord (Root w) (isLearned (Root w) model.known)
            )
            (Dict.keys model.wordsLocations)
        )


isLearned : Root -> Known -> Bool
isLearned (Root root) known =
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
            , rootToText root
            ]
        ]


rootToText : Root -> Html Msg
rootToText (Root ( a, b, c )) =
    text (fromChar a ++ " " ++ fromChar b ++ " " ++ fromChar c)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }
