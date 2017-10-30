port module Main exposing (..)

import Array exposing (Array)
import Char exposing (isDigit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:))
import Json.Encode exposing (..)
import String exposing (all, any)
import Task exposing (..)
import Tuple exposing (..)


init : CacheModel -> ( Model, Cmd Msg )
init cacheModel =
    let
        model =
            cacheModel.cacheModel
    in
    ( model, Cmd.none )


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- MODEL


type alias Model =
    { books : List Book
    , editing : Book
    }


type alias Book =
    { isbn : String
    , name : String
    , author : String
    , publisher : String
    , year : String
    , genre : String
    , edition : String
    , inventory : String
    }


type alias CacheModel =
    { cacheModel : Model
    }


emptyModel : Model
emptyModel =
    Model [] emptyBook


emptyBook : Book
emptyBook =
    Book "" "" "" "" "" "" "" ""



-- UPDATE // CONTROLLER


type Msg
    = ISBN String
    | Name String
    | Author String
    | Publisher String
    | Year String
    | Genre String
    | Edition String
    | Inventory String
    | AddEditing
    | DeleteBook String
    | EditBook String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        book =
            model.editing

        books =
            model.books
    in
    case msg of
        AddEditing ->
            ( { model | books = dropDuplicates book books }, updateCache (encodeModel model) )

        ISBN isbn ->
            ( Model books { book | isbn = isbn }, updateCache (encodeModel model) )

        Name name ->
            ( Model books { book | name = name }, updateCache (encodeModel model) )

        Author author ->
            ( Model books { book | author = author }, updateCache (encodeModel model) )

        Publisher publisher ->
            ( Model books { book | publisher = publisher }, updateCache (encodeModel model) )

        Year year ->
            ( Model books { book | year = year }, updateCache (encodeModel model) )

        Genre genre ->
            ( Model books { book | genre = genre }, updateCache (encodeModel model) )

        Edition edition ->
            ( Model books { book | edition = edition }, updateCache (encodeModel model) )

        Inventory inventory ->
            ( Model books { book | inventory = inventory }, updateCache (encodeModel model) )

        DeleteBook isbn ->
            ( { model | books = deleteBook model isbn }, updateCache (encodeModel model) )

        EditBook isbn ->
            ( { model | editing = editExistingBook model isbn }, updateCache (encodeModel model) )


validateButton : Book -> Bool
validateButton book =
    if
        String.isEmpty book.isbn
            || String.isEmpty book.name
            || String.isEmpty book.author
            || String.isEmpty book.publisher
            || String.isEmpty book.year
            || String.isEmpty book.genre
            || String.isEmpty book.edition
            || String.isEmpty book.inventory
            || not (all isDigit book.isbn)
            || any isDigit book.name
            || any isDigit book.author
            || any isDigit book.publisher
            || not (all isDigit book.year)
            || any isDigit book.genre
            || any isDigit book.edition
            || not (all isDigit book.inventory)
    then
        True
    else
        False


deleteBook : Model -> String -> List Book
deleteBook model isbn =
    Tuple.second (List.partition (\x -> String.contains x.isbn isbn && String.contains isbn x.isbn) model.books)


editExistingBook : Model -> String -> Book
editExistingBook model isbn =
    Maybe.withDefault emptyBook (List.head (List.filter (\book -> book.isbn /= isbn) model.books))


dropDuplicates : Book -> List Book -> List Book
dropDuplicates book books =
    book :: Tuple.second (List.partition (\x -> String.contains x.isbn book.isbn && String.contains book.isbn x.isbn) books)


digitValidation : String -> Html msg
digitValidation digit =
    let
        ( color, message ) =
            if all isDigit digit then
                ( "green", "OK" )
            else
                ( "red", "Only Digits" )
    in
    span [ style [ ( "color", color ) ] ] [ text message ]


stringValidation : String -> Html msg
stringValidation string =
    let
        ( color, message ) =
            if any isDigit string then
                ( "red", "Only Letters" )
            else
                ( "green", "OK" )
    in
    span [ style [ ( "color", color ) ] ] [ text message ]


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "editing", encodeBook model.editing )
        , ( "books", Json.Encode.list (List.map encodeBook model.books) )
        ]


encodeBook : Book -> Json.Encode.Value
encodeBook book =
    Json.Encode.object
        [ ( "isbn", Json.Encode.string book.isbn )
        , ( "name", Json.Encode.string book.name )
        , ( "author", Json.Encode.string book.author )
        , ( "publisher", Json.Encode.string book.publisher )
        , ( "year", Json.Encode.string book.year )
        , ( "genre", Json.Encode.string book.genre )
        , ( "edition", Json.Encode.string book.edition )
        , ( "inventory", Json.Encode.string book.inventory )
        ]


decodeCacheModel : Json.Decode.Decoder CacheModel
decodeCacheModel =
    Json.Decode.succeed CacheModel
        |: field "cacheModel" decodeModel


decodeBook : Json.Decode.Decoder Book
decodeBook =
    Json.Decode.succeed Book
        |: field "isbn" Json.Decode.string
        |: field "name" Json.Decode.string
        |: field "author" Json.Decode.string
        |: field "publisher" Json.Decode.string
        |: field "year" Json.Decode.string
        |: field "genre" Json.Decode.string
        |: field "edition" Json.Decode.string
        |: field "inventory" Json.Decode.string


decodeModel : Json.Decode.Decoder Model
decodeModel =
    Json.Decode.succeed Model
        |: field "books" (Json.Decode.list decodeBook)
        |: field "editing" decodeBook



-- PORTS


port updateCache : Json.Encode.Value -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ booksInStock model.books
        , editBook model.editing
        , button [ Html.Attributes.disabled (validateButton model.editing), onClick AddEditing ] [ text "add" ]
        , div [] [ bookList model.books ]
        ]


booksInStock : List Book -> Html Msg
booksInStock books =
    div [] [ text ("Number of unique books in stock: " ++ toString (List.length books)) ]


editBook : Book -> Html Msg
editBook book =
    div [ style [] ]
        [ div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "ISBN", style [ ( "display", "block" ) ] ] [ text "ISBN" ]
            , input [ id "ISBN", type_ "text", placeholder "ISBN", onInput ISBN, Html.Attributes.value book.isbn ] []
            , digitValidation book.isbn
            ]
        , div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "Name", style [ ( "display", "block" ) ] ] [ text "Name" ]
            , input [ type_ "text", placeholder "Name", onInput Name, Html.Attributes.value book.name ] []
            , stringValidation book.name
            ]
        , div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "Author", style [ ( "display", "block" ) ] ] [ text "Author" ]
            , input [ type_ "text", placeholder "Author", onInput Author, Html.Attributes.value book.author ] []
            , stringValidation book.author
            ]
        , div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "Publisher", style [ ( "display", "block" ) ] ] [ text "Publisher" ]
            , input [ type_ "text", placeholder "Publisher", onInput Publisher, Html.Attributes.value book.publisher ] []
            , stringValidation book.publisher
            ]
        , div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "Year", style [ ( "display", "block" ) ] ] [ text "Year" ]
            , input [ type_ "text", placeholder "Year", onInput Year, Html.Attributes.value book.year ] []
            , digitValidation book.year
            ]
        , div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "Genre", style [ ( "display", "block" ) ] ] [ text "Genre" ]
            , input [ type_ "text", placeholder "Genre", onInput Genre, Html.Attributes.value book.genre ] []
            , stringValidation book.genre
            ]
        , div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "Edition", style [ ( "display", "block" ) ] ] [ text "Edition" ]
            , input [ type_ "text", placeholder "Edition", onInput Edition, Html.Attributes.value book.edition ] []
            , stringValidation book.edition
            ]
        , div [ style [ ( "padding", "10px" ) ] ]
            [ label [ for "Inventory", style [ ( "display", "block" ) ] ] [ text "Inventory" ]
            , input [ type_ "number", placeholder "Inventory", onInput Inventory, Html.Attributes.value book.inventory ] []
            , digitValidation book.inventory
            ]
        ]


bookList : List Book -> Html Msg
bookList books =
    booksToBook books


booksToBook : List Book -> Html Msg
booksToBook book =
    div [] (List.map bookToBookInfo book)


bookToBookInfo : Book -> Html Msg
bookToBookInfo book =
    div [ style [ ( "border", "black solid 1px" ), ( "padding", "10px" ) ] ]
        [ dt []
            [ text "ISBN" ]
        , dd
            []
            [ text book.isbn ]
        , dt
            []
            [ text "Name" ]
        , dd
            []
            [ text book.name ]
        , dt
            []
            [ text "Author" ]
        , dd
            []
            [ text book.author ]
        , dt
            []
            [ text "Publisher" ]
        , dd
            []
            [ text book.publisher ]
        , dt
            []
            [ text "Year" ]
        , dd
            []
            [ text book.year ]
        , dt
            []
            [ text "Genre" ]
        , dd
            []
            [ text book.genre ]
        , dt
            []
            [ text "Edition" ]
        , dd
            []
            [ text book.edition ]
        , dt
            []
            [ text "Inventory" ]
        , dd
            []
            [ text book.inventory ]
        , button [ onClick (DeleteBook book.isbn) ] [ text "Delete" ]
        , button [ onClick (EditBook book.isbn) ] [ text "Edit" ]
        ]
