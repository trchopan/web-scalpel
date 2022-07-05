module Main exposing (main)

import Browser
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html exposing (..)
import Html.Attributes exposing (href, placeholder, src, style, value)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map7, string)
import Simple.Fuzzy



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias MySuccess =
    { products : List ItemProduct
    , search : String
    }


type Model
    = Failure Http.Error
    | Loading
    | Success MySuccess


type alias ItemPrice =
    { list : Int
    , special : Int
    }


type alias ItemProduct =
    { date : String
    , name : String
    , image : String
    , link : String
    , moreInfo : List String
    , source : String
    , price : ItemPrice
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getItemProducts )



-- UPDATE


type Msg
    = MorePlease
    | GotItemProduct (Result Http.Error (List ItemProduct))
    | Change ( List ItemProduct, String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getItemProducts )

        GotItemProduct result ->
            case result of
                Ok itemProducts ->
                    ( Success (MySuccess itemProducts ""), Cmd.none )

                Err err ->
                    ( Failure err, Cmd.none )

        Change ( products, search ) ->
            ( Success (MySuccess products search), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "padding" "1rem" ]
        [ h2 [] [ text "My Simple Scraper ✨" ]
        , viewProduct model
        ]


itemRow : ItemProduct -> Html Msg
itemRow item =
    div []
        [ a [ href item.link ]
            [ h3 []
                [ text item.name
                ]
            ]
        , div []
            [ div [ style "width" "150px" ]
                [ img [ src item.image, style "height" "auto", style "width" "150px" ] []
                ]
            , div []
                [ p [ style "font-weight" "bold" ] [ text ("special: " ++ formatPrice item.price.special) ]
                , p [] [ text ("normal: " ++ formatPrice item.price.list) ]
                , p [] [ text ("reduce: " ++ (format usLocale <| reducePercent item.price) ++ "%") ]
                , p [] [ text ("source: " ++ item.source) ]
                , p [] [ text ("date: " ++ item.date) ]
                ]
            ]
        ]


formatPrice : Int -> String
formatPrice price =
    format { usLocale | decimals = Exact 0 } <| toFloat price


reducePercent : ItemPrice -> Float
reducePercent p =
    let
        price =
            toFloat p.list

        special =
            toFloat p.special
    in
    (1.0 - special / price) * 100


viewProduct : Model -> Html Msg
viewProduct model =
    case model of
        Failure err ->
            div []
                [ Debug.log ("Error: " ++ Debug.toString err) (text "Cannot fetch data: Error: ")
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success succ ->
            div []
                [ div []
                    [ input [ placeholder "Search", value succ.search, onInput (\str -> Change ( succ.products, str )) ] []
                    ]
                , div [ style "display" "grid", style "grid-template-columns" "1fr 1fr 1fr", style "grid-gap" "1rem" ]
                    (succ.products
                        |> Simple.Fuzzy.filter .name succ.search
                        |> List.sortWith priceReduceSort
                        |> List.sortWith dateDescSort
                        |> List.map itemRow
                    )
                ]


priceReduceSort : ItemProduct -> ItemProduct -> Order
priceReduceSort product1 product2 =
    let
        reducePrice1 =
            reducePercent product1.price

        reducePrice2 =
            reducePercent product2.price
    in
    compare reducePrice2 reducePrice1


dateDescSort : ItemProduct -> ItemProduct -> Order
dateDescSort product1 product2 =
    compare product1.date product2.date



-- HTTP


getItemProducts : Cmd Msg
getItemProducts =
    Http.get
        { url = "http://localhost:3000/products"
        , expect = Http.expectJson GotItemProduct itemProductListDecoder
        }


priceDecoder : Decoder ItemPrice
priceDecoder =
    map2 ItemPrice
        (field "list" int)
        (field "special" int)


itemProductDecoder : Decoder ItemProduct
itemProductDecoder =
    map7 ItemProduct
        (field "date" string)
        (field "name" string)
        (field "image" string)
        (field "link" string)
        (field "moreInfo" (list string))
        (field "source" string)
        (field "price" priceDecoder)


itemProductListDecoder : Decoder (List ItemProduct)
itemProductListDecoder =
    list itemProductDecoder
