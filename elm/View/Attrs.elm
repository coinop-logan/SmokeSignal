module View.Attrs exposing (cappedHeight, cappedWidth, roundBorder, sanSerifFont, typeFont, whiteGlowAttribute, whiteGlowAttributeSmall)

{-| A module for managing elm-ui 'Attribute' values and related functions.
-}

import Element exposing (Attribute)
import Element.Border as Border
import Element.Font as Font


roundBorder : Attribute msg
roundBorder =
    Border.rounded 5


typeFont : Attribute msg
typeFont =
    Font.family
        [ Font.typeface "SmallType"
        ]


sanSerifFont : Attribute msg
sanSerifFont =
    Font.family
        [ Font.typeface "DDin"
        ]


cappedWidth : Int -> Attribute msg
cappedWidth n =
    Element.fill |> Element.maximum n |> Element.width


cappedHeight : Int -> Attribute msg
cappedHeight n =
    Element.fill |> Element.maximum n |> Element.height


whiteGlowAttribute : Attribute msg
whiteGlowAttribute =
    Border.glow
        (Element.rgba 1 1 1 0.4)
        5


whiteGlowAttributeSmall : Attribute msg
whiteGlowAttributeSmall =
    Border.glow
        (Element.rgba 1 1 1 0.4)
        2