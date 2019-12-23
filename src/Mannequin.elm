module Mannequin exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode as Decode
import Length exposing (Length)
import List.Zipper as Zipper exposing (Zipper)
import Luminance
import Parser exposing ((|.), (|=), Parser)
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Exposure
import Scene3d.Light
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Shape as Shape
import Task
import Viewpoint3d


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { time : Float
    , width : Float
    , height : Float
    }


type alias OpposedJoints a =
    { shoulder : a
    , elbow : a
    , wrist : a
    , hip : a
    , knee : a
    , ankle : a
    }


type alias Joints a =
    { neck : a
    , spine : a
    , left : OpposedJoints a
    , right : OpposedJoints a
    }


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { time = 0
      , width = flags.width
      , height = flags.height
      }
    , Cmd.none
    )


type Msg
    = Diff Float
    | Resize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff_msec ->
            let
                time =
                    model.time + diff_msec
            in
            ( { model | time = time }, Cmd.none )

        Resize width height ->
            ( { model | width = toFloat width, height = toFloat height }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Diff
        , Browser.Events.onResize Resize
        ]



-- RENDER


view : Model -> Html Msg
view model =
    Html.div
        []
        [ viewDancer floss model.time model.width model.height
        ]


viewDancer : Routine -> Float -> Float -> Float -> Html msg
viewDancer routine time width height =
    let
        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 -0.5 0
                , eyePoint = Point3d.meters 0 0 4
                , upDirection = Direction3d.y
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        sunlight =
            Scene3d.Light.directional Scene3d.Chromaticity.daylight
                (Illuminance.lux 10000)
                (Direction3d.zxY (Angle.degrees 45) (Angle.degrees 195))

        ambientLighting =
            Scene3d.Light.overcast
                { zenithDirection = Direction3d.z
                , chromaticity = Scene3d.Chromaticity.daylight
                , zenithLuminance = Luminance.nits 5000
                }

        ( step, moves ) =
            case routine of
                Routine s m ->
                    ( s, m )

        leg joints =
            move time step joints.hip <|
                Drawable.group
                    [ upperLeg
                    , move time step joints.knee <|
                        Drawable.group
                            [ lowerLeg
                            , move time step joints.ankle foot
                            ]
                    ]

        arm joints =
            move time step joints.shoulder <|
                Drawable.group
                    [ upperArm
                    , move time step joints.elbow <|
                        Drawable.group
                            [ lowerArm
                            , move time step joints.wrist hand
                            ]
                    ]

        left =
            Drawable.translateIn Direction3d.negativeX

        right =
            Drawable.translateIn Direction3d.x
    in
    Scene3d.render [ Scene3d.clearColor (Color.hsl 200 0.5 0.5) ]
        { camera = camera
        , width = Pixels.pixels width
        , height = Pixels.pixels height
        , ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = False }
        , exposure = Scene3d.Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Scene3d.Chromaticity.daylight
        }
        [ Drawable.group
            [ torso
            , move time step moves.neck head
            , move time step moves.spine <|
                Drawable.group
                    [ abdomen
                    , leg moves.left |> left legOffset
                    , leg moves.right |> right legOffset
                    ]
            , arm moves.left |> left armOffset
            , arm moves.right |> right armOffset
            ]
        ]


head : Drawable a
head =
    bodyPart (Shape.sphere { radius = var.head.radius, subdivisions = 72 })
        |> Drawable.translateIn Direction3d.y var.head.radius


upperLeg : Drawable a
upperLeg =
    pill var.leg.upper.radius var.leg.upper.length
        |> Drawable.translateIn Direction3d.negativeY
            (Quantity.sum [ var.torso.length, var.abdomen.length ])


lowerLeg : Drawable a
lowerLeg =
    pill var.leg.lower.radius var.leg.lower.length
        |> Drawable.translateIn Direction3d.negativeY var.leg.upper.length


foot : Drawable a
foot =
    pill var.foot.radius var.foot.length
        |> Drawable.translateIn Direction3d.negativeY var.leg.lower.length


upperArm : Drawable a
upperArm =
    pill var.arm.upper.radius var.arm.upper.length


lowerArm : Drawable a
lowerArm =
    pill var.arm.lower.radius var.arm.lower.length
        |> Drawable.translateIn Direction3d.negativeY var.arm.upper.length


hand : Drawable a
hand =
    pill var.hand.radius var.hand.length
        |> Drawable.translateIn Direction3d.negativeY var.arm.lower.length


torso : Drawable a
torso =
    pill var.torso.radius var.torso.length


abdomen : Drawable a
abdomen =
    pill var.abdomen.radius var.abdomen.length
        |> Drawable.translateIn Direction3d.negativeY var.torso.length


pill : Length -> Length -> Drawable a
pill radius length =
    let
        height =
            Quantity.minus (Quantity.twice radius) length

        trunk =
            Shape.cylinder { radius = radius, height = height, subdivisions = 72 }

        end =
            Shape.sphere { radius = radius, subdivisions = 72 }
    in
    Drawable.group
        [ bodyPart trunk
            |> Drawable.translateIn Direction3d.z radius
        , bodyPart end
            |> Drawable.translateIn Direction3d.z radius
        , bodyPart end
            |> Drawable.translateIn Direction3d.z (Quantity.plus radius height)
        ]
        |> Drawable.rotateAround Axis3d.x (Angle.degrees 90)


bodyPart : Mesh a (Mesh.Triangles Mesh.WithNormals uv tangents shadows) -> Drawable a
bodyPart =
    Drawable.physical { baseColor = Color.white, roughness = 0.25, metallic = False }


armOffset : Length
armOffset =
    Quantity.sum [ var.torso.radius, var.arm.upper.radius ]


legOffset : Length
legOffset =
    var.leg.upper.radius


var =
    { head = { radius = Length.meters 0.075 }
    , arm =
        { upper = { length = Length.meters 0.2, radius = Length.meters 0.03 }
        , lower = { length = Length.meters 0.2, radius = Length.meters 0.035 }
        }
    , hand =
        { length = Length.meters 0.08, radius = Length.meters 0.035 }
    , leg =
        { upper = { length = Length.meters 0.32, radius = Length.meters 0.06 }
        , lower = { length = Length.meters 0.34, radius = Length.meters 0.05 }
        }
    , foot =
        { length = Length.meters 0.37, radius = Length.meters 0.06 }
    , torso = { length = Length.meters 0.32, radius = Length.meters 0.15 }
    , abdomen = { length = Length.meters 0.2, radius = Length.meters 0.15 }
    , movement = Length.meters 0.85
    }



-- DANCE (ROUTINE)


type Routine
    = Routine Float (Joints (List Move))


move : Float -> Float -> List Move -> Drawable () -> Drawable ()
move time step moves drawable =
    let
        progress =
            time / step

        start =
            floor progress

        apply motion =
            curve start (progress - toFloat start) motion.steps
                |> Angle.degrees
                |> Drawable.rotateAround motion.axis
    in
    List.foldl apply drawable moves


type alias Move =
    { steps : List Float
    , axis : Axis3d Length.Meters ()
    }


floss : Routine
floss =
    Routine 350
        { neck =
            [ { steps = [ 15, -10, 10, -15, 10, -10 ], axis = shouldersZ } ]
        , spine =
            [ { steps = [ -5, 5 ], axis = hips } ]
        , left =
            { shoulder =
                [ { steps = [ 10, -10 ], axis = Axis3d.z }
                , { steps = [ -15, -15, 15 ], axis = Axis3d.x }
                ]
            , elbow = []
            , wrist = []
            , hip = [ { steps = [ 5, -5 ], axis = feet } ]
            , knee = []
            , ankle = []
            }
        , right =
            { shoulder =
                [ { steps = [ 10, -10 ], axis = Axis3d.z }
                , { steps = [ -15, -15, 15 ], axis = Axis3d.x }
                ]
            , elbow = []
            , wrist = []
            , hip = [ { steps = [ 5, -5 ], axis = feet } ]
            , knee = []
            , ankle = []
            }
        }


shouldersX : Axis3d Length.Meters c
shouldersX =
    shoulders Direction3d.x


shouldersZ : Axis3d Length.Meters c
shouldersZ =
    shoulders Direction3d.z


shoulders : Direction3d c -> Axis3d Length.Meters c
shoulders direction =
    joint direction <| var.head.radius


hips : Axis3d Length.Meters c
hips =
    joint Direction3d.z <| Quantity.sum [ var.head.radius, var.torso.length ]


feet : Axis3d Length.Meters c
feet =
    joint Direction3d.z <| Quantity.sum [ var.head.radius, var.torso.length, var.leg.upper.length ]


joint : Direction3d c -> Length -> Axis3d Length.Meters c
joint dir y =
    Axis3d.withDirection dir <|
        Point3d.xyz Quantity.zero (Quantity.negate y) Quantity.zero



-- CUBIC BEZIER
-- from earlier experiments


curve : Int -> Float -> List Float -> Float
curve completed t steps =
    case
        List.drop
            (modBy (List.length steps) completed)
            (steps ++ List.take 1 steps)
    of
        first :: second :: _ ->
            cubicBezier t first second

        _ ->
            0


cubicBezier : Float -> Float -> Float -> Float
cubicBezier t p0 p3 =
    let
        controlPoint1 =
            1.05

        controlPoint2 =
            0.75

        p1 =
            p0 + controlPoint1 * (p3 - p0)

        p2 =
            p0 + controlPoint2 * (p3 - p0)
    in
    (p0 * ((1 - t) ^ 3))
        + (p1 * 3 * ((1 - t) ^ 2) * t)
        + (p2 * 3 * (1 - t) * (t ^ 2))
        + (p3 * (t ^ 3))
