module Flash exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Length exposing (Length)
import List.Zipper as Zipper exposing (Zipper)
import Luminance
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
    , training : Training
    }


type Exercise
    = Dance Routine Duration
    | Flash String Duration
    | Pause String


type alias Training =
    Zipper Exercise


type alias Body a =
    { head : a
    , torso : a
    , leftArm : a
    , rightArm : a
    , leftLeg : a
    , rightLeg : a
    }


type alias Flags =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { time = 0
      , width = flags.width
      , height = flags.height
      , training =
            Zipper.withDefault (Pause "Pauze!") <|
                Zipper.fromList
                    [ Dance floss <| Duration.seconds 6
                    , Flash "Zwaaien met je handen!" <| Duration.milliseconds 500
                    , Dance macarena <| Duration.seconds 15
                    , Flash "Raak je neus aan!" <| Duration.milliseconds 200
                    , Pause "Klaar!"
                    ]
      }
    , Cmd.none
    )


type Msg
    = Diff Float
    | Resize Int Int


cycleTraining : Training -> Training
cycleTraining training =
    case Zipper.next training of
        Nothing ->
            Zipper.first training

        Just training_ ->
            training_


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff_msec ->
            let
                time =
                    model.time + diff_msec

                exercise =
                    Zipper.current model.training

                proceedAfter duration =
                    if time > Duration.inMilliseconds duration then
                        { model | time = 0, training = cycleTraining model.training }

                    else
                        { model | time = time }

                model_ =
                    case exercise of
                        Pause _ ->
                            { model | time = time }

                        Dance _ duration ->
                            proceedAfter duration

                        Flash _ duration ->
                            proceedAfter duration
            in
            pure model_

        Resize width height ->
            pure { model | width = toFloat width, height = toFloat height }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Diff
        , Browser.Events.onResize Resize
        ]



-- RENDER


view : Model -> Html Msg
view model =
    let
        exercise =
            Zipper.current model.training
    in
    Html.div
        [ Html.Attributes.style "position" "relative" ]
        [ case exercise of
            Dance routine _ ->
                viewDancer routine model.time model.width model.height

            Flash text _ ->
                viewText text

            Pause text ->
                viewText text
        ]


viewText text =
    Html.h1 [] [ Html.text text ]


viewDancer : Routine -> Float -> Float -> Float -> Html msg
viewDancer routine time width height =
    let
        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 -2 0
                , eyePoint = Point3d.meters 0 2 16
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
    in
    Scene3d.render [ Scene3d.clearColor Color.darkPurple ]
        { camera = camera
        , width = Pixels.pixels width
        , height = Pixels.pixels height
        , ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = False }
        , exposure = Scene3d.Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Scene3d.Chromaticity.daylight
        }
        [ move routine time head Head
        , move routine time torso Torso
        , move routine time arm LeftArm
            |> Drawable.translateIn Direction3d.negativeX armOffset
        , move routine time arm RightArm
            |> Drawable.translateIn Direction3d.x armOffset
        , move routine time leg LeftLeg
            |> Drawable.translateIn Direction3d.negativeX legOffset
        , move routine time leg RightLeg
            |> Drawable.translateIn Direction3d.x legOffset
        ]


head : Drawable a
head =
    bodyPart <| Shape.sphere { radius = var.headRadius, subdivisions = 72 }


leg : Drawable a
leg =
    pill var.limbRadius var.legLength
        |> Drawable.translateIn Direction3d.negativeY
            (Quantity.sum [ var.headRadius, var.spacing, var.torsoLength, var.spacing ])


arm : Drawable a
arm =
    pill var.limbRadius var.armLength
        |> Drawable.translateIn Direction3d.negativeY (Quantity.plus var.spacing var.headRadius)


torso : Drawable a
torso =
    pill torsoRadius var.torsoLength
        |> Drawable.translateIn Direction3d.negativeY (Quantity.plus var.spacing var.headRadius)


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


torsoRadius : Length
torsoRadius =
    Quantity.half (Quantity.plus var.headRadius var.limbRadius)


armOffset : Length
armOffset =
    Quantity.sum [ torsoRadius, var.spacing, var.limbRadius ]


legOffset : Length
legOffset =
    Quantity.plus var.spacing var.limbRadius


var =
    { spacing = Length.meters 0.075
    , headRadius = Length.meters 0.85
    , limbRadius = Length.meters 0.375
    , armLength = Length.meters 2.0
    , legLength = Length.meters 2.8
    , torsoLength = Length.meters 2.5
    , movement = Length.meters 0.85
    }



-- DANCE (ROUTINE)


type Routine
    = Routine Float (Body (List Move))


type Part
    = Head
    | Torso
    | LeftArm
    | RightArm
    | LeftLeg
    | RightLeg


part : Part -> Body a -> a
part part_ body =
    case part_ of
        Head ->
            body.head

        Torso ->
            body.torso

        LeftArm ->
            body.leftArm

        RightArm ->
            body.rightArm

        LeftLeg ->
            body.leftLeg

        RightLeg ->
            body.rightLeg


move : Routine -> Float -> Drawable () -> Part -> Drawable ()
move moves time drawable body =
    case moves of
        Routine stepDuration sequence ->
            let
                progress =
                    time / stepDuration

                start =
                    floor progress

                apply motion =
                    curve start (progress - toFloat start) motion.steps
                        |> Angle.degrees
                        |> Drawable.rotateAround motion.axis
            in
            List.foldl apply drawable (part body sequence)


type alias Move =
    { steps : List Float
    , axis : Axis3d Length.Meters ()
    }


floss : Routine
floss =
    Routine 350
        { head =
            [ { steps = [ 15, -10, 10, -15, 10, -10 ], axis = shouldersZ } ]
        , torso =
            [ { steps = [ -5, 5 ], axis = hips } ]
        , leftArm =
            [ { steps = [ 10, -10 ], axis = Axis3d.z }
            , { steps = [ -15, -15, 15 ], axis = Axis3d.x }
            ]
        , rightArm =
            [ { steps = [ 10, -10 ], axis = Axis3d.z }
            , { steps = [ -15, -15, 15 ], axis = Axis3d.x }
            ]
        , leftLeg =
            [ { steps = [ 5, -5 ], axis = feet } ]
        , rightLeg =
            [ { steps = [ 5, -5 ], axis = feet } ]
        }


macarena : Routine
macarena =
    let
        shake vigor =
            List.map ((*) vigor) <|
                List.concat
                    [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0, 1, -1, 1, -1 ]
                    ]
    in
    Routine 280
        { head =
            [ { steps = shake 5, axis = shouldersZ } ]
        , torso =
            [ { steps = shake 10, axis = hips } ]
        , leftArm =
            [ { steps =
                    List.concat
                        [ [ 0, 0, 0, 0, -5, 0, 0, 0 ]
                        , [ -15, -15, -15, -15, 30, 30, 30, 30 ]
                        , [ 0, 0, 0, 0, 30, 30, 30, 30 ]
                        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                        ]
              , axis = shouldersZ
              }
            , { steps =
                    List.concat
                        [ [ 0, 0, 0, 0, -90, -90, -90, -90 ]
                        , [ -95, -90, -90, -90, -105, -105, -105, -105 ]
                        , [ -150, -150, -150, -150, -60, -60, -60, -60 ]
                        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                        ]
              , axis = shouldersX
              }
            , { steps =
                    List.concat
                        [ [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , [ 10, 10, 10, 10, 10, -10, 10, -10 ]
                        ]
              , axis = hips
              }
            ]
        , rightArm =
            [ { steps =
                    List.concat
                        [ [ 0, 0, 0, 0, 0, 0, 5, 0, 0, 0 ]
                        , [ 15, 15, 15, 15, -30, -30, -30, -30 ]
                        , [ 0, 0, 0, 0, -30, -30, -30, -30 ]
                        , [ 0, 0, 0, 0, 0, 0 ]
                        ]
              , axis = shouldersZ
              }
            , { steps =
                    List.concat
                        [ [ 0, 0, 0, 0, 0, 0, -90, -90, -90, -90 ]
                        , [ -95, -90, -90, -90, -105, -105, -105, -105 ]
                        , [ -150, -150, -150, -150, -60, -60, -60, -60 ]
                        , [ 0, 0, 0, 0, 0, 0 ]
                        ]
              , axis = shouldersX
              }
            , { steps =
                    List.concat
                        [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , [ -10, -10, -10, -10, 10, -10 ]
                        ]
              , axis = hips
              }
            ]
        , leftLeg =
            [ { steps = shake -5, axis = feet } ]
        , rightLeg =
            [ { steps = shake -5, axis = feet } ]
        }


shouldersX : Axis3d Length.Meters c
shouldersX =
    shoulders Direction3d.x


shouldersZ : Axis3d Length.Meters c
shouldersZ =
    shoulders Direction3d.z


shoulders : Direction3d c -> Axis3d Length.Meters c
shoulders direction =
    joint direction <| Quantity.plus var.headRadius var.spacing


hips : Axis3d Length.Meters c
hips =
    joint Direction3d.z <| Quantity.sum [ var.headRadius, var.spacing, var.torsoLength ]


feet : Axis3d Length.Meters c
feet =
    joint Direction3d.z <| Quantity.sum [ var.headRadius, var.spacing, var.torsoLength, var.spacing, var.legLength ]


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
