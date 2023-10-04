module Slide exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Atts
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Random
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Simple.Transition as Transition


main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

type alias Cell = {x:Int, y:Int, n:Int}
type alias Model = {conf: List Cell
                    ,started: Bool}

type Msg = Slide Cell
           | RandomMove Int Cell
           | Shuffle

init: () -> (Model, Cmd Msg)
init _= ({conf=[{x=0,y=0,n=0}
             ,{x=0,y=1,n=1}
             ,{x=0,y=2,n=2}
             ,{x=1,y=0,n=3}
             ,{x=1,y=1,n=4}
             ,{x=1,y=2,n=5}
             ] 
            ,started = False
       }
       ,Cmd.none)
randomCell: List Cell -> Random.Generator Cell
randomCell cells =
    Random.uniform ((Maybe.withDefault (Cell 0 0 0)) <| List.head cells)
        (List.drop 1 cells)
update:Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Slide cell ->
            if cell.n == 0 then
                (model,Cmd.none)
            else
                ({model | conf = slide cell model.conf}
                ,Cmd.none)
        RandomMove n cell ->
            if n == 0 then
                (model,Cmd.none)
            else if cell.n == 0 then
                (model,Random.generate (RandomMove (n-1))(randomCell model.conf) )
            else
                ({model | conf = slide cell model.conf}
                ,Random.generate (RandomMove (n-1))(randomCell model.conf) )
        Shuffle ->
            ({model|started=True},Random.generate (RandomMove 100)(randomCell model.conf) )

check:Model -> Bool
check model =
    List.all (\cell -> cell.n==(cell.y+3*cell.x)) model.conf



slide: Cell -> List Cell -> List Cell
slide cell conf =
    let
        toBeSwapped = cell::(List.filter (\c -> c.n == 0 && adjacent c cell) conf)
        others = List.filter
                 (\c -> not <| List.member c toBeSwapped) conf
    in
        if (List.length toBeSwapped)==2 then
            (List.map (\c -> if c/=cell then
                                {c|n=cell.n}
                            else
                                {c|n=0}
                      ) toBeSwapped
            )++others
        else
            conf

adjacent: Cell -> Cell -> Bool
adjacent cell1 cell2 =
    if cell1.x == cell2.x && cell1.y == cell2.y then
        False
    else if cell1.x == cell2.x then
             True
         else if cell1.y == cell2.y then
                  True
              else
                  False



unit = 150

animatedSvg =
    Animated.svg
        { class = Svg.Attributes.class
        }
animatedG : Animation -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
animatedG =
        animatedSvg Svg.g
fade : Animation
fade = Animation.fromTo
                { duration = 1000
                , options = []
                }
                [ P.opacity 0 ]
                [ P.opacity 1 ]
cellView: Cell -> Html Msg
cellView cell =
    let
        fy = (toFloat cell.y)
        px = 100*(1+2*(toFloat cell.x))*(cos (fy*(2*pi/3)))
        py = 100*(1+2*(toFloat cell.x))*(sin (fy*(2*pi/3)))
        xystr = "("++(String.fromFloat px)++","++(String.fromFloat py)++")"
    in
        g[transform ("translate"++xystr)
         ,onClick (Slide cell)]
            (if cell.n /= 0 then
                 [circle [cx "0"
                         ,cy "0"
                         ,r "50"
                         ,fill "skyblue"
                         ,stroke (if (cell.y+3*cell.x)==cell.n then
                                   "red"
                                else
                                   "black")

                         ,strokeWidth "2"
                         ][]
                 ,text_ [x "-10"
                        ,y "20"
                        ,stroke "black"
                        ,fontSize "50px"
                        ]
                      [text (String.fromInt cell.n)]
                 ,circle[cx "-20"
                        ,cy "30"
                        ,r "10"
                        ,fill "white"]
                      []
                ,text_ [x "40"
                        ,y "40"
                        ,stroke "red"
                        ,fontSize "30"
                        ]
                        [text (String.fromInt (cell.y+3*cell.x))]

                 ]
             else
                 [circle [cx "0"
                         ,cy "0"
                         ,r "50"
                         ,fill "white"
                         ,stroke "black"
                         ,strokeWidth "2"
                         ][]
                ,text_ [x "40"
                        ,y "40"
                        ,stroke "red"
                        ,fontSize "30"
                        ]
                        [text (String.fromInt (cell.y+3*cell.x))]
                 ]
            )

buttonView: Svg Msg
buttonView = g [transform "translate(250,250)"
                ,onClick Shuffle
                ,class "gold-box-on-hover"

            ]
            [rect [x "0"
                   ,y "0"
                   ,width "100"
                   ,height "50"
                   --,fill "gray"
                   ,rx "10"
                   ,ry "10"
                   ,stroke "black"
                   ,class "shuffle"
                   ,Svg.Attributes.style "cursor: pointer;"
                   , Transition.properties
                     [ Transition.property "fill" 100 []
                     , Transition.color 100 [ Transition.delay 50 ]
                   ]]
                   []
            ,text_ [x "10"
                    ,y "40"
                    ,stroke "black"
                    ,fontSize "30"]
                    [text "shuffle"]]

view: Model -> Html Msg
view model =
    Html.div [Atts.align "center"]
        [svg [width "800"
             ,height "800"
             ,viewBox "-400 -400 800 800"
             ]
             ([circle [r "300",fill "none",stroke "black",strokeWidth "2"][]
              ,circle [r "100",fill "none",stroke "black",strokeWidth "2"][]
              ,line [x1 "100"
                    ,y1 "0"
                    ,x2 "300"
                    ,y2 "0"
                    ,stroke "black"
                    ,strokeWidth "2"
                    ][]
              ,line [x1 (String.fromFloat (100*(cos (2*pi/3))))
                    ,y1 (String.fromFloat (100*(sin (2*pi/3))))
                    ,x2 (String.fromFloat (300*(cos (2*pi/3))))
                    ,y2 (String.fromFloat (300*(sin (2*pi/3))))
                    ,stroke "black"
                    ,strokeWidth "2"
                    ][]
              ,line [x1 (String.fromFloat (100*(cos (4*pi/3))))
                    ,y1 (String.fromFloat (100*(sin (4*pi/3))))
                    ,x2 (String.fromFloat (300*(cos (4*pi/3))))
                    ,y2 (String.fromFloat (300*(sin (4*pi/3))))
                    ,stroke "black"
                    ,strokeWidth "2"
                    ][]
             , buttonView

              ]++
                  (List.map cellView model.conf)++
                  (if check model then
                    [text_ [x "0"
                           ,y "0"
                           ,fontSize "100"
                           ,stroke "red"]
                    [text (if model.started then
                                "完成"
                            else 
                                ""
                            )]
                    ]
                    else
                        []
                  )
             )
        ]
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
