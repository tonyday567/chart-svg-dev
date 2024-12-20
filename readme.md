
# Table of Contents

1.  [chart-svg-dev](#org24ca2b7)
2.  [Imports](#org8e9c34a)
3.  [dotparse debugging](#org2c83355)
    1.  [simpler example](#org1257358)
    2.  [testAll](#org8ad2403)
    3.  [nh](#orge431ae1)
        1.  [cNH deconstruction](#org32b7ca8)
    4.  [AST](#org22709cc)
4.  [chartSocketPage](#orgfe0e9de)
5.  [v06 Changes](#org3735180)
6.  [example problems](#org4b89119)
    1.  [pathExample](#org0ffa7fe)
    2.  [lineExample](#orgb8dc31e)
    3.  [legends](#org9d30923)
        1.  [frame bug](#org83a8bd3)
        2.  [large text bug](#org5bc6ef2)
    4.  [surface legend](#orga08a5c8)
    5.  [compoundExample](#orgca313e4)
        1.  [original compoundExample](#orge773512)
        2.  [simple experiment](#org9bb3ef8)
        3.  [new example](#org343fb42)
    6.  [stackExample](#org5ecca23)
    7.  [bar & sbar](#orga1925d7)
    8.  [ellipse & quad & cubic](#org17585d1)
    9.  [textExample](#org3a00d72)
    10. [higher number of ticks](#org75a11fb)
    11. [dateExample](#orgee7980e)
7.  [Exact reproduction of proportionate scaling](#org30b99cf)
8.  [projectChart](#orgd18afd6)
    1.  [rebox code](#org02a0fc7)
    2.  [projection decomp](#orgca62762)
    3.  [multi bulk test](#orga396d22)
9.  [jam](#org1a6ea8a)
    1.  [unscaled + no hud](#org39ba0ac)
    2.  [unscaled + zero frame](#org12e043e)
    3.  [ChartAspect + no hud](#org0019885)
    4.  [ChartAspect + zero frame](#org1008171)
    5.  [FixedAspect + no hud](#org27b5e81)
    6.  [FixedAspect + zero frame](#org6e40481)
    7.  [CanvasAspect + zero frame](#orgc7db683)
    8.  [styleBoxText](#org97c72b0)
    9.  [markup manual checks](#org3374fe4)
10. [text and points](#org65de2e6)
11. [fonts](#org2ec1880)
12. [Non-singular Text](#orgdcfefe7)
13. [HudChart lens audit](#org874fb54)
14. [ScaleBorder removal](#org1a9820c)
15. [HudChartSection](#org09cd826)
    1.  [decomp](#org51afffe)
    2.  [solution](#org75d98a8)
16. [mempty](#orgecd4b7e)
17. [chart-svg mega cleanup checklist](#org28732c7)
18. [AST](#orgc81a36a)
    1.  [ChartOptions](#org8fd8e8e)
    2.  [MarkupOptions](#orge4d70a8)
    3.  [HudOptions](#org1139186)
        1.  [AxisOptions](#orgaef3150)
        2.  [FrameOptions](#orgd5d69b3)
        3.  [LegendOptions](#orgb3b56ce)
        4.  [Title](#org9d66a21)
    4.  [ChartTree](#orgdaa9276)
    5.  [Style](#org8781204)


<a id="org24ca2b7"></a>

# chart-svg-dev

[![img](https://img.shields.io/hackage/v/chart-svg-dev.svg)](https://hackage.haskell.org/package/chart-svg-dev)
[![img](https://github.com/tonyday567/chart-svg-dev/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/chart-svg-dev/actions?query=workflow%3Ahaskell-ci)

`chart-svg-dev` is a development environment for chart-svg.


<a id="org8e9c34a"></a>

# Imports

    :r
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    :set -XTupleSections
    :set -XQuasiQuotes
    import Lib
    import Prelude
    import Control.Category ((>>>))
    import Data.Function
    import Data.Maybe
    import Data.Bool
    import Faker.Lorem
    import Chart
    import Prettychart
    import Chart.Examples
    import Optics.Core
    import Data.ByteString.Char8 qualified as C
    import Data.Text qualified as T
    import Control.Monad
    import NumHask.Prelude qualified as NH
    import Data.Functor.Identity
    import Data.Bifunctor
    import Prettyprinter
    import Text.Pretty.Simple
    import MarkupParse
    import DotParse
    import DotParse.Examples
    import Data.String.Interpolate
    (display, quit) <- startChartServer (Just "chart-svg-dev")

    Ok, one module loaded.
    Setting phasers to stung.h.c.i >( port 9160) (ctrl-c to quit)

    writeAllExamples

    ok

    display lineExample

    True

    co = rectExample
    asp = view (#markupOptions % #chartAspect) co
    csAndHud = addHud asp (view #hudOptions co) (view #chartTree co)
    viewbox = finalCanvas asp (Just csAndHud)
    ctFinal = set styleBox' (Just viewbox) csAndHud
    view box' ctFinal
    view styleBox' ctFinal
    view safeBox' ctFinal
    view safeStyleBox' ctFinal
    padSingletons <$> view styleBox' ctFinal
    viewbox


<a id="org2c83355"></a>

# dotparse debugging

-   [ ] check examples, nh, base
-   [X] nodeSize, nodeHeight
-   [X] boxes

    cfg = defaultChartConfig
    exGraph = defaultGraph & addStatements (toStatements Directed (C.pack . show <$> exAGraph)) & set (attL NodeType (ID "shape")) (Just $ ID "box")
    exGraphAugmented <- processGraph exGraph
    exChart = graphToChartWith cfg (T.pack . label) exGraphAugmented & set (#markupOptions % #chartAspect) ChartAspect
    display exChart

    True

    pPrint defaultChartConfig

    pPrint exGraphAugmented

    Graph
        { strict = Last { getLast = Just NoMergeEdges }
        , directed = Last { getLast = Just Directed }
        , graphid = Last { getLast = Nothing }
        , nodeAttributes = fromList
            [
                ( ID "height"
                , IDDouble 0.5
                )
            ,
                ( ID "label"
                , IDQuoted "\N"
                )
            ,
                ( ID "shape"
                , ID "circle"
                )
            ]
        , graphAttributes = fromList
            [
                ( ID "bb"
                , IDQuoted "0,0,436.13,489.31"
                )
            ,
                ( ID "overlap"
                , ID "false"
                )
            ,
                ( ID "rankdir"
                , ID "TB"
                )
            ,
                ( ID "size"
                , IDQuoted "1!"
                )
            ,
                ( ID "splines"
                , ID "spline"
                )
            ]
        , edgeAttributes = fromList
            [
                ( ID "arrowsize"
                , IDDouble 0.5
                )
            ]
        , globalAttributes = fromList []
        , nodes =
            [ NodeStatement
                { nodeID = IDInt 1
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "388,172.66"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 12
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "332,172.66"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 13
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "416,96.393"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 14
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "215,20.131"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 18
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "157,20.131"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 19
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "99,20.131"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 2
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "209,96.393"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 15
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "112,96.393"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 3
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "138,471.31"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 10
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "389,325.18"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 16
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "331,325.18"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 4
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "303,399.31"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 0
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "360,248.92"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 11
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "304,248.92"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 17
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "height"
                        , IDDouble 0.5592
                        )
                    ,
                        ( ID "pos"
                        , IDQuoted "246,248.92"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5592
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 5
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "275,325.18"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 6
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "276,172.66"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 7
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "360,96.393"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 8
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "271,20.131"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            , NodeStatement
                { nodeID = IDInt 9
                , port = Nothing
                , nodeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "18,399.31"
                        )
                    ,
                        ( ID "width"
                        , IDDouble 0.5
                        )
                    ]
                }
            ]
        , edges =
            [ EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 0 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 1 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,381.84,189.99 366.08,231.8 370.05,221.27 375.29,207.37 379.65,195.8"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 0 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 12 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,338.85,191.83 353.92,231.8 350.18,221.87 345.3,208.94 341.1,197.8"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 0 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 6 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,289.31,185.42 346.88,236.31 332.66,223.74 309.88,203.61 294,189.57"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 1 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 13 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,409.15,115.56 394.08,155.54 397.82,145.61 402.7,132.68 406.9,121.54"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 1 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 2 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,226.5,102.07 374.16,160.78 370.07,157.86 365.48,154.87 361,152.52 317.81,129.92 263.28,112.62 232.66,103.82"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 1 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 7 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,366.16,113.73 381.92,155.54 377.95,145.01 372.71,131.11 368.35,119.54"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 15 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 18 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,146.83,37.916 122.21,78.542 128.54,68.098 136.74,54.565 143.59,43.262"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 15 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 19 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,102.38,40.449 108.65,76.269 107.07,67.244 105.16,56.347 103.46,46.631"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 18 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 3 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,142.24,453.57 157.64,40.614 158.52,69.304 160,124.61 160,171.66 160,326.18 160,326.18 160,326.18 160,369.48 150.18,419.27 143.66,\
                          447.54"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 19 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 3 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,126.11,457.54 91.917,39.352 81.69,67.346 64,122.83 64,171.66 64,326.18 64,326.18 64,326.18 64,376.55 100.2,426.97 121.97,452.73"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 2 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 14 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,213.43,40.618 210.39,78.169 211.14,68.873 212.08,57.213 212.92,46.861"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 2 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 18 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,168.26,37.211 198.97,81.075 191.28,70.085 180.53,54.741 171.84,42.329"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 2 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 19 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,115.39,32.195 194.16,85.378 175.24,72.604 142.38,50.415 120.66,35.751"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 2 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 3 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,149.17,456.69 206.93,114.52 203.74,142.42 198,199.44 198,247.92 198,326.18 198,326.18 198,326.18 198,368.06 191.35,379.19 174,\
                          417.31 168.48,429.44 160.24,441.83 153.04,451.57"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 2 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 8 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,260.01,34.293 220.08,82.117 230.14,70.078 245.03,52.238 256.12,38.959"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 3 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 15 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,113.06,116.87 133.95,453.56 127.85,426.81 117,372.71 117,326.18 117,326.18 117,326.18 117,247.92 117,204.16 114.86,153.29 113.38,\
                          123.3"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 3 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 4 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,286.64,407.25 154.42,463.34 184.2,450.71 247.11,424.02 280.72,409.76"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 3 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 9 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,33.454,409.33 122.8,461.45 101.55,449.05 62.873,426.49 38.915,412.51"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 4 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 10 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,373.92,338.83 316.44,387.04 330.57,375.19 352.95,356.42 369.12,342.86"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 4 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 16 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,323.88,344.51 309.35,381.94 312.97,372.63 317.56,360.79 321.59,350.42"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 4 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 5 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,281.33,342.48 296.65,381.94 292.78,371.98 287.79,359.13 283.58,348.28"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 5 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 0 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,346.81,261.44 288.28,312.58 302.75,299.93 325.98,279.64 342.07,265.58"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 5 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 11 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,296.9,268.09 281.29,308.07 285.17,298.13 290.22,285.21 294.57,274.06"
                        )
                    ]
                }
            , EdgeStatement
                { edgeOp = EdgeDirected
                , leftEdge = EdgeID
                    ( IDInt 5 ) Nothing
                , rightEdges = EdgeID
                    ( IDInt 17 ) Nothing :| []
                , edgeAttrs = fromList
                    [
                        ( ID "pos"
                        , IDQuoted "e,253.1,268.09 268.71,308.07 264.83,298.13 259.78,285.21 255.43,274.06"
                        )
                    ]
                }
            ]
        , subgraphs = []
        }


<a id="org1257358"></a>

## simpler example

-   [X] simpler example
-   [X] direct

    :{
    ex1 = [i|
    digraph {
        node [height=0.5;shape=circle]
        graph [overlap=false;size="1!";splines=spline]
        edge [arrowsize=0.5]
        rankdir="TB"
        1
        2
        1 -> 2
        }
    |]
    :}

Graph

    g1 = runDotParser ex1 :: Graph
    g1

    Graph {strict = Last {getLast = Just NoMergeEdges}, directed = Last {getLast = Just Directed}, graphid = Last {getLast = Nothing}, nodeAttributes = fromList [(ID "height",IDDouble 0.5),(ID "shape",ID "circle")], graphAttributes = fromList [(ID "overlap",ID "false"),(ID "size",IDQuoted "1!"),(ID "splines",ID "spline")], edgeAttributes = fromList [(ID "arrowsize",IDDouble 0.5)], globalAttributes = fromList [(ID "rankdir",IDQuoted "TB")], nodes = [NodeStatement {nodeID = IDInt 2, port = Nothing, nodeAttrs = fromList []},NodeStatement {nodeID = IDInt 1, port = Nothing, nodeAttrs = fromList []}], edges = [EdgeStatement {edgeOp = EdgeDirected, leftEdge = EdgeID (IDInt 1) Nothing, rightEdges = EdgeID (IDInt 2) Nothing :| [], edgeAttrs = fromList []}], subgraphs = []}

    g1' <- processGraph g1
    pPrint $ dotPrint defaultDotConfig g1'

    "digraph {
        node [height=0.5;label="\N";shape=circle]
        graph [bb="0,0,36,108";overlap=false;rankdir=TB;size="1!";splines=spline]
        edge [arrowsize=0.5]
        1 [pos="18,90";width=0.5]
        2 [pos="18,18";width=0.5]
        1 -> 2 [pos="e,18,36.104 18,71.697 18,62.881 18,52.032 18,42.474"]
        }"

    c1 = graphToChartWith defaultChartConfig g1' & set (#markupOptions % #chartAspect) ChartAspect & set (#chartTree % charts' % each % #chartStyle % #scaleP) ScalePArea
    display c1
    pPrint c1

    True
    ChartOptions
        { markupOptions = MarkupOptions
            { markupHeight = Just 500.0
            , chartAspect = ChartAspect
            , cssOptions = CssOptions
                { shapeRendering = NoShapeRendering
                , preferColorScheme = PreferHud
                , fontFamilies = "
                  svg { font-family: system-ui,-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,"Noto Sans","Liberation Sans",sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol","Noto Color Emoji";
                  }
    
                  ticktext { font-family: SFMono-Regular,Menlo,Monaco,Consolas,"Liberation Mono","Courier New",monospace;
                  }
    
                  "
                , cssExtra = ""
                }
            , renderStyle = Compact
            }
        , hudOptions = HudOptions
            { axes = []
            , frames = []
            , legends = []
            , titles = []
            }
        , chartTree = ChartTree
            { tree = Node
                { rootLabel =
                    ( Nothing
                    , []
                    )
                , subForest =
                    [ Node
                        { rootLabel =
                            ( Just "edges"
                            ,
                                [ Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour -0.51 0.30 0.37 1.00
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 18.0 71.697
                                        , CubicP Point 18.0 62.881 Point 18.0 52.032 Point 18.0 42.474
                                        , LineP Point 18.0 36.104
                                        ]
                                    }
                                ]
                            )
                        , subForest = []
                        }
                    , Node
                        { rootLabel =
                            ( Nothing
                            , []
                            )
                        , subForest =
                            [ Node
                                { rootLabel =
                                    ( Just "shapes"
                                    ,
                                        [ Chart
                                            { chartStyle = Style
                                                { size = 36.0
                                                , borderSize = 0.5
                                                , color = Colour 0.02 0.73 0.80 0.20
                                                , borderColor = Colour -0.51 0.30 0.37 1.00
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = CircleGlyph
                                                }
                                            , chartData = GlyphData
                                                [ Point 18.0 90.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 36.0
                                                , borderSize = 0.5
                                                , color = Colour 0.02 0.73 0.80 0.20
                                                , borderColor = Colour -0.51 0.30 0.37 1.00
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = CircleGlyph
                                                }
                                            , chartData = GlyphData
                                                [ Point 18.0 18.0 ]
                                            }
                                        ]
                                    )
                                , subForest = []
                                }
                            , Node
                                { rootLabel =
                                    ( Just "labels"
                                    ,
                                        [ Chart
                                            { chartStyle = Style
                                                { size = 14.0
                                                , borderSize = 1.0e-2
                                                , color = Colour -0.51 0.30 0.37 1.00
                                                , borderColor = Colour 0.02 0.29 0.48 1.00
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = NoEscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = SquareGlyph
                                                }
                                            , chartData = TextData
                                                [
                                                    ( "1"
                                                    , Point 18.0 86.3
                                                    )
                                                ,
                                                    ( "2"
                                                    , Point 18.0 14.3
                                                    )
                                                ]
                                            }
                                        ]
                                    )
                                , subForest = []
                                }
                            ]
                        }
                    ]
                }
            }
        }

    -- :t c1 & view #chartTree & view safeStyleBox'
    c1 & forgetHud & view #chartTree & view safeStyleBox'

    Rect (-0.16820276497695852) 0.16820276497695852 (-0.5) 0.5


<a id="org8ad2403"></a>

## testAll

    import Data.Proxy
    testDotParser (Proxy :: Proxy Graph) defaultDotConfig ex0
    testAll

    ex0
    ex1
    ex2
    ex3
    ex4
    ex5
    ex6
    ex7
    ex8
    ex9
    ex10
    ex11
    ex12
    ex13
    ex14
    ex15


<a id="orge431ae1"></a>

## nh

    import DotParse.Examples.NumHask qualified as NH
    g <- processGraph (NH.dotGraphNH Directed)
    display $ (graphToChartWith defaultChartConfig NH.toLink g)

    True

    import DotParse.Examples.NumHask qualified as NH
    g <- processGraph (NH.dotGraphNH Directed)
    display $ (graphToChartWith (defaultChartConfig & set #textSize 12 & set #chartVshift (-4)) (T.pack . label) g)

    True

toLink

    import DotParse.Examples.NumHask qualified as NH
    g <- processGraph (NH.dotGraphNH Directed)
    cNH = graphToChartWith (defaultChartConfig & set #textSize 12 & set #vshift (-4)) NH.toLink g & over (#chartTree % charts' % each) (\c -> c & bool (set (#chartStyle % #size) 0) id (isNothing . view (#chartData % textData') $ c))
    cNH' = forgetHud cNH & set (#markupOptions % #chartAspect) UnscaledAspect & over (#chartTree % charts' % each) (\c -> c & bool (set (#chartStyle % #size) 0.03) id (isNothing . view (#chartData % textData') $ c))
    display cNH'

    True

    import DotParse.Examples.NumHask qualified as NH
    g <- processGraph (NH.dotGraphNH Directed)
    cNH = graphToChartWith (defaultChartConfig & set #textSize 12 & set #chartVshift (-4)) NH.toLinkNH g & set (#markupOptions % #chartAspect) ChartAspect
    display cNH

    True


<a id="org32b7ca8"></a>

### cNH deconstruction

    ss = cNH & toListOf (#chartTree % charts' % each) & fmap sbox
    l = cNH & toListOf (#chartTree % charts' % each)
    z = reverse l & drop 1
    view styleBox' (unnamed z)
    sbox <$> take 1 z
    take 1 z

    Just Rect (-0.500160000000001) 373.00016 (-0.5) 324.5
    [Just Rect (-0.500160000000001) 67.50016 (-0.5) 36.5]
    [Chart {chartStyle = Style {size = 67.00032, borderSize = 1.0, color = Colour 0.50 0.50 0.50 0.20, borderColor = Colour 0.40 0.40 0.40 0.80, scaleP = ScalePArea, anchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = RectSharpGlyph 0.5373108665749656}, chartData = GlyphData [Point 33.5 18.0]}]

    pPrint cNH

    ChartOptions
        { markupOptions = MarkupOptions
            { markupHeight = Just 500.0
            , chartAspect = ChartAspect
            , cssOptions = CssOptions
                { shapeRendering = NoShapeRendering
                , preferColorScheme = PreferHud
                , fontFamilies = "
                  svg { font-family: system-ui,-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,"Noto Sans","Liberation Sans",sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol","Noto Color Emoji";
                  }
    
                  ticktext { font-family: SFMono-Regular,Menlo,Monaco,Consolas,"Liberation Mono","Courier New",monospace;
                  }
    
                  "
                , cssExtra = ""
                }
            , renderStyle = Compact
            }
        , hudOptions = HudOptions
            { axes = []
            , frames = []
            , legends = []
            , titles = []
            }
        , chartTree = ChartTree
            { tree = Node
                { rootLabel =
                    ( Nothing
                    , []
                    )
                , subForest =
                    [ Node
                        { rootLabel =
                            ( Just "edges"
                            ,
                                [ Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 226.42 180.3
                                        , CubicP Point 218.3 189.63 Point 208.19 201.23 Point 199.53 211.17
                                        , LineP Point 195.41 215.9
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 170.71 180.3
                                        , CubicP Point 172.35 189.12 Point 174.37 199.97 Point 176.14 209.53
                                        , LineP Point 177.32 215.9
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 111.24 180.3
                                        , CubicP Point 123.74 189.81 Point 139.34 201.68 Point 152.57 211.75
                                        , LineP Point 157.7 215.65
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 192.37 252.3
                                        , CubicP Point 198.64 261.46 Point 206.42 272.81 Point 213.16 282.62
                                        , LineP Point 216.77 287.9
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 162.95 252.3
                                        , CubicP Point 153.41 261.71 Point 141.51 273.44 Point 131.37 283.44
                                        , LineP Point 126.85 287.9
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 76.376 252.3
                                        , CubicP Point 82.131 261.46 Point 89.263 272.81 Point 95.434 282.62
                                        , LineP Point 98.749 287.9
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 180.86 36.303
                                        , CubicP Point 175.81 45.374 Point 169.55 56.596 Point 164.12 66.35
                                        , LineP Point 161.03 71.896
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 124.31 96.197
                                        , CubicP Point 98.963 102.64 Point 62.591 116.35 Point 45.5 144.0
                                        , CubicP Point 33.05 164.14 Point 41.968 191.12 Point 51.539 210.08
                                        , LineP Point 54.508 215.68
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 178.63 101.79
                                        , CubicP Point 204.65 112.24 Point 244.97 128.71 Point 279.5 144.0
                                        , CubicP Point 280.37 144.38 Point 281.25 144.78 Point 282.13 145.18
                                        , LineP Point 288.05 147.87
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 336.58 108.3
                                        , CubicP Point 332.98 117.29 Point 328.55 128.39 Point 324.67 138.08
                                        , LineP Point 322.34 143.9
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 252.78 36.483
                                        , CubicP Point 231.94 47.292 Point 205.3 61.103 Point 184.58 71.849
                                        , LineP Point 178.9 74.794
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 290.06 36.412
                                        , CubicP Point 295.26 61.887 Point 304.96 109.37 Point 310.76 137.79
                                        , LineP Point 312.01 143.89
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 122.14 36.303
                                        , CubicP Point 127.19 45.374 Point 133.45 56.596 Point 138.88 66.35
                                        , LineP Point 141.97 71.896
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 288.18 177.17
                                        , CubicP Point 268.52 187.36 Point 241.71 201.26 Point 219.76 212.65
                                        , LineP Point 214.0 215.63
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 312.29 180.3
                                        , CubicP Point 310.65 189.12 Point 308.63 199.97 Point 306.86 209.53
                                        , LineP Point 305.68 215.9
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 284.21 252.3
                                        , CubicP Point 274.34 261.64 Point 262.05 273.26 Point 251.53 283.21
                                        , LineP Point 246.84 287.65
                                        ]
                                    }
                                , Chart
                                    { chartStyle = Style
                                        { size = 6.0e-2
                                        , borderSize = 1.0
                                        , color = Colour 0.00 0.00 0.00 0.00
                                        , borderColor = Colour 0.40 0.40 0.40 0.80
                                        , scaleP = ScalePArea
                                        , anchor = AnchorMiddle
                                        , rotation = Nothing
                                        , translate = Nothing
                                        , escapeText = EscapeText
                                        , frame = Nothing
                                        , lineCap = Nothing
                                        , lineJoin = Nothing
                                        , dasharray = Nothing
                                        , dashoffset = Nothing
                                        , hsize = 0.6
                                        , vsize = 1.1
                                        , vshift = -0.25
                                        , glyphShape = SquareGlyph
                                        }
                                    , chartData = PathData
                                        [ StartP Point 62.972 36.483
                                        , CubicP Point 79.923 46.539 Point 101.25 59.193 Point 118.73 69.561
                                        , LineP Point 124.02 72.699
                                        ]
                                    }
                                ]
                            )
                        , subForest = []
                        }
                    , Node
                        { rootLabel =
                            ( Nothing
                            , []
                            )
                        , subForest =
                            [ Node
                                { rootLabel =
                                    ( Just "shapes"
                                    ,
                                        [ Chart
                                            { chartStyle = Style
                                                { size = 58.00032
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.6206862306966582
                                                }
                                            , chartData = GlyphData
                                                [ Point 241.5 162.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 63.24984
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.569171400275479
                                                }
                                            , chartData = GlyphData
                                                [ Point 228.5 306.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 54.0
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.6666666666666666
                                                }
                                            , chartData = GlyphData
                                                [ Point 167.5 162.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 67.00032
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.5373108665749656
                                                }
                                            , chartData = GlyphData
                                                [ Point 88.5 162.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 80.5032
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.44718719255880507
                                                }
                                            , chartData = GlyphData
                                                [ Point 180.5 234.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 61.75008
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.5829951961195841
                                                }
                                            , chartData = GlyphData
                                                [ Point 65.5 234.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 65.49983999999999
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.5496196631930704
                                                }
                                            , chartData = GlyphData
                                                [ Point 190.5 18.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 54.0
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.6666666666666666
                                                }
                                            , chartData = GlyphData
                                                [ Point 151.5 90.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 58.00032
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.6206862306966582
                                                }
                                            , chartData = GlyphData
                                                [ Point 343.5 90.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 93.24719999999999
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.38607057370087255
                                                }
                                            , chartData = GlyphData
                                                [ Point 109.5 306.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 91.0008
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.39560091779412926
                                                }
                                            , chartData = GlyphData
                                                [ Point 286.5 18.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 54.0
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.6666666666666666
                                                }
                                            , chartData = GlyphData
                                                [ Point 112.5 18.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 54.0
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.6666666666666666
                                                }
                                            , chartData = GlyphData
                                                [ Point 315.5 162.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 77.5008
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.4645113340765515
                                                }
                                            , chartData = GlyphData
                                                [ Point 302.5 234.0 ]
                                            }
                                        , Chart
                                            { chartStyle = Style
                                                { size = 67.00032
                                                , borderSize = 1.0
                                                , color = Colour 0.50 0.50 0.50 0.20
                                                , borderColor = Colour 0.40 0.40 0.40 0.80
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = EscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = RectSharpGlyph 0.5373108665749656
                                                }
                                            , chartData = GlyphData
                                                [ Point 33.5 18.0 ]
                                            }
                                        ]
                                    )
                                , subForest = []
                                }
                            , Node
                                { rootLabel =
                                    ( Just "labels"
                                    ,
                                        [ Chart
                                            { chartStyle = Style
                                                { size = 12.0
                                                , borderSize = 1.0e-2
                                                , color = Colour 0.40 0.40 0.40 0.80
                                                , borderColor = Colour 0.02 0.29 0.48 1.00
                                                , scaleP = ScalePArea
                                                , anchor = AnchorMiddle
                                                , rotation = Nothing
                                                , translate = Nothing
                                                , escapeText = NoEscapeText
                                                , frame = Nothing
                                                , lineCap = Nothing
                                                , lineJoin = Nothing
                                                , dasharray = Nothing
                                                , dashoffset = Nothing
                                                , hsize = 0.6
                                                , vsize = 1.1
                                                , vshift = -0.25
                                                , glyphShape = SquareGlyph
                                                }
                                            , chartData = TextData
                                                [
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Action.html#t:Actions">Actions</a>"
                                                    , Point 241.5 158.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Additive.html#t:Additive">Additive</a>"
                                                    , Point 228.5 302.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Metric.html#t:Basis">Basis</a>"
                                                    , Point 167.5 158.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Metric.html#t:Direction">Direction</a>"
                                                    , Point 88.5 158.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Distributive.html#t:Distributive">Distributive</a>"
                                                    , Point 180.5 230.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Multiplicative.html#t:Divisive">Divisive</a>"
                                                    , Point 65.5 230.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Field.html#t:ExpField">ExpField</a>"
                                                    , Point 190.5 14.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Field.html#t:Field">Field</a>"
                                                    , Point 151.5 86.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Data-Integral.html#t:Integral">Integral</a>"
                                                    , Point 343.5 86.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Multiplicative.html#t:Multiplicative">Multiplicative</a>"
                                                    , Point 109.5 302.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Field.html#t:QuotientField">QuotientField</a>"
                                                    , Point 286.5 14.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Data-Rational.html#t:Ratio">Ratio</a>"
                                                    , Point 112.5 14.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Ring.html#t:Ring">Ring</a>"
                                                    , Point 315.5 158.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Additive.html#t:Subtractive">Subtractive</a>"
                                                    , Point 302.5 230.0
                                                    )
                                                ,
                                                    ( "<a href="https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Field.html#t:TrigField">TrigField</a>"
                                                    , Point 33.5 14.0
                                                    )
                                                ]
                                            }
                                        ]
                                    )
                                , subForest = []
                                }
                            ]
                        }
                    ]
                }
            }
        }


<a id="org22709cc"></a>

## AST

    import DotParse.Examples.AST
    gAST = dotAST allSC componentEdges
    C.writeFile "other/ast.dot" $ dotPrint defaultDotConfig gAST
    bsSvg <- processDotWith Directed ["-Tsvg"] (dotPrint defaultDotConfig gAST)
    C.writeFile "other/ast.svg" bsSvg


<a id="orgfe0e9de"></a>

# chartSocketPage

    showRGB light

    "rgb(94%, 94%, 94%)"

    pPrint $ chartSocketPage (Just "test")

    Page
        { libsCss = Markup
            { elements =
                [ Node
                    { rootLabel = OpenTag StartTag "link"
                        [ Attr
                            { attrName = "rel"
                            , attrValue = "stylesheet"
                            }
                        , Attr
                            { attrName = "href"
                            , attrValue = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"
                            }
                        , Attr
                            { attrName = "integrity"
                            , attrValue = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC"
                            }
                        , Attr
                            { attrName = "crossorigin"
                            , attrValue = "anonymous"
                            }
                        ]
                    , subForest = []
                    }
                ]
            }
        , libsJs = Markup
            { elements =
                [ Node
                    { rootLabel = OpenTag StartTag "script"
                        [ Attr
                            { attrName = "src"
                            , attrValue = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js"
                            }
                        , Attr
                            { attrName = "integrity"
                            , attrValue = "sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM"
                            }
                        , Attr
                            { attrName = "crossorigin"
                            , attrValue = "anonymous"
                            }
                        ]
                    , subForest = []
                    }
                , Node
                    { rootLabel = OpenTag StartTag "script"
                        [ Attr
                            { attrName = "src"
                            , attrValue = "https://code.jquery.com/jquery-3.3.1.slim.min.js"
                            }
                        , Attr
                            { attrName = "integrity"
                            , attrValue = "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
                            }
                        , Attr
                            { attrName = "crossorigin"
                            , attrValue = "anonymous"
                            }
                        ]
                    , subForest = []
                    }
                ]
            }
        , cssBody = Css
            { cssByteString = "
            {
              color-scheme: light dark;
            }
            {
              body {
                background-color: #000;
              }
            }
            @media (prefers-color-scheme:dark) {
              body {
                background-color: #fff;
              }
            }" }
        , jsGlobal = Js
            { jsByteString = "" }
        , jsOnLoad = Js
            { jsByteString = "
            window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
            jsb.event = function(ev) {
                jsb.ws.send(JSON.stringify({event: ev}));
            };
            jsb.ws.onmessage = function(evt){
                eval('(function(){' + evt.data + '})()');
            };
    
            function insertScript ($script) {
              var s = document.createElement('script')
              s.type = 'text/javascript'
              if ($script.src) {
                s.onload = callback
                s.onerror = callback
                s.src = $script.src
              } else {
                s.textContent = $script.innerText
              }
    
              // re-insert the script tag so it executes.
              document.head.appendChild(s)
    
              // clean-up
              $script.parentNode.removeChild($script)
            }
    
            function runScripts ($container) {
              // get scripts tags from a node
              var $scripts = $container.querySelectorAll('script')
              $scripts.forEach(function ($script) {
                insertScript($script)
              })
            }
    
            function refreshJsb () {
              $('.jsbClassEventInput').off('input');
              $('.jsbClassEventInput').on('input', (function(){
                jsb.event({ 'element': this.id, 'value': this.value});
              }));
              $('.jsbClassEventChange').off('change');
              $('.jsbClassEventChange').on('change', (function(){
                jsb.event({ 'element': this.id, 'value': this.value});
              }));
              $('.jsbClassEventFocusout').off('focusout');
              $('.jsbClassEventFocusout').on('focusout', (function(){
                jsb.event({ 'element': this.id, 'value': this.value});
              }));
              $('.jsbClassEventButton').off('click');
              $('.jsbClassEventButton').on('click', (function(){
                jsb.event({ 'element': this.id, 'value': this.value});
              }));
              $('.jsbClassEventToggle').off('click');
              $('.jsbClassEventToggle').on('click', (function(){
                jsb.event({ 'element': this.id, 'value': ('true' !== this.getAttribute('aria-pressed')).toString()});
              }));
              $('.jsbClassEventCheckbox').off('click');
              $('.jsbClassEventCheckbox').on('click', (function(){
                jsb.event({ 'element': this.id, 'value': this.checked.toString()});
              }));
              $('.jsbClassEventChooseFile').off('input');
              $('.jsbClassEventChooseFile').on('input', (function(){
                jsb.event({ 'element': this.id, 'value': this.files[0].name});
              }));
              $('.jsbClassEventShowSum').off('change');
              $('.jsbClassEventShowSum').on('change', (function(){
                var v = this.value;
                $(this).parent('.sumtype-group').siblings('.subtype').each(function(i) {
                  if (this.dataset.sumtype === v) {
                    this.style.display = 'block';
                    } else {
                    this.style.display = 'none';
                  }
                })
              }));
              $('.jsbClassEventChangeMultiple').off('change');
              $('.jsbClassEventChangeMultiple').on('change', (function(){
                jsb.event({ 'element': this.id, 'value': [...this.options].filter(option => option.selected).map(option => option.value).join(',')});
              }));
            };
            " }
        , htmlHeader = Markup
            { elements =
                [ Node
                    { rootLabel = OpenTag StartTag "meta"
                        [ Attr
                            { attrName = "charset"
                            , attrValue = "utf-8"
                            }
                        ]
                    , subForest = []
                    }
                , Node
                    { rootLabel = OpenTag StartTag "meta"
                        [ Attr
                            { attrName = "name"
                            , attrValue = "viewport"
                            }
                        , Attr
                            { attrName = "content"
                            , attrValue = "width=device-width, initial-scale=1, shrink-to-fit=no"
                            }
                        ]
                    , subForest = []
                    }
                ]
            }
        , htmlBody = Markup
            { elements =
                [ Node
                    { rootLabel = OpenTag StartTag "div"
                        [ Attr
                            { attrName = "class"
                            , attrValue = "container"
                            }
                        ]
                    , subForest =
                        [ Node
                            { rootLabel = OpenTag StartTag "row"
                                [ Attr
                                    { attrName = "class"
                                    , attrValue = "col"
                                    }
                                ]
                            , subForest =
                                [ Node
                                    { rootLabel = OpenTag StartTag "h4" []
                                    , subForest =
                                        [ Node
                                            { rootLabel = Content "test"
                                            , subForest = []
                                            }
                                        ]
                                    }
                                ]
                            }
                        , Node
                            { rootLabel = OpenTag StartTag "div"
                                [ Attr
                                    { attrName = "id"
                                    , attrValue = "prettychart"
                                    }
                                ]
                            , subForest = []
                            }
                        ]
                    }
                ]
            }
        }


<a id="org3735180"></a>

# v06 Changes

Chart.Compound

Chart.Bar

barTextCharts
textShiftVert

Chart.Hud

-   defaultPriority
-   HudBox
-   CanvasBox
-   ChartBox

-   canvasBox&rsquo;
-   canvasStyleBox&rsquo;
-   hudBox&rsquo;
-   hudStyleBox&rsquo;
-   runHud
-   HudChartSection
-   hudChartBox&rsquo;

-   closes
-   fromEffect
-   applyChartAspect
-   getHudBox

-   appendHud
-   makeHuds
-   projectChartTreeWith
-   addHud
-   finalCanvas

-   defaultAxisOptions
-   defaultXAxisOptions
-   defaultYAxisOptions

-   placeText
-   flipPlace

Title ==> TitleOptions

-   defaultGlyphTick
-   defaultGlyphTickStyleX
-   defaultGlyphTickStyleY

-   defaultTicks
-   defaultXTicks

-   defaultTick

TickStyle ==> Tick

formatN&rsquo;
numTicks&rsquo;
tickExtend&rsquo;

-   axisHud
-   titleHud

-   legend
-   legendFrame

-   freezeAxes
-   freezeTicks

Priority refactor
Hud refactor

Chart.Markup

-   forgetHud

CssPreferColorScheme ==> PreferColorScheme
CssShapeRendering ==> ShapeRendering

-   defaultCssFontFamilies

Primitive

-   ChartData (..),
-   rectData&rsquo;,
-   lineData&rsquo;,
-   glyphData&rsquo;,
-   textData&rsquo;,
-   pathData&rsquo;,
-   blankData&rsquo;,
-   pattern RectChart,
-   pattern LineChart,
-   pattern GlyphChart,
-   pattern TextChart,
-   pattern PathChart,
-   pattern BlankChart,
-   pattern LineChart1,

-   scaleP
-   projectChartDataWith
-   scaleStyle,
-   colourChart,
-   scaleChartData,
-   colourStyle

-   safeBox&rsquo;
-   safeStyleBox&rsquo;
-   overText
-   blankChart

Chart.Style

-   Style (..),
-   defaultStyle,
-   scaleStyle,

gpalette <== gpalette1

-   ScaleP
-   scaleRatio

Data.Colour

palette <== palette1
paletteO <== palette1a

Chart.Surface

-   surfaceLegendChart,
-   surfaceAxisOptions,
-   surfaceLegendAxisOptions,
-   gridReferenceChart,
-   addSurfaceLegend,

Chart.Data

-   singletonGuard
-   isSingleton

axis &#x2013;> axisHud
title &#x2013;> titleHud
legend &#x2013;> legendHud


<a id="org4b89119"></a>

# example problems


<a id="org0ffa7fe"></a>

## pathExample

    display $ pathExample & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #anchorTo) CanvasStyleSection & set (#hudOptions % #axes % each % #item % #bar %? #anchorTo) CanvasStyleSection

    True

    ps = [ StartP (Point 0 0), LineP (Point 1 0), CubicP (Point 0.2 0) (Point 0.25 1) (Point 1 1), QuadP (Point (-1) 2) (Point 0 1), ArcP (ArcInfo (Point 1 1) (-pi / 6) False False) (Point 0 0)]
    ts = [ "StartP (Point 0 0)", "LineP (Point 1 0)", "CubicP (Point 0.2 0) (Point 0.25 1) (Point 1 1)", "QuadP (Point (-1) 2) (Point 0 1)", "ArcP (ArcInfo (Point 1 1) (-pi / 6) False False) (Point 0 0)"]
    path' = PathChart (defaultPathStyle & #color .~ palette1a 0 0.05 & #borderColor .~ palette1a 1 0.3) ps
    c0 = GlyphChart defaultGlyphStyle ((SquareGlyph,) . pointPath <$> ps)
    midp = Point 0 0 : zipWith (\(Point x y) (Point x' y') -> Point ((x + x') / 2) ((y + y') / 2)) (drop 1 (pointPath <$> ps)) (pointPath <$> ps)
    offp = [Point (-0.35) 0.05, Point 0 0.05, Point (-0.2) 0, Point (-0.1) 0.1, Point 0 (-0.1)]
    t0 = TextChart (defaultTextStyle & set #size 0.025) (zip ts (zipWith addp offp midp))
    display $ mempty & #charts .~ named "path" [path', c0] <> named "pathtext" [t0] & #hudOptions .~ defaultHudOptions & #markupOptions % #chartAspect .~ ChartAspect & #markupOptions % #cssOptions % #preferColorScheme .~ PreferHud & #markupOptions % #cssOptions % #cssExtra .~ fillSwitch (dark, light) "dark" "pathtext"

    True


<a id="orgb8dc31e"></a>

## lineExample

    co = lineExample & set (#hudOptions % #legends % each % _2 % #size) 0.2 & set (#hudOptions % #legends % each % _2 % #frame) (Just defaultRectStyle) & set (#hudOptions % #legends % each % _2 % #vgap) 0 & set (#hudOptions % #legends % each % _2 % #outerPad) 0 & set (#hudOptions % #legends % each % _2 % #innerPad) 0 & set (#hudOptions % #legends % each % _2 % #textStyle % #frame) (Just defaultRectStyle) & set (#hudOptions % #legends % each % _2 % #overallScale) 0.5 & set (#hudOptions % #legends % each % _2 % #scaleP) ScalePX
    writeChartOptions "other/line.svg" co
    display co

    True


<a id="org9d30923"></a>

## legends


<a id="org83a8bd3"></a>

### frame bug

-   [X] add scaleP for legendoptions

lineExample legend with zero gaps and padding highlights that:

-   charts scale independently in the X and Y dimensions
-   chart styles scale proportionately.

Thus legends have to choose to compromise by adopting X, Y, Area or MinDim

    lo0 = defaultLegendOptions & set (#textStyle % #frame) (Just defaultRectStyle) & set #vgap 0 & set #hgap 0 & set #outerPad 0 & set #innerPad 0 & set #overallScale 0.2 & set #size 0.3 & set #legendCharts (take 3 $ fromMaybe undefined $ preview (#hudOptions % #legends % ix 0 % _2 % #legendCharts) lineExample) & set #buffer 0
    
    -- manual construction
    cs = legendChart lo0
    view styleBox' cs
    
    c0 = unnamed [RectChart defaultRectStyle [one]]
    cs'' = cs & over (charts' % each) (scaleChart 0.3)
    cs''' = placeLegend lo0 one cs'' & set (charts' % each % #style % #scaleP) ScalePX
    view styleBox' $ set styleBox' (Just one) (c0 <> cs''')
    
    -- automated construction via HudOptions
    display $ (mempty :: ChartOptions) & set #charts c0 & set (#markupOptions % #chartAspect) ChartAspect & set #hudOptions (mempty & set #legends [(100,lo0 & set #scaleP ScalePArea)]) -- defaultHudOptions

    Just Rect -2.5e-3 1.6023999999999998 -9.129999999999999e-2 0.44650000000000006
    Just Rect -0.5 0.5 -0.5 0.5
    True


<a id="org5bc6ef2"></a>

### large text bug

Manual construction and placement for a legend, using ScalePX.

A slight space opens up between the horizontal elements.

    lo0 = defaultLegendOptions & set (#textStyle % #frame) (Just defaultRectStyle) & set (#textStyle % #size) 0.16 & set #vgap 0 & set #hgap 0 & set #outerPad 0 & set #innerPad 0 & set #overallScale 0.2 & set #size 0.2 & set #legendCharts (take 3 $ fromMaybe undefined $ preview (#hudOptions % #legends % ix 0 % _2 % #legendCharts) lineExample) & set #buffer 0 & set #scaleP ScalePX
    
    -- manual construction
    cs = legendChart lo0 & set (charts' % each % #style % #scaleP) (view #scaleP lo0)
    view styleBox' cs
    
    c0 = unnamed [RectChart defaultRectStyle [one]]
    cs'' = cs & over (charts' % each) (scaleChart 0.3)
    cs''' = placeLegend lo0 one cs''
    view styleBox' $ set styleBox' (Just one) (c0 <> cs''')
    
    display $ (mempty :: ChartOptions) & set #charts (c0 <> cs''') & set (#markupOptions % #chartAspect) (FixedAspect 1) & set #hudOptions defaultHudOptions

    Just Rect -2.5e-3 1.4024 -9.129999999999999e-2 0.44650000000000006
    Just Rect -0.5 0.49999999999999994 -0.5 0.5
    True

    :t legendEntry lo0 "palette #0"
    :t fmap (legendizeChart lo0) <$> (toListOf (#charts % charts') lineExample)
    :t view #legendCharts lo0
    :t legendText lo0
    l = defaultLegendOptions & set (#textStyle % #frame) (Just defaultRectStyle) & set (#textStyle % #size) 0.12 & set #vgap 0 & set #hgap 0 & set #outerPad 0 & set #innerPad 0 & set #overallScale 0.2 & set #size 0.2 & set #legendCharts (take 3 $ fromMaybe undefined $ preview (#hudOptions % #legends % ix 0 % _2 % #legendCharts) lineExample) & set #buffer 0
    es = reverse $ uncurry (legendEntry l) <$> view #legendCharts l
    twidth = maybe zero (\(Rect x z _ _) -> z - x) (styleBoxes (fst <$> es))
    gapwidth t = maybe 0 (\(Rect x z _ _) -> z - x) (sbox t)
    twidth
    x1 = vert 0 $ hori 0 <$> (\(t,a) -> [unnamed [t], unnamed a]) <$> es
    x2 = x1 & set (charts' % each % #style % #scaleP) ScalePArea
    display $ (mempty :: ChartOptions) & set #charts x2 & set (#markupOptions % #chartAspect) ChartAspect & set #hudOptions defaultHudOptions

    legendEntry lo0 "palette #0" :: [Chart] -> (Chart, [Chart])
    fmap (legendizeChart lo0) <$> (toListOf (#charts % charts') lineExample)
      :: [[Chart]]
    view #legendCharts lo0 :: [(Text, [Chart])]
    legendText lo0 :: Text -> Chart
    0.7212000000000001
    True


<a id="orga08a5c8"></a>

## surface legend

    display surfaceExample

    True

scale and move basic charts

    c' = [RectChart (defaultRectStyle & set #scaleP NoScaleP)  [one]]
    cs = (mconcat [named "left" c', named "right" c' & over (charts' % each) (scaleChart 0.5 >>> moveChart (Point 0.8 0.25))])
    display $ (mempty :: ChartOptions) & set #charts cs & set #hudOptions defaultHudOptions

    True

scale and move legend

    slc = surfaceLegendChart (Range (-0.5) 0.5) (defaultSurfaceLegendOptions dark "surface" & set (#sloLegendOptions % #vgap) 0.1 & set (#sloLegendOptions % #size) 0.6 & set (#sloLegendOptions % #hgap) 0 & set (#sloLegendOptions % #textStyle % #frame) (Just defaultRectStyle) & set (#sloAxisOptions % #ticks % #gtick) (Just (defaultGlyphTick, HLineGlyph, (-0.014))) & set (#sloAxisOptions % #ticks % #ttick) (Just (defaultTextTick, -0.005))) & set (charts' % each % #style % #scaleP) ScaleMinDim
    display $ (mempty :: ChartOptions) & set #charts (mconcat [named "proxy" c', slc & set (charts' % each % #style % #scaleP) ScalePArea & over (charts' % each) (scaleChart 1 >>> moveChart (Point 0.6 (-0.3)))]) & set #hudOptions defaultHudOptions

    True

    grain = Point 100 100
    r = one
    f = fst . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    evenColors = trimColour . over lightness' (const 0.55) . palette1 <$> [0 .. 5]
    so = defaultSurfaceOptions & #soGrain .~ grain & #soRange .~ r & #soStyle % #surfaceColors .~ evenColors
    (cs, rangef) = surfacef f so
    slo = defaultSurfaceLegendOptions dark "surface" & set #sloWidth 0.1 & set (#sloStyle % #surfaceColors) evenColors & set (#sloLegendOptions % #vgap) 0.1 & set (#sloLegendOptions % #size) 0.6 & set (#sloLegendOptions % #hgap) 0 & set (#sloLegendOptions % #textStyle % #frame) (Just defaultRectStyle) & set (#sloAxisOptions % #ticks % #gtick) (Just (defaultGlyphTick, HLineGlyph, (-0.014))) & set (#sloAxisOptions % #ticks % #ttick) (Just (defaultTextTick, -0.005))
    
    slc = surfaceLegendChart rangef slo & set (charts' % each % #style % #scaleP) ScaleMinDim
    -- display $ (mempty :: ChartOptions) & set #charts (mconcat [named "surface" cs, slc & set (charts' % each % #style % #scaleP) ScalePArea & over (charts' % each) (scaleChart 1 >>> moveChart (Point 0.6 (-0.3)))]) & set #hudOptions defaultHudOptions
    display $ (mempty :: ChartOptions) & set #charts (mconcat [slc]) & set #hudOptions defaultHudOptions & #markupOptions .~ (defaultMarkupOptions & #cssOptions % #shapeRendering .~ UseCssCrisp)

addSurfaceLegend version

    grain = Point 100 100
    r = one
    f = fst . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    evenColors = trimColour . over lightness' (const 0.55) . palette1 <$> [0 .. 5]
    so = defaultSurfaceOptions & #soGrain .~ grain & #soRange .~ r & #soStyle % #surfaceColors .~ evenColors
    (cs, rangef) = surfacef f so
    slo = defaultSurfaceLegendOptions dark "surface" & set #sloWidth 0.1 & set (#sloStyle % #surfaceColors) evenColors & set (#sloLegendOptions % #vgap) 0.1 & set (#sloLegendOptions % #size) 0.6 & set (#sloLegendOptions % #hgap) 0 & set (#sloLegendOptions % #textStyle % #frame) (Just defaultRectStyle) & set (#sloAxisOptions % #ticks % #gtick) (Just (defaultGlyphTick, HLineGlyph, (-0.014))) & set (#sloAxisOptions % #ticks % #ttick) (Just (defaultTextTick, -0.005))
    
    slc = surfaceLegendChart rangef slo & set (charts' % each % #style % #scaleP) ScaleMinDim
    -- display $ (mempty :: ChartOptions) & set #charts (mconcat [named "surface" cs, slc & set (charts' % each % #style % #scaleP) ScalePArea & over (charts' % each) (scaleChart 1 >>> moveChart (Point 0.6 (-0.3)))]) & set #hudOptions defaultHudOptions
    display $ (mempty :: ChartOptions) & set #charts (mconcat [slc]) & set #hudOptions defaultHudOptions & #markupOptions .~ (defaultMarkupOptions & #cssOptions % #shapeRendering .~ UseCssCrisp)

    True

-   [ ] addHud
-   [ ] projectChartTree
-   [ ] mconcat with main chart

co version

    display $ (mempty :: ChartOptions) & set #charts (gridReferenceChart rangef slo) & set #hudOptions (mempty & set #axes [(1, view #sloAxisOptions slo & set #place PlaceRight)]) & set #markupOptions (defaultMarkupOptions & set (#cssOptions % #shapeRendering) UseCssCrisp) & set (#markupOptions % #chartAspect) (FixedAspect 0.2)

    True

charttree version

    grain = Point 100 100
    r = one
    f = fst . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    evenColors = trimColour . over lightness' (const 0.55) . palette1 <$> [0 .. 5]
    so = defaultSurfaceOptions & #soGrain .~ grain & #soRange .~ r & #soStyle % #surfaceColors .~ evenColors
    (cs, rangef) = surfacef f so
    
    slo = defaultSurfaceLegendOptions & set (#sloSurfaceStyle % #surfaceColors) evenColors
    grc = gridReferenceChart rangef slo
    hoLegend = (mempty :: HudOptions) & set #axes [(1, view #sloAxisOptions slo)]
    grcLegend = addHud (FixedAspect (view #sloWidth slo)) hoLegend grc
    ct = view #charts surfaceExample
    ctbox = fromMaybe one (view styleBox' ct)
    legbox = projectOnR ctbox one (view #sloRect slo)
    ctBoth = mconcat [projectChartTree legbox grcLegend, ct]
    display $ (mempty :: ChartOptions) & set #charts ctBoth & set #markupOptions (defaultMarkupOptions & set (#cssOptions % #shapeRendering) UseCssCrisp) & set (#markupOptions % #chartAspect) ChartAspect & set #hudOptions defaultHudOptions

    True

addSurfaceLegend version

    grain = Point 20 20
    r = one
    f = fst . bimap ((-1.0)
    slo = defaultSurfaceLegendOptions & set (#sloSurfaceStyle % #surfaceColors) evenColors & set (#sloDataRange) rangef
    
    cs' = addSurfaceLegend slo (unnamed cs)
    
    display $ (mempty :: ChartOptions) & set #charts cs' & set #markupOptions (defaultMarkupOptions & set (#cssOptions % #shapeRendering) UseCssCrisp) & set (#markupOptions % #chartAspect) ChartAspect & set #hudOptions defaultHudOptions

    True


<a id="orgca313e4"></a>

## compoundExample

    display compoundExample

    True

-   [X] try a no extend
-   [X] try a ScalePArea
-   [X] simplest decompose


<a id="orge773512"></a>

### original compoundExample

    ts = TickRound (FormatN FSCommaPrec (Just 1) 4 True True) 5 TickExtend
    tsf = set (#hudOptions % #axes % each % _2 % #ticks % #style) ts
    sap = set (#charts % charts' % each % #style % #scaleP) ScalePArea
    co = compoundMerge [lineExample & tsf & sap, unitExample & tsf & sap & #hudOptions % #axes %~ fmap (_2 % #place %~ flipPlace)]
    display co

    True


<a id="org9bb3ef8"></a>

### simple experiment

-   [X] titles ok
-   [X] noextend axes ok
-   [X] extend axes

    ts = TickRound (FormatN FSCommaPrec (Just 1) 4 True True) 4 NoTickExtend
    tse = TickRound (FormatN FSCommaPrec (Just 1) 4 True True) 4 TickExtend
    tsf = set (#axes % each % _2 % #ticks % #style)
    sap = set (#charts % charts' % each % #style % #scaleP) ScalePArea
    ho1 = (mempty :: HudOptions) & set #titles [(3,defaultTitle "chart1")] & set #axes [(2,defaultXAxisOptions), (2,defaultYAxisOptions)] & tsf ts & colourHudOptions (const (palette1 0))
    c1 = (mempty :: ChartOptions) & set #hudOptions ho1 & set #charts (named "c1" [Chart defaultRectStyle (RectData [fmap (2*) one])])
    
    ho2 = (mempty :: HudOptions) & set #titles [(3.1,defaultTitle "chart2")] & set #axes [(2,defaultXAxisOptions & set #place PlaceTop), (2,defaultYAxisOptions & set #place PlaceRight)] & tsf ts & colourHudOptions (const (palette1 3))
    c2 = (mempty :: ChartOptions) & set #hudOptions ho2 & set #charts (named "c2" [Chart (blob (set opac' 0.3 $ palette1 3)) (RectData [fmap (*0.8) one]), BlankChart defaultStyle [one]])
    co = compoundMerge [c1,c2]
    display co

    True


<a id="org343fb42"></a>

### new example

    ho1 = (mempty :: HudOptions) & set #titles [(3,defaultTitle "chart1")] & set #axes [(2,defaultXAxisOptions), (2,defaultYAxisOptions)] & colourHudOptions (const (palette1 0))
    c1 = (mempty :: ChartOptions) & set #hudOptions ho1 & set #charts (named "c1" [Chart defaultRectStyle (RectData [fmap (2*) one])])
    
    ho2 = (mempty :: HudOptions) & set #titles [(3.1,defaultTitle "chart2")] & set #axes [(2,defaultXAxisOptions & set #place PlaceTop), (2,defaultYAxisOptions & set #place PlaceRight)] & colourHudOptions (const (palette1 3))
    c2 = (mempty :: ChartOptions) & set #hudOptions ho2 & set #charts (named "c2" [Chart (blob (set opac' 0.3 $ palette1 3)) (RectData [fmap (*0.8) one]), BlankChart defaultStyle [one]])
    co = compoundMerge [c1,c2]
    display co

    True


<a id="org5ecca23"></a>

## stackExample

    display $ mempty & set #charts (stack 4 0.1 (replicate 16 $ (view #charts $ (set (#charts % charts' % each % #style % #scaleP) ScalePArea) $ forgetHud lineExample)))

    True


<a id="orga1925d7"></a>

## bar & sbar

-   [X] numbers are badly placed on both X and Y axis
-   [X] Not due to negative
-   [X] numbers a bit small
-   [X] vgap on legend
-   [X] legend skewif
-   [X] bar Hori text is still Vert
-   [X] Stacked not showing second series
-   [X] bar Hori axis is wrong, extending beyond the original

zeroised

    e1 = barDataExample & over #barData (fmap (fmap (max 1))) & over #barData (fmap (take 4))
    display $ barChart (defaultBarOptions & set (#barTextStyles % each % #anchor) AnchorMiddle & set (#barTextStyles % each % #size) 0.2 & set #textGap 0 & set #textGapNegative 0) e1 & set (#markupOptions % #chartAspect) (FixedAspect 1) & set (#charts % charts' % each % #style % #scaleP) ScalePArea

    True

too dependent on original barRect scale

Hori

    n = 1
    barDataExample' = barDataExample & over #barData (fmap (fmap (*n)))
    bo = (defaultBarOptions & set (#barOrientation) Hori & set (#barTextStyles % each % #anchor) AnchorMiddle & set (#barTextStyles % each % #size) 0.03 & set #textGap 0.03 & set #textGapNegative 0.05) & set #textShiftVert (-0.008)
    display $ barChart bo barDataExample'

    True

Vert

    n = 1
    barDataExample' = barDataExample & over #barData (fmap (fmap (*n)))
    bo = (defaultBarOptions & set (#barOrientation) Vert & set (#barTextStyles % each % #size) 0.03 & set #textGap 0.03 & set #textGapNegative 0.05) & set #textShiftVert (-0.008)
    display $ barChart bo barDataExample' & set (#markupOptions % #chartAspect) (FixedAspect 1.5) & set (#charts % charts' % each % #style % #scaleP) ScalePArea & set (#hudOptions % #frames) [(101, defaultFrameOptions & set #buffer 0.02)] & set (#hudOptions % #legends) []

    True

    barRects bo (view #barData barDataExample')
    barTexts bo (view #barData barDataExample')


<a id="org17585d1"></a>

## ellipse & quad & cubic

-   [X] title
-   [X] yaxis ticks
-   [X] quad
-   [X] cubic


<a id="org3a00d72"></a>

## textExample

-   [X] bad y axis ticks
-   [X] funny axis bar

    co = textExample
    display co

    True

    co & view #chartTree & view box'

    Just Rect 0.0 0.9995736030415051 0.0 25.0

    forgetHud co & toListOf (#chartTree % charts' % each % #style % #scaleP)

    [ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea,ScalePArea]


<a id="org75a11fb"></a>

## higher number of ticks

    co = unitExample & set (#hudOptions % #axes % each % _2 % #ticks % #style % numTicks') (Just 8) & over (#charts % charts' % each % #chartData) (scaleChartData 1)
    display co

    True


<a id="orgee7980e"></a>

## dateExample

-   [X] y axis ticks being cut off

    display $ dateExample & set (#hudOptions % #frames) [(100,defaultFrameOptions & set #buffer 0.05)]

    True


<a id="org30b99cf"></a>

# Exact reproduction of proportionate scaling

Scaling of style elements is proportional to the ratio of areas of the before and after rectangle. This means that individual elements do not scale to the exact proportions of the overall projections.

The effect is typically small but in pathological instances can cause irritation.

An extreme example, where:

-   tick and text marks fail to scale properly, if NoScaleP (the default) is used.
-   tick marks (almost) scale on ScalePArea, but text tick fails, because of an auto change in format

    scale = NoScaleP
    asp = FixedAspect 2
    cszero =  (unnamed [blankChart1 one]) & over (charts' % each % #chartData) (scaleChartData 1)
    r1 = fmap (*1) (Rect 0 1 0 1)
    bar' = (AxisBar (border 0.001 (grey 0.3 1)) 0.05 0 0)
    rs1 = border 0.001 (grey 0.3 1)
    tt = (defaultTextTick & set #scaleP scale,0)
    gt = defaultGlyphTick & set #scaleP scale & set #borderSize 0.001 & set #color (grey 0.3 1) & set #size 0.1
    axes0 = [(5,defaultYAxisOptions & set #place PlaceLeft), (5,defaultXAxisOptions & set #place PlaceBottom)] & set (each % _2 % #ticks % #ltick) Nothing & set (each % _2 % #bar) (Just bar') & set (each % _2 % #ticks % #ttick) (Just tt) & set (each % _2 % #ticks % #ttick %? _2) 0 & set (each % _2 % #ticks % #gtick %? _1) gt & set (each % _2 % #ticks % #gtick %? _3) 0.0 & set (each % _2 % #ticks % #ttick %? _1 % #frame) (Just (border 0.005 black))
    cozero = (mempty :: ChartOptions) & set #charts cs & set (#hudOptions % #axes) axes0 & set (#hudOptions % #frames) [(1,defaultFrameOptions & set #frame (Just rs1))] & set (#markupOptions % #chartAspect) asp
    display cozero

    True


<a id="orgd18afd6"></a>

# projectChart

-   [X] get CanvasAspect working
-   [X] find a non-exact single projection
-   [X] styleRebox using jam


<a id="org02a0fc7"></a>

## rebox code

styleRebox is `projectWith (r - (styleBox - box)) box`
projectChartTree is `projectWith r styleBox`

    styleBox_ :: ChartTree -> Maybe (Rect Double)
    styleBox_ = styleBoxes . foldOf charts'
    
    styleRebox_ :: ChartTree -> Maybe (Rect Double) -> ChartTree
    styleRebox_ cs r =
      cs
        & over chart' (fromMaybe id $ projectWith <$> r' <*> box_ cs)
      where
        r' = (NH.-) <$> r <*> ((NH.-) <$> styleBox_ cs <*> box_ cs)
    
    -- | Lens between a style bounding box and a ChartTree tree.
    --
    -- Note that a round trip may be only approximately isomorphic ie
    --
    -- > forall c r. \c -> view styleBox' . set styleBox' r c ~= r
    styleBox' :: Lens' ChartTree (Maybe (Rect Double))
    styleBox' =
      lens styleBox_ styleRebox_

    -- | Project a chart tree to a new bounding box, guarding against singleton bounds.
    projectChartTree :: Rect Double -> ChartTree -> ChartTree
    projectChartTree new ct = case view styleBox' ct of
      Nothing -> ct
      Just b -> ct & over charts' (fmap (projectWith new b))
    -- | projects a Chart to a new space from an old rectangular space, preserving linear metric structure.
    --
    -- FIXME: test singleton protections
    --
    -- >>> projectWith (fmap (2*) one) one r
    -- RectChart (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.02 0.29 0.48 1.00, color = Colour 0.02 0.73 0.80 0.10}) [Rect -1.0 1.0 -1.0 1.0]
    projectWith :: Rect Double -> Rect Double -> Chart -> Chart
    projectWith new old (Chart s a) =
      Chart (scaleStyle (scaleRatio (view #scaleP s) new old) s) (projectChartDataWith new old a)
    
    projectChartDataWith :: Rect Double -> Rect Double -> ChartData -> ChartData
    projectChartDataWith new old (RectData a) = RectData (projectOnR new old <$> a)
    projectChartDataWith new old (TextData a) = TextData (second (projectOnP new old) <$> a)
    projectChartDataWith new old (LineData a) = LineData (fmap (projectOnP new old) <$> a)
    projectChartDataWith new old (GlyphData a) = GlyphData (fmap (second (projectOnP new old)) a)
    projectChartDataWith new old (PathData a) = PathData (projectPaths new old a)
    projectChartDataWith new old (BlankData a) = BlankData (projectOnR new old <$> a)


<a id="orgca62762"></a>

## projection decomp

    co = jal
    -- co = tandp
    -- co = lineExample & set (#hudOptions % #legends % each % _2 % #place) PlaceRight & set (#markupOptions % #chartAspect) (CanvasAspect 1.5)
    asp = co & view (#markupOptions % #chartAspect)
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    finalCT = projectChartTreeN 4 viewbox csAndHud
    boxs' = sbox <$> (mconcat $ toListOf charts' finalCT)
    ct' = projectChartTree viewbox csAndHud
    ct'' = set styleBox' (Just viewbox) csAndHud
    putStrLn ("initial:  " <> show (initialCanvas asp Nothing))
    putStrLn ("csAndHud: " <> maybe "" show (view styleBox' csAndHud))
    putStrLn ("single:   " <> maybe "" show (view styleBox' ct'))
    putStrLn ("final:    " <> maybe "" show (view styleBox' finalCT))
    putStrLn ("rebox:    " <> maybe "" show (view styleBox' (set styleBox' (Just viewbox) csAndHud)))
    ct' == ct''
    display (mempty & set #charts csAndHud & set (#markupOptions % #chartAspect) ChartAspect)

    initial:  Rect -0.5 0.5 -0.5 0.5
    csAndHud: Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    single:   Rect -0.5387155110912017 0.8141468207614003 -0.5 0.5
    final:    Rect -0.5 0.5326563466786947 -0.5 0.5
    rebox:    Rect -0.5387155110912017 0.8141468207614003 -0.5 0.5
    True
    True


<a id="orga396d22"></a>

## multi bulk test

    pPrint $ filter ((\(x,_,_) -> not x) . snd) $ second (sameMulti) <$> pathChartOptions

    [
        ( "other/text.svg"
        ,
            ( False
            , Just Rect -0.75 0.75 -0.5 0.5
            , Just Rect -0.75 0.7499999999999998 -0.5 0.5
            )
        )
    ,
        ( "other/sbar.svg"
        ,
            ( False
            , Just Rect -0.75 0.75 -0.5 0.5
            , Just Rect -0.75 0.75 -0.49999999999999994 0.5
            )
        )
    ,
        ( "other/wave.svg"
        ,
            ( False
            , Just Rect -0.75 0.75 -0.5 0.5
            , Just Rect -0.75 0.7499999999999998 -0.5 0.5
            )
        )
    ,
        ( "other/quad.svg"
        ,
            ( False
            , Just Rect -0.75 0.75 -0.5 0.5
            , Just Rect -0.75 0.7499999999999998 -0.5 0.5
            )
        )
    ,
        ( "other/priorityv1.svg"
        ,
            ( False
            , Just Rect -0.75 0.75 -0.5 0.5
            , Just Rect -0.75 0.7500000000000002 -0.5 0.5
            )
        )
    ,
        ( "other/priorityv2.svg"
        ,
            ( False
            , Just Rect -0.75 0.75 -0.5 0.5
            , Just Rect -0.75 0.7500000000000002 -0.5 0.5
            )
        )
    ]


<a id="org1a6ea8a"></a>

# jam

    exampleText = ["jam"]
    tsScale = defaultTextStyle & set #frame (Just defaultRectStyle) & set #anchor AnchorMiddle & set #scaleP ScaleMinDim
    textScale = zipWith (\t x -> TextChart tsScale [(t, Point 0 x)]) exampleText [0..]
    ct = unnamed textScale
    jam = mempty & #charts .~ ct & set (#hudOptions % #frames) [(100,defaultFrameOptions & set #buffer 0 & set #frame (Just $ blob (grey 0.5 0.1)))] & set (#markupOptions % #chartAspect) (FixedAspect 2) :: ChartOptions
    display jam

    True


<a id="org39ba0ac"></a>

## unscaled + no hud

    co = jam & set (#markupOptions % #chartAspect) UnscaledAspect & set #hudOptions mempty
    display co
    
    asp = co & view (#markupOptions % #chartAspect)
    icanvas = initialCanvas asp Nothing
    cs = view #charts co
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    csAndHudSingle = set styleBox' (Just viewbox) csAndHud
    csm = set (styleBoxN' 10) (Just viewbox) csAndHud
    csp = projectChartWith (view (#markupOptions % #repeatAspect) co) (view (#markupOptions % #chartAspect) co) (view #hudOptions co) cs
    
    -- addHud
    ho = view #hudOptions co
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    csPadded = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
    ivb = initialCanvas asp (Just csPadded)
    db' = fromMaybe db mdb
    csAndHud' = runHudWith ivb db' hs csPadded
    hc0 = cs & set styleBox' (Just ivb)
    
    -- projectWith
    new = ivb
    old = fromMaybe one $ view styleBox' csPadded
    csPaddeds = toListOf charts' csPadded & mconcat
    pwData = csPaddeds & over (each % #chartData) (projectChartDataWith new old)
    pwC = pwData & over (each % #style) (\s -> scaleStyle (scaleRatio (view #scaleP s) new old) s)
    pwRatio = scaleRatio (view #scaleP (head $ view #style <$> pwC)) new old
    pwStyle = view #style (head pwC)
    pwC' = unnamed pwC
    
    
    csp & view styleBox' & NH.traverse_ (show >>> ("co:" <>) >>> putStrLn)
    
    icanvas & (show >>> ("initial canvas:" <>) >>> putStrLn)
    cs & view styleBox' & NH.traverse_ (show >>> ("initial chart:" <>) >>> putStrLn)
    csAndHud & view styleBox' & NH.traverse_ (show >>> ("csAndHud:" <>) >>> putStrLn)
    viewbox & (show >>> ("final canvas:" <>) >>> putStrLn)
    csAndHudSingle & view styleBox' & NH.traverse_ (show >>> ("single proj:" <>) >>> putStrLn)
    csm & view styleBox' & NH.traverse_ (show >>> ("multi proj:" <>) >>> putStrLn)
    csPadded & view styleBox' & NH.traverse_ (show >>> ("padding:" <>) >>> putStrLn)
    ivb & (show >>> ("initial padded canvas:" <>) >>> putStrLn)
    hc0 & view styleBox' & NH.traverse_ (show >>> ("hc0:" <>) >>> putStrLn)
    csAndHud' & view styleBox' & NH.traverse_ (show >>> ("runHudWith:" <>) >>> putStrLn)
    
    -- projectWith
    ratio new & (show >>> ("ratio new:" <>) >>> putStrLn)
    ratio old & (show >>> ("ratio old:" <>) >>> putStrLn)
    pwRatio & (show >>> ("scale ratio:" <>) >>> putStrLn)
    
    
    db' & (show >>> ("data box padded:" <>) >>> putStrLn)
    
    svgViewbox (Rect x z y w) = (x, (-w), (z-x), (w-y))
    svgvb = svgViewbox <$> (view styleBox' csm)
    svgvb & NH.traverse_ (show >>> ("svg viewbox:" <>) >>> putStrLn)

    True
    co:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    initial canvas:Rect -0.5 0.5 -0.5 0.5
    initial chart:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    csAndHud:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    final canvas:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    single proj:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    multi proj:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    padding:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    initial padded canvas:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    hc0:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    runHudWith:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    ratio new:1.63063063063063
    ratio old:1.63063063063063
    scale ratio:1.0
    data box padded:Rect -0.5 0.5 -0.5 0.5
    svg viewbox:(-3.0e-4,-4.83e-2,0.10859999999999997,6.66e-2)


<a id="org12e043e"></a>

## unscaled + zero frame

    co = jam & set (#markupOptions % #chartAspect) UnscaledAspect & set #hudOptions mempty & set (#hudOptions % #frames) [(100,defaultFrameOptions & set #buffer 0 & set #frame (Just $ blob (grey 0.5 0.1)))]
    display co
    
    asp = co & view (#markupOptions % #chartAspect)
    icanvas = initialCanvas asp Nothing
    cs = view #charts co
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    csAndHudSingle = set styleBox' (Just viewbox) csAndHud
    csm = set (styleBoxN' 10) (Just viewbox) csAndHud
    csp = projectChartWith (view (#markupOptions % #repeatAspect) co) (view (#markupOptions % #chartAspect) co) (view #hudOptions co) cs
    
    -- addHud
    ho = view #hudOptions co
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    csPadded = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
    ivb = initialCanvas asp (Just csPadded)
    db' = fromMaybe db mdb
    csAndHud' = runHudWith ivb db' hs csPadded
    hc0 = cs & set styleBox' (Just ivb)
    
    -- projectWith
    new = ivb
    old = fromMaybe one $ view styleBox' csPadded
    csPaddeds = toListOf charts' csPadded & mconcat
    pwData = csPaddeds & over (each % #chartData) (projectChartDataWith new old)
    pwC = pwData & over (each % #style) (\s -> scaleStyle (scaleRatio (view #scaleP s) new old) s)
    pwRatio = scaleRatio (view #scaleP (head $ view #style <$> pwC)) new old
    pwStyle = view #style (head pwC)
    pwC' = unnamed pwC
    
    
    csp & view styleBox' & NH.traverse_ (show >>> ("co:" <>) >>> putStrLn)
    
    icanvas & (show >>> ("initial canvas:" <>) >>> putStrLn)
    cs & view styleBox' & NH.traverse_ (show >>> ("initial chart:" <>) >>> putStrLn)
    csAndHud & view styleBox' & NH.traverse_ (show >>> ("csAndHud:" <>) >>> putStrLn)
    viewbox & (show >>> ("final canvas:" <>) >>> putStrLn)
    csAndHudSingle & view styleBox' & NH.traverse_ (show >>> ("single proj:" <>) >>> putStrLn)
    csm & view styleBox' & NH.traverse_ (show >>> ("multi proj:" <>) >>> putStrLn)
    csPadded & view styleBox' & NH.traverse_ (show >>> ("padding:" <>) >>> putStrLn)
    ivb & (show >>> ("initial padded canvas:" <>) >>> putStrLn)
    hc0 & view styleBox' & NH.traverse_ (show >>> ("hc0:" <>) >>> putStrLn)
    csAndHud' & view styleBox' & NH.traverse_ (show >>> ("runHudWith:" <>) >>> putStrLn)
    
    -- projectWith
    ratio new & (show >>> ("ratio new:" <>) >>> putStrLn)
    ratio old & (show >>> ("ratio old:" <>) >>> putStrLn)
    pwRatio & (show >>> ("scale ratio:" <>) >>> putStrLn)
    
    
    db' & (show >>> ("data box padded:" <>) >>> putStrLn)
    
    svgViewbox (Rect x z y w) = (x, (-w), (z-x), (w-y))
    svgvb = svgViewbox <$> (view styleBox' csm)
    svgvb & NH.traverse_ (show >>> ("svg viewbox:" <>) >>> putStrLn)

    True
    co:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    initial canvas:Rect -0.5 0.5 -0.5 0.5
    initial chart:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    csAndHud:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    final canvas:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    single proj:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    multi proj:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    padding:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    initial padded canvas:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    hc0:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    runHudWith:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    ratio new:1.63063063063063
    ratio old:1.63063063063063
    scale ratio:1.0
    data box padded:Rect -0.5 0.5 -0.5 0.5
    svg viewbox:(-3.0e-4,-4.83e-2,0.10859999999999997,6.66e-2)


<a id="org0019885"></a>

## ChartAspect + no hud

-   [X] border cool as!

    co = jam & set (#markupOptions % #chartAspect) ChartAspect & set #hudOptions mempty
    display co
    
    asp = co & view (#markupOptions % #chartAspect)
    icanvas = initialCanvas asp Nothing
    cs = view #charts co
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    csAndHudSingle = set styleBox' (Just viewbox) csAndHud
    csm = set (styleBoxN' 10) (Just viewbox) csAndHud
    csp = projectChartWith (view (#markupOptions % #repeatAspect) co) (view (#markupOptions % #chartAspect) co) (view #hudOptions co) cs
    
    -- addHud
    ho = view #hudOptions co
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    csPadded = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
    ivb = initialCanvas asp (Just csPadded)
    db' = fromMaybe db mdb
    csAndHud' = runHudWith ivb db' hs csPadded
    hc0 = cs & set styleBox' (Just ivb)
    
    -- projectWith
    new = ivb
    old = fromMaybe one $ view styleBox' csPadded
    csPaddeds = toListOf charts' csPadded & mconcat
    pwData = csPaddeds & over (each % #chartData) (projectChartDataWith new old)
    pwC = pwData & over (each % #style) (\s -> scaleStyle (scaleRatio (view #scaleP s) new old) s)
    pwRatio = scaleRatio (view #scaleP (head $ view #style <$> pwC)) new old
    pwStyle = view #style (head pwC)
    pwC' = unnamed pwC
    
    
    csp & view styleBox' & NH.traverse_ (show >>> ("co:" <>) >>> putStrLn)
    
    icanvas & (show >>> ("initial canvas:" <>) >>> putStrLn)
    cs & view styleBox' & NH.traverse_ (show >>> ("initial chart:" <>) >>> putStrLn)
    csAndHud & view styleBox' & NH.traverse_ (show >>> ("csAndHud:" <>) >>> putStrLn)
    viewbox & (show >>> ("final canvas:" <>) >>> putStrLn)
    csAndHudSingle & view styleBox' & NH.traverse_ (show >>> ("single proj:" <>) >>> putStrLn)
    csm & view styleBox' & NH.traverse_ (show >>> ("multi proj:" <>) >>> putStrLn)
    csPadded & view styleBox' & NH.traverse_ (show >>> ("padding:" <>) >>> putStrLn)
    ivb & (show >>> ("initial padded canvas:" <>) >>> putStrLn)
    hc0 & view styleBox' & NH.traverse_ (show >>> ("hc0:" <>) >>> putStrLn)
    csAndHud' & view styleBox' & NH.traverse_ (show >>> ("runHudWith:" <>) >>> putStrLn)
    
    -- projectWith
    ratio new & (show >>> ("ratio new:" <>) >>> putStrLn)
    ratio old & (show >>> ("ratio old:" <>) >>> putStrLn)
    pwRatio & (show >>> ("scale ratio:" <>) >>> putStrLn)
    
    
    db' & (show >>> ("data box padded:" <>) >>> putStrLn)
    
    svgViewbox (Rect x z y w) = (x, (-w), (z-x), (w-y))
    svgvb = svgViewbox <$> (view styleBox' csm)
    svgvb & NH.traverse_ (show >>> ("svg viewbox:" <>) >>> putStrLn)

    True
    co:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    initial canvas:Rect -0.5 0.5 -0.5 0.5
    initial chart:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    csAndHud:Rect -0.815315315315315 0.8153153153153154 -0.5 0.5
    final canvas:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    single proj:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    multi proj:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    padding:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    initial padded canvas:Rect -0.815315315315315 0.815315315315315 -0.5 0.5
    hc0:Rect -0.815315315315315 0.8153153153153154 -0.5 0.5
    runHudWith:Rect -0.815315315315315 0.8153153153153154 -0.5 0.5
    ratio new:1.63063063063063
    ratio old:1.63063063063063
    scale ratio:15.015015015015013
    data box padded:Rect -0.5 0.5 -0.5 0.5
    svg viewbox:(-0.8153153153153152,-0.5,1.6306306306306304,1.0)


<a id="org1008171"></a>

## ChartAspect + zero frame

    co = jam & set (#markupOptions % #chartAspect) ChartAspect & set #hudOptions mempty & set (#hudOptions % #frames) [(100,defaultFrameOptions & set #buffer 0 & set #frame (Just $ blob (grey 0.5 0.1)))]
    display co
    
    asp = co & view (#markupOptions % #chartAspect)
    icanvas = initialCanvas asp Nothing
    cs = view #charts co
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    csAndHudSingle = set styleBox' (Just viewbox) csAndHud
    csm = set (styleBoxN' 10) (Just viewbox) csAndHud
    csp = projectChartWith (view (#markupOptions % #repeatAspect) co) (view (#markupOptions % #chartAspect) co) (view #hudOptions co) cs
    
    -- addHud
    ho = view #hudOptions co
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    csPadded = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
    ivb = initialCanvas asp (Just csPadded)
    db' = fromMaybe db mdb
    csAndHud' = runHudWith ivb db' hs csPadded
    hc0 = cs & set styleBox' (Just ivb)
    
    -- projectWith
    new = ivb
    old = fromMaybe one $ view styleBox' csPadded
    csPaddeds = toListOf charts' csPadded & mconcat
    pwData = csPaddeds & over (each % #chartData) (projectChartDataWith new old)
    pwC = pwData & over (each % #style) (\s -> scaleStyle (scaleRatio (view #scaleP s) new old) s)
    pwRatio = scaleRatio (view #scaleP (head $ view #style <$> pwC)) new old
    pwStyle = view #style (head pwC)
    pwC' = unnamed pwC
    
    
    csp & view styleBox' & NH.traverse_ (show >>> ("co:" <>) >>> putStrLn)
    
    icanvas & (show >>> ("initial canvas:" <>) >>> putStrLn)
    cs & view styleBox' & NH.traverse_ (show >>> ("initial chart:" <>) >>> putStrLn)
    csAndHud & view styleBox' & NH.traverse_ (show >>> ("csAndHud:" <>) >>> putStrLn)
    viewbox & (show >>> ("final canvas:" <>) >>> putStrLn)
    csAndHudSingle & view styleBox' & NH.traverse_ (show >>> ("single proj:" <>) >>> putStrLn)
    csm & view styleBox' & NH.traverse_ (show >>> ("multi proj:" <>) >>> putStrLn)
    csPadded & view styleBox' & NH.traverse_ (show >>> ("padding:" <>) >>> putStrLn)
    ivb & (show >>> ("initial padded canvas:" <>) >>> putStrLn)
    hc0 & view styleBox' & NH.traverse_ (show >>> ("hc0:" <>) >>> putStrLn)
    csAndHud' & view styleBox' & NH.traverse_ (show >>> ("runHudWith:" <>) >>> putStrLn)
    
    -- projectWith
    ratio new & (show >>> ("ratio new:" <>) >>> putStrLn)
    ratio old & (show >>> ("ratio old:" <>) >>> putStrLn)
    pwRatio & (show >>> ("scale ratio:" <>) >>> putStrLn)
    
    
    db' & (show >>> ("data box padded:" <>) >>> putStrLn)
    
    svgViewbox (Rect x z y w) = (x, (-w), (z-x), (w-y))
    svgvb = svgViewbox <$> (view styleBox' csm)
    svgvb & NH.traverse_ (show >>> ("svg viewbox:" <>) >>> putStrLn)

    True
    co:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    initial canvas:Rect -0.5 0.5 -0.5 0.5
    initial chart:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    csAndHud:Rect -0.815315315315315 0.8153153153153154 -0.5 0.5
    final canvas:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    single proj:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    multi proj:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    padding:Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    initial padded canvas:Rect -0.815315315315315 0.815315315315315 -0.5 0.5
    hc0:Rect -0.815315315315315 0.8153153153153154 -0.5 0.5
    runHudWith:Rect -0.815315315315315 0.8153153153153154 -0.5 0.5
    ratio new:1.63063063063063
    ratio old:1.63063063063063
    scale ratio:15.015015015015013
    data box padded:Rect -0.5 0.5 -0.5 0.5
    svg viewbox:(-0.8153153153153152,-0.5,1.6306306306306304,1.0)


<a id="org27b5e81"></a>

## FixedAspect + no hud

With no hud, the viewbox is keyed off of the styleBox of the chart, which is different to the FixedAspect 1 requested.

    co = jam & set (#markupOptions % #chartAspect) (FixedAspect 1) & set #hudOptions mempty & set (#charts % charts' % each % #style % #scaleP) ScaleMinDim & set (#charts % charts' % each % #style % #anchor) AnchorMiddle
    display co
    
    asp = co & view (#markupOptions % #chartAspect)
    icanvas = initialCanvas asp Nothing
    cs = view #charts co
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    csAndHudSingle = set styleBox' (Just viewbox) csAndHud
    csm = set (styleBoxN' 10) (Just viewbox) csAndHud
    csp = projectChartWith (view (#markupOptions % #repeatAspect) co) (view (#markupOptions % #chartAspect) co) (view #hudOptions co) cs
    
    -- addHud
    ho = view #hudOptions co
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    csPadded = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
    ivb = initialCanvas asp (Just csPadded)
    db' = fromMaybe db mdb
    csAndHud' = runHudWith ivb db' hs csPadded
    hc0 = cs & set styleBox' (Just ivb)
    
    -- projectWith
    new = ivb
    old = fromMaybe one $ view styleBox' csPadded
    csPaddeds = toListOf charts' csPadded & mconcat
    pwData = csPaddeds & over (each % #chartData) (projectChartDataWith new old)
    pwC = pwData & over (each % #style) (\s -> scaleStyle (scaleRatio (view #scaleP s) new old) s)
    pwRatio = scaleRatio (view #scaleP (head $ view #style <$> pwC)) new old
    pwStyle = view #style (head pwC)
    pwC' = unnamed pwC
    
    
    csp & view styleBox' & NH.traverse_ (show >>> ("co:" <>) >>> putStrLn)
    
    icanvas & (show >>> ("initial canvas:" <>) >>> putStrLn)
    cs & view styleBox' & NH.traverse_ (show >>> ("initial chart:" <>) >>> putStrLn)
    csAndHud & view styleBox' & NH.traverse_ (show >>> ("csAndHud:" <>) >>> putStrLn)
    viewbox & (show >>> ("final canvas:" <>) >>> putStrLn)
    csAndHudSingle & view styleBox' & NH.traverse_ (show >>> ("single proj:" <>) >>> putStrLn)
    csm & view styleBox' & NH.traverse_ (show >>> ("multi proj:" <>) >>> putStrLn)
    csPadded & view styleBox' & NH.traverse_ (show >>> ("padding:" <>) >>> putStrLn)
    ivb & (show >>> ("initial padded canvas:" <>) >>> putStrLn)
    hc0 & view styleBox' & NH.traverse_ (show >>> ("hc0:" <>) >>> putStrLn)
    csAndHud' & view styleBox' & NH.traverse_ (show >>> ("runHudWith:" <>) >>> putStrLn)
    
    -- projectWith
    ratio new & (show >>> ("ratio new:" <>) >>> putStrLn)
    ratio old & (show >>> ("ratio old:" <>) >>> putStrLn)
    pwRatio & (show >>> ("scale ratio:" <>) >>> putStrLn)
    
    db' & (show >>> ("data box padded:" <>) >>> putStrLn)
    
    svgViewbox (Rect x z y w) = (x, (-w), (z-x), (w-y))
    svgvb = svgViewbox <$> (view styleBox' csm)
    svgvb & NH.traverse_ (show >>> ("svg viewbox:" <>) >>> putStrLn)

    True
    co:Rect -0.5000000000000002 0.5000000000000002 -0.3937335125180429 0.2195261559902446
    initial canvas:Rect -0.5 0.5 -0.5 0.5
    initial chart:Rect -5.4299999999999994e-2 5.4299999999999994e-2 -1.8300000000000004e-2 4.83e-2
    csAndHud:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    final canvas:Rect -0.5 0.5 -0.5 0.5
    single proj:Rect -0.5000000000000002 0.5000000000000002 -0.3937335125180429 0.2195261559902446
    multi proj:Rect -0.5000000000000002 0.5000000000000002 -0.3937335125180429 0.2195261559902446
    padding:Rect -5.4299999999999994e-2 5.4299999999999994e-2 -1.8300000000000004e-2 4.83e-2
    initial padded canvas:Rect -0.5 0.5 -0.5 0.5
    hc0:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    runHudWith:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    ratio new:1.0
    ratio old:1.6306306306306302
    scale ratio:9.208103130755065
    data box padded:Rect -0.5 0.5 -0.5 0.5
    svg viewbox:(-0.5000000000000002,-0.2195261559902446,1.0000000000000004,0.6132596685082875)

    toListOf (each % #chartData) csPaddeds
    toListOf (each % #chartData) pwData
    exp1 = csPaddeds & over (each % #chartData) id & over (each % #style) (\s -> scaleStyle 6 (set #scaleP ScaleMinDim s))

    [TextData [("jam",Point 0.0 0.0)]]
    [TextData [("jam",Point -5.551115123125783e-17 -0.2252252252252252)]]
    True


<a id="org6e40481"></a>

## FixedAspect + zero frame

    co = jam & set (#markupOptions % #chartAspect) (FixedAspect 1) & set #hudOptions mempty & set (#charts % charts' % each % #style % #scaleP) ScaleMinDim & set (#charts % charts' % each % #style % #anchor) AnchorMiddle & set (#hudOptions % #frames) [(100,defaultFrameOptions & set #buffer 0 & set #frame (Just $ blob (grey 0.5 0.1)))]
    display co
    
    asp = co & view (#markupOptions % #chartAspect)
    icanvas = initialCanvas asp Nothing
    cs = view #charts co
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    csAndHudSingle = set styleBox' (Just viewbox) csAndHud
    csm = set (styleBoxN' 10) (Just viewbox) csAndHud
    csp = projectChartWith (view (#markupOptions % #repeatAspect) co) (view (#markupOptions % #chartAspect) co) (view #hudOptions co) cs
    
    -- addHud
    ho = view #hudOptions co
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    csPadded = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
    ivb = initialCanvas asp (Just csPadded)
    db' = fromMaybe db mdb
    csAndHud' = runHudWith ivb db' hs csPadded
    hc0 = cs & set styleBox' (Just ivb)
    
    -- projectWith
    new = ivb
    old = fromMaybe one $ view styleBox' csPadded
    csPaddeds = toListOf charts' csPadded & mconcat
    pwData = csPaddeds & over (each % #chartData) (projectChartDataWith new old)
    pwC = pwData & over (each % #style) (\s -> scaleStyle (scaleRatio (view #scaleP s) new old) s)
    pwRatio = scaleRatio (view #scaleP (head $ view #style <$> pwC)) new old
    pwStyle = view #style (head pwC)
    pwC' = unnamed pwC
    
    
    csp & view styleBox' & NH.traverse_ (show >>> ("co:" <>) >>> putStrLn)
    
    icanvas & (show >>> ("initial canvas:" <>) >>> putStrLn)
    cs & view styleBox' & NH.traverse_ (show >>> ("initial chart:" <>) >>> putStrLn)
    csAndHud & view styleBox' & NH.traverse_ (show >>> ("csAndHud:" <>) >>> putStrLn)
    viewbox & (show >>> ("final canvas:" <>) >>> putStrLn)
    csAndHudSingle & view styleBox' & NH.traverse_ (show >>> ("single proj:" <>) >>> putStrLn)
    csm & view styleBox' & NH.traverse_ (show >>> ("multi proj:" <>) >>> putStrLn)
    csPadded & view styleBox' & NH.traverse_ (show >>> ("padding:" <>) >>> putStrLn)
    ivb & (show >>> ("initial padded canvas:" <>) >>> putStrLn)
    hc0 & view styleBox' & NH.traverse_ (show >>> ("hc0:" <>) >>> putStrLn)
    csAndHud' & view styleBox' & NH.traverse_ (show >>> ("runHudWith:" <>) >>> putStrLn)
    
    -- projectWith
    ratio new & (show >>> ("ratio new:" <>) >>> putStrLn)
    ratio old & (show >>> ("ratio old:" <>) >>> putStrLn)
    pwRatio & (show >>> ("scale ratio:" <>) >>> putStrLn)
    
    db' & (show >>> ("data box padded:" <>) >>> putStrLn)
    
    svgViewbox (Rect x z y w) = (x, (-w), (z-x), (w-y))
    svgvb = svgViewbox <$> (view styleBox' csm)
    svgvb & NH.traverse_ (show >>> ("svg viewbox:" <>) >>> putStrLn)

    True
    co:Rect -0.5000000000000002 0.5000000000000002 -0.5 0.5
    initial canvas:Rect -0.5 0.5 -0.5 0.5
    initial chart:Rect -5.4299999999999994e-2 5.4299999999999994e-2 -1.8300000000000004e-2 4.83e-2
    csAndHud:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    final canvas:Rect -0.5 0.5 -0.5 0.5
    single proj:Rect -0.5000000000000002 0.5000000000000002 -0.5 0.5
    multi proj:Rect -0.5000000000000002 0.5000000000000002 -0.5 0.5
    padding:Rect -5.4299999999999994e-2 5.4299999999999994e-2 -1.8300000000000004e-2 4.83e-2
    initial padded canvas:Rect -0.5 0.5 -0.5 0.5
    hc0:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    runHudWith:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    ratio new:1.0
    ratio old:1.6306306306306302
    scale ratio:9.208103130755065
    data box padded:Rect -0.5 0.5 -0.5 0.5
    svg viewbox:(-0.5000000000000002,-0.5,1.0000000000000004,1.0)


<a id="orgc7db683"></a>

## CanvasAspect + zero frame

    co = jam & set (#markupOptions % #chartAspect) (CanvasAspect 1) & set #hudOptions mempty & set (#charts % charts' % each % #style % #scaleP) ScaleMinDim & set (#charts % charts' % each % #style % #anchor) AnchorMiddle & set (#hudOptions % #frames) [(100,defaultFrameOptions & set #buffer 0 & set #frame (Just $ blob (grey 0.5 0.1)))]
    display co
    
    asp = co & view (#markupOptions % #chartAspect)
    icanvas = initialCanvas asp Nothing
    cs = view #charts co
    csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #charts co)
    viewbox = finalCanvas asp (Just csAndHud)
    csAndHudSingle = set styleBox' (Just viewbox) csAndHud
    csm = set (styleBoxN' 10) (Just viewbox) csAndHud
    csp = projectChartWith (view (#markupOptions % #repeatAspect) co) (view (#markupOptions % #chartAspect) co) (view #hudOptions co) cs
    
    -- addHud
    ho = view #hudOptions co
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    csPadded = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
    ivb = initialCanvas asp (Just csPadded)
    db' = fromMaybe db mdb
    csAndHud' = runHudWith ivb db' hs csPadded
    hc0 = cs & set styleBox' (Just ivb)
    
    -- projectWith
    new = ivb
    old = fromMaybe one $ view styleBox' csPadded
    csPaddeds = toListOf charts' csPadded & mconcat
    pwData = csPaddeds & over (each % #chartData) (projectChartDataWith new old)
    pwC = pwData & over (each % #style) (\s -> scaleStyle (scaleRatio (view #scaleP s) new old) s)
    pwRatio = scaleRatio (view #scaleP (head $ view #style <$> pwC)) new old
    pwStyle = view #style (head pwC)
    pwC' = unnamed pwC
    
    
    csp & view styleBox' & NH.traverse_ (show >>> ("co:" <>) >>> putStrLn)
    
    icanvas & (show >>> ("initial canvas:" <>) >>> putStrLn)
    cs & view styleBox' & NH.traverse_ (show >>> ("initial chart:" <>) >>> putStrLn)
    csAndHud & view styleBox' & NH.traverse_ (show >>> ("csAndHud:" <>) >>> putStrLn)
    viewbox & (show >>> ("final canvas:" <>) >>> putStrLn)
    csAndHudSingle & view styleBox' & NH.traverse_ (show >>> ("single proj:" <>) >>> putStrLn)
    csm & view styleBox' & NH.traverse_ (show >>> ("multi proj:" <>) >>> putStrLn)
    csPadded & view styleBox' & NH.traverse_ (show >>> ("padding:" <>) >>> putStrLn)
    ivb & (show >>> ("initial padded canvas:" <>) >>> putStrLn)
    hc0 & view styleBox' & NH.traverse_ (show >>> ("hc0:" <>) >>> putStrLn)
    csAndHud' & view styleBox' & NH.traverse_ (show >>> ("runHudWith:" <>) >>> putStrLn)
    
    -- projectWith
    ratio new & (show >>> ("ratio new:" <>) >>> putStrLn)
    ratio old & (show >>> ("ratio old:" <>) >>> putStrLn)
    pwRatio & (show >>> ("scale ratio:" <>) >>> putStrLn)
    
    db' & (show >>> ("data box padded:" <>) >>> putStrLn)
    
    svgViewbox (Rect x z y w) = (x, (-w), (z-x), (w-y))
    svgvb = svgViewbox <$> (view styleBox' csm)
    svgvb & NH.traverse_ (show >>> ("svg viewbox:" <>) >>> putStrLn)

    True
    co:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    initial canvas:Rect -0.5 0.5 -0.5 0.5
    initial chart:Rect -5.4299999999999994e-2 5.4299999999999994e-2 -1.8300000000000004e-2 4.83e-2
    csAndHud:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    final canvas:Rect -0.8153153153153151 0.8153153153153151 -0.5 0.5
    single proj:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    multi proj:Rect -0.8153153153153152 0.8153153153153152 -0.5 0.5
    padding:Rect -5.4299999999999994e-2 5.4299999999999994e-2 -1.8300000000000004e-2 4.83e-2
    initial padded canvas:Rect -0.5 0.5 -0.5 0.5
    hc0:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    runHudWith:Rect -0.5 0.4999999999999999 -0.39373351251804295 0.2195261559902445
    ratio new:1.0
    ratio old:1.6306306306306302
    scale ratio:9.208103130755065
    data box padded:Rect -0.5 0.5 -0.5 0.5
    svg viewbox:(-0.8153153153153152,-0.5,1.6306306306306304,1.0)


<a id="org97c72b0"></a>

## styleBoxText

-   Anchors ok
-   scaleps ok

    s0 = defaultTextStyle & set #frame (Just $ defaultRectStyle) & set #anchor AnchorStart & set #scaleP NoScaleP
    r0 = styleBoxText s0 "jam" (Point 0 0)
    r1 = styleBoxText (scaleStyle 2 s0) "jam" (Point 0 0)
    r0
    r1
    (\x y -> x - 2 * y) <$> r1 <*> r0

    Rect -3.0e-4 0.10829999999999998 -1.8300000000000004e-2 4.83e-2
    Rect -6.0e-4 0.21659999999999996 -3.660000000000001e-2 9.66e-2
    Rect 0.0 0.0 0.0 0.0

    import Data.Text qualified as Text
    t = "jam"
    o = s0
    p = Point 0 0
    s = o ^. #size
    h = o ^. #hsize
    v = o ^. #vsize
    n1 = o ^. #vshift
    x' = s * h * fromIntegral (Text.length t)
    y' = s * v
    n1' = (-s) * n1
    a' = case o ^. #anchor of; AnchorStart -> 0.5; AnchorEnd -> -0.5; AnchorMiddle -> 0.0
    mpad = maybe id (\f -> padRect (0.5 * view #borderSize f * view #size o)) (view #frame o)
    flat = Rect ((-x' / 2.0) + x' * a') (x' / 2 + x' * a') (-y' / 2 + n1') (y' / 2 + n1')
    mpad $ move p $ maybe flat (`rotationBound` flat) (o ^. #rotation)
    flat

    Rect -5.4299999999999994e-2 5.4299999999999994e-2 -1.8300000000000004e-2 4.83e-2
    Rect -5.399999999999999e-2 5.399999999999999e-2 -1.8000000000000002e-2 4.8e-2

    import Data.Text qualified as Text
    t = "jam"
    o = scaleStyle 2 s0
    p = Point 0 0
    s = o ^. #size
    h = o ^. #hsize
    v = o ^. #vsize
    n1 = o ^. #vshift
    x' = s * h * fromIntegral (Text.length t)
    y' = s * v
    n1' = (-s) * n1
    a' = case o ^. #anchor of; AnchorStart -> 0.5; AnchorEnd -> -0.5; AnchorMiddle -> 0.0
    mpad = maybe id (\f -> padRect (0.5 * view #borderSize f * view #size o)) (view #frame o)
    flat = Rect ((-x' / 2.0) + x' * a') (x' / 2 + x' * a') (-y' / 2 + n1') (y' / 2 + n1')
    mpad $ move p $ maybe flat (`rotationBound` flat) (o ^. #rotation)
    flat

    Rect -0.10859999999999999 0.10859999999999999 -3.660000000000001e-2 9.66e-2
    Rect -0.10799999999999998 0.10799999999999998 -3.6000000000000004e-2 9.6e-2


<a id="org3374fe4"></a>

## markup manual checks

-   [X] initial padded canvas

    view styleBox' csPadded & fmap (ratio * 0.5 >>> (== (let (Rect _ z _ _) = ivb in z)))

    Just False

-   [X] viewbox svg (x,y,width,height): -0.0003 -0.0483 0.1086 0.0666
-   [X] height: 300
-   [X] width: 489 = 0.1086 / 0.0666 \* 300
-   [X] font-size: 0.06 \* 15.015015015015013 = .9009
-   [X] border-size: 0.0090 (border-size 0.01 \* font-size)
-   [X] text rect:  viewbox - 1/2 \* border size

    pPrint $ markupChartOptions co

    Markup
        { elements =
            [ Node
                { rootLabel = OpenTag StartTag "svg"
                    [ Attr
                        { attrName = "xmlns"
                        , attrValue = "http://www.w3.org/2000/svg"
                        }
                    , Attr
                        { attrName = "xmlns:xlink"
                        , attrValue = "http://www.w3.org/1999/xlink"
                        }
                    , Attr
                        { attrName = "width"
                        , attrValue = "489"
                        }
                    , Attr
                        { attrName = "height"
                        , attrValue = "300"
                        }
                    , Attr
                        { attrName = "viewBox"
                        , attrValue = "-0.5000 -0.2195 1.0000 0.6133"
                        }
                    ]
                , subForest =
                    [ Node
                        { rootLabel = OpenTag StartTag "style" []
                        , subForest =
                            [ Node
                                { rootLabel = Content "svg {
                                    color-scheme: light dark;
                                  }
                                  {
                                    .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
                                      fill: rgb(5%, 5%, 5%);
                                    }
                                    .ticklines g, .tickglyph g, .legendBorder g {
                                      stroke: rgb(5%, 5%, 5%);
                                    }
                                    .legendBorder g {
                                      fill: rgb(94%, 94%, 94%);
                                    }
                                  }
                                  @media (prefers-color-scheme:dark) {
                                    .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
                                      fill: rgb(94%, 94%, 94%);
                                    }
                                    .ticklines g, .tickglyph g, .legendBorder g {
                                      stroke: rgb(94%, 94%, 94%);
                                    }
                                    .legendBorder g {
                                      fill: rgb(5%, 5%, 5%);
                                    }
                                  }"
                                , subForest = []
                                }
                            ]
                        }
                    , Node
                        { rootLabel = OpenTag StartTag "g"
                            [ Attr
                                { attrName = "class"
                                , attrValue = "chart"
                                }
                            ]
                        , subForest =
                            [ Node
                                { rootLabel = OpenTag StartTag "g"
                                    [ Attr
                                        { attrName = "stroke-width"
                                        , attrValue = "0.0"
                                        }
                                    , Attr
                                        { attrName = "stroke"
                                        , attrValue = "none"
                                        }
                                    , Attr
                                        { attrName = "fill"
                                        , attrValue = "rgb(5%, 5%, 5%)"
                                        }
                                    , Attr
                                        { attrName = "fill-opacity"
                                        , attrValue = "1.0"
                                        }
                                    , Attr
                                        { attrName = "font-size"
                                        , attrValue = "0.5525"
                                        }
                                    , Attr
                                        { attrName = "text-anchor"
                                        , attrValue = "middle"
                                        }
                                    ]
                                , subForest =
                                    [ Node
                                        { rootLabel = OpenTag StartTag "g"
                                            [ Attr
                                                { attrName = "stroke-width"
                                                , attrValue = "0.0055"
                                                }
                                            , Attr
                                                { attrName = "stroke"
                                                , attrValue = "rgb(2%, 29%, 48%)"
                                                }
                                            , Attr
                                                { attrName = "stroke-opacity"
                                                , attrValue = "1.0"
                                                }
                                            , Attr
                                                { attrName = "fill"
                                                , attrValue = "rgb(2%, 73%, 80%)"
                                                }
                                            , Attr
                                                { attrName = "fill-opacity"
                                                , attrValue = "0.1"
                                                }
                                            ]
                                        , subForest =
                                            [ Node
                                                { rootLabel = OpenTag EmptyElemTag "rect"
                                                    [ Attr
                                                        { attrName = "width"
                                                        , attrValue = "0.9945"
                                                        }
                                                    , Attr
                                                        { attrName = "height"
                                                        , attrValue = "0.6077"
                                                        }
                                                    , Attr
                                                        { attrName = "x"
                                                        , attrValue = "-0.4972"
                                                        }
                                                    , Attr
                                                        { attrName = "y"
                                                        , attrValue = "-0.2168"
                                                        }
                                                    ]
                                                , subForest = []
                                                }
                                            ]
                                        }
                                    , Node
                                        { rootLabel = OpenTag StartTag "text"
                                            [ Attr
                                                { attrName = "x"
                                                , attrValue = "0"
                                                }
                                            , Attr
                                                { attrName = "y"
                                                , attrValue = "0.2252"
                                                }
                                            ]
                                        , subForest =
                                            [ Node
                                                { rootLabel = Content "jam"
                                                , subForest = []
                                                }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                    , Node
                        { rootLabel = OpenTag StartTag "g"
                            [ Attr
                                { attrName = "class"
                                , attrValue = "hud"
                                }
                            ]
                        , subForest = []
                        }
                    ]
                }
            ]
        }


<a id="org65de2e6"></a>

# text and points

    exampleText <- fmap T.pack <$> replicateM 2 (unwords <$> replicateM 3 word)
    exampleText

    ["quia qui aut"]

    exampleText <- fmap T.pack <$> replicateM 2 (unwords <$> replicateM 3 word)
    tsNoScale = defaultTextStyle & set #frame (Just $ defaultRectStyle) & set #anchor AnchorStart & set #scaleP NoScaleP
    tsScale = defaultTextStyle & set #frame (Just $ defaultRectStyle) & set #anchor AnchorStart & set #scaleP ScalePArea
    
    textNoScale = zipWith (\t x -> TextChart tsNoScale [(t, Point 0 x)]) exampleText [0..]
    textScale = zipWith (\t x -> TextChart tsScale [(t, Point 0 (x+0.2))]) exampleText [0..]
    points = Chart defaultGlyphStyle $ GlyphData ((CircleGlyph,) <$> (Point 0 0 :corners4 (Rect 0 1 (-0.5) 0.5)))
    cs = textScale <> [points]
    csNo = textNoScale <> [points]
    ct = unnamed cs
    tandp = mempty & #charts .~ ct & #markupOptions % #chartAspect .~ FixedAspect 1 :: ChartOptions
    tandpNo = mempty & #charts .~ unnamed csNo & #markupOptions % #chartAspect .~ FixedAspect 1 :: ChartOptions
    display tandpNo

    True


<a id="org2ec1880"></a>

# fonts

system-ui,-apple-system,&ldquo;Segoe UI&rdquo;,Roboto,&ldquo;Helvetica Neue&rdquo;,Arial,&ldquo;Noto Sans&rdquo;,&ldquo;Liberation Sans&rdquo;,sans-serif,&ldquo;Apple Color Emoji&rdquo;,&ldquo;Segoe UI Emoji&rdquo;,&ldquo;Segoe UI Symbol&rdquo;,&ldquo;Noto Color Emoji&rdquo;;

SFMono-Regular,Menlo,Monaco,Consolas,&ldquo;Liberation Mono&rdquo;,&ldquo;Courier New&rdquo;,monospace;


<a id="orgdcfefe7"></a>

# Non-singular Text

    co = (mempty :: ChartOptions) & set #charts (unnamed [TextChart defaultTextStyle [("jim", Point 0 0), ("jam",Point 1 1)]]) & set (#charts % charts' % each % #style % #frame) (Just defaultRectStyle) & set #hudOptions defaultHudOptions & set (#charts % charts' % each % #style % #size) 0.2 & set (#charts % charts' % each % #style % #vshift) 0
    display co

    True


<a id="org874fb54"></a>

# HudChart lens audit

canvasBox&rsquo;
  makeAxisBar
  tickGlyph
  tickText
  tickLine

hudStyleBox&rsquo;
  frameHud
  makeAxisBar
  title
  tickText
  legendHud

hudBox&rsquo;
  tickGlyph
  makeTick


<a id="org1a9820c"></a>

# ScaleBorder removal

    display $ glyphsExample & set (#markupOptions % #chartAspect) (FixedAspect 6) & set (#markupOptions % #markupHeight) (Just 200)

    True

    :t glyphsExample & over (#charts % charts' % _last) (fmap id) -- ((\(PathGlyph p _, pt) -> (PathGlyph p NoScaleBorder,pt))))

    <interactive>:1:43: error:
        • Couldn't match type ‘f0 a0’ with ‘Chart’
            arising from a functional dependency between:
              constraint ‘Snoc [Chart] [Chart] (f0 a0) (f0 a0)’
                arising from a use of ‘_last’
              instance ‘Snoc [a] [b] a b’ at <no location info>
        • In the second argument of ‘(%)’, namely ‘_last’
          In the first argument of ‘over’, namely
            ‘(#charts % charts' % _last)’
          In the second argument of ‘(&)’, namely
            ‘over (#charts % charts' % _last) (fmap id)’


<a id="org09cd826"></a>

# HudChartSection

-   [X] axis bar
    -   [X] lens with AnchoredTo option
-   [X] distortion in tickGlyphs due to NoScaleP
-   [X] glyph ticks are a consistent hair away from the axis bar
-   [X] textExample and others are a long way away
-   [X] decide on axis bar and glyph tick buffers and section anchors
    0.01 0.015

    display $ unitExample & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #buffer) 0.01 & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #anchorTo) CanvasSection

    True


<a id="org51afffe"></a>

## decomp

UnscaledAspect is perfect, and then ChartAspect stuffs everything up:

    sec = CanvasStyleSection
    asp = UnscaledAspect
    co = unitExample & set (#hudOptions % #axes % each % #item % #bar %? #buffer) 0.0 & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #buffer) 0.0 & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #anchorTo) sec & set (#markupOptions % #chartAspect) asp  & set (#hudOptions % #axes % each % #item % #ticks % #style % tickExtend' % _Just) TickExtend & set (#hudOptions % #axes % each % #item % #bar %? #anchorTo) sec & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #item % #size) 0.10 & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #item % #shape) SquareGlyph & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #item % #scaleP) NoScaleP & set (#hudOptions % #axes % each % #item % #ticks % #lineTick) Nothing & set (#hudOptions % #axes % each % #item % #ticks % #textTick) Nothing & set (#hudOptions % #frames) []
    display co

    True

    co & view (#hudOptions % #frames)

    :set -Wno-incomplete-uni-patterns
    -- pPrint $ (forgetHud co) & view (#chartTree % #tree) & fmap (second (toListOf (each % #chartData)))
    co' = forgetHud co
    display co'
    xs = filter (/=[]) $ (forgetHud co) & toListOf (#chartTree % charts') & fmap (fmap (view #chartData))
    (Just b') = view styleBox' (view #chartTree co')
    (Just bca') = view styleBox' (view #chartTree (forgetHud $ co & set (#markupOptions % #chartAspect) ChartAspect))
    b'
    bca'
    ratio b'
    ratio bca'
    csUnscaled = addHud UnscaledAspect (view #hudOptions co) (view #chartTree co)
    csChartScaled = addHud ChartAspect (view #hudOptions co) (view #chartTree co)
    view styleBox' csUnscaled
    view styleBox' csChartScaled
    (initialCanvas UnscaledAspect (Just (view #chartTree co)))
    (initialCanvas ChartAspect (Just (view #chartTree co)))

unscaled runHudWith breakup

    cs = view #chartTree co
    ho = view #hudOptions co
    asp = UnscaledAspect
        db = maybe one padSingletons (view box' cs)
        (mdb, hs) = toHuds ho db
        cs' = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
        asp0 = initialCanvas asp (Just cs')
        csAndHud = runHudWith asp0 hs cs'
    view styleBox' cs'
    asp0
    view styleBox' csAndHud
    unscaledCSsAndHud = csAndHud

    Just Rect (-0.6000000000000001) 0.6000000000000001 (-0.6000000000000001) 0.6000000000000001
    Rect (-0.6000000000000001) 0.6000000000000001 (-0.6000000000000001) 0.6000000000000001
    Just Rect (-0.7040000000000002) 0.6520000000000001 (-0.7040000000000002) 0.6520000000000001

ChartAspect runHudWith breakup

    cs = view #chartTree co
    ho = view #hudOptions co
    asp = ChartAspect
        db = maybe one padSingletons (view box' cs)
        (mdb, hs) = toHuds ho db
        cs' = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb
        asp0 = initialCanvas asp (Just cs')
        csAndHud = runHudWith asp0 hs cs'
    view styleBox' cs'
    asp0
    view styleBox' csAndHud
    vb = finalCanvas asp (Just csAndHud)
    vb
    view styleBox' (projectChartWith asp ho cs)
    display $ (mempty :: ChartOptions) & set #chartTree csAndHud & set (#markupOptions % #chartAspect) UnscaledAspect

    Just Rect (-0.6000000000000001) 0.6000000000000001 (-0.6000000000000001) 0.6000000000000001
    Rect (-0.5) 0.5 (-0.5) 0.5
    Just Rect (-0.6040000000000001) 0.552 (-0.6040000000000001) 0.552
    Rect (-0.5) 0.5 (-0.5) 0.5
    Just Rect (-0.49999999999999994) 0.49999999999999994 (-0.49999999999999994) 0.49999999999999994
    True

The problem is in the final set styleBox&rsquo;, which is operating on NoScaleP styled charts:

    ctFinal = set styleBox' (Just vb) $ csAndHud & set (charts' % each % #style % #scaleP) ScalePY
    display $ (mempty :: ChartOptions) & set #chartTree ctFinal & set (#markupOptions % #chartAspect) (FixedAspect 1.5)

    True

    csAndHud & toListOf (charts' % each % #style % #scaleP)

    [NoScaleP,NoScaleP,NoScaleP,NoScaleP,NoScaleP,NoScaleP]


<a id="org75d98a8"></a>

## solution

Scale the xaxis by ScalePX etc:

    sec = CanvasSection
    asp = FixedAspect 1.5
    co = unitExample & set (#hudOptions % #axes % each % #item % #bar %? #buffer) 0.0 & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #buffer) 0.0 & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #anchorTo) sec & set (#markupOptions % #chartAspect) asp  & set (#hudOptions % #axes % each % #item % #ticks % #style % tickExtend' % _Just) TickExtend & set (#hudOptions % #axes % each % #item % #bar %? #anchorTo) sec & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #item % #size) 0.10 & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #item % #shape) SquareGlyph & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #style % #item % #scaleP) NoScaleP & set (#hudOptions % #axes % each % #item % #ticks % #lineTick) Nothing & set (#hudOptions % #axes % each % #item % #ticks % #textTick) Nothing & set (#hudOptions % #frames) [] & set (#hudOptions % #axes % ix 0 % #item % #ticks % #glyphTick %? #style % #item % #scaleP) ScalePX & set (#hudOptions % #axes % ix 1 % #item % #ticks % #glyphTick %? #style % #item % #scaleP) ScalePY
    display co

    True

    display $ unitExample & set (#markupOptions % #chartAspect) UnscaledAspect

    True


<a id="orgecd4b7e"></a>

# mempty

    display $ mempty & set #chartTree (view #chartTree unitExample)

    True

    pPrint $ mempty & set #chartTree (view #chartTree unitExample) & markupChartOptions

    Markup
        { elements =
            [ Node
                { rootLabel = OpenTag StartTag "svg"
                    [ Attr
                        { attrName = "xmlns"
                        , attrValue = "http://www.w3.org/2000/svg"
                        }
                    , Attr
                        { attrName = "xmlns:xlink"
                        , attrValue = "http://www.w3.org/1999/xlink"
                        }
                    , Attr
                        { attrName = "width"
                        , attrValue = "449"
                        }
                    , Attr
                        { attrName = "height"
                        , attrValue = "300"
                        }
                    , Attr
                        { attrName = "viewBox"
                        , attrValue = "-0.7500 -0.5000 1.5000 1.0000"
                        }
                    ]
                , subForest =
                    [ Node
                        { rootLabel = OpenTag StartTag "style" []
                        , subForest =
                            [ Node
                                { rootLabel = Content "svg {
                                    color-scheme: light dark;
                                  }
                                  {
                                    .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
                                      fill: rgb(5%, 5%, 5%);
                                    }
                                    .ticklines g, .tickglyph g, .legendBorder g {
                                      stroke: rgb(5%, 5%, 5%);
                                    }
                                    .legendBorder g {
                                      fill: rgb(94%, 94%, 94%);
                                    }
                                  }
                                  @media (prefers-color-scheme:dark) {
                                    .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
                                      fill: rgb(94%, 94%, 94%);
                                    }
                                    .ticklines g, .tickglyph g, .legendBorder g {
                                      stroke: rgb(94%, 94%, 94%);
                                    }
                                    .legendBorder g {
                                      fill: rgb(5%, 5%, 5%);
                                    }
                                  }
                                  svg { font-family: system-ui,-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,"Noto Sans","Liberation Sans",sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol","Noto Color Emoji";
                                  }
    
                                  ticktext { font-family: SFMono-Regular,Menlo,Monaco,Consolas,"Liberation Mono","Courier New",monospace;
                                  }
    
                                  "
                                , subForest = []
                                }
                            ]
                        }
                    , Node
                        { rootLabel = OpenTag StartTag "g"
                            [ Attr
                                { attrName = "class"
                                , attrValue = "chart"
                                }
                            ]
                        , subForest =
                            [ Node
                                { rootLabel = OpenTag StartTag "g"
                                    [ Attr
                                        { attrName = "class"
                                        , attrValue = "unit"
                                        }
                                    ]
                                , subForest =
                                    [ Node
                                        { rootLabel = OpenTag StartTag "g"
                                            [ Attr
                                                { attrName = "stroke-width"
                                                , attrValue = "0.0100"
                                                }
                                            , Attr
                                                { attrName = "stroke"
                                                , attrValue = "rgb(2%, 29%, 48%)"
                                                }
                                            , Attr
                                                { attrName = "stroke-opacity"
                                                , attrValue = "1.0"
                                                }
                                            , Attr
                                                { attrName = "fill"
                                                , attrValue = "rgb(2%, 73%, 80%)"
                                                }
                                            , Attr
                                                { attrName = "fill-opacity"
                                                , attrValue = "0.1"
                                                }
                                            ]
                                        , subForest =
                                            [ Node
                                                { rootLabel = OpenTag EmptyElemTag "rect"
                                                    [ Attr
                                                        { attrName = "width"
                                                        , attrValue = "1.4900"
                                                        }
                                                    , Attr
                                                        { attrName = "height"
                                                        , attrValue = "0.9900"
                                                        }
                                                    , Attr
                                                        { attrName = "x"
                                                        , attrValue = "-0.7450"
                                                        }
                                                    , Attr
                                                        { attrName = "y"
                                                        , attrValue = "-0.4950"
                                                        }
                                                    ]
                                                , subForest = []
                                                }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                    , Node
                        { rootLabel = OpenTag StartTag "g"
                            [ Attr
                                { attrName = "class"
                                , attrValue = "hud"
                                }
                            ]
                        , subForest = []
                        }
                    ]
                }
            ]
        }


<a id="org28732c7"></a>

# chart-svg mega cleanup checklist

-   [X] rerun whole org file and observe results
-   [X] styleBox&rsquo; versus projectChartTree
-   [X] text box bug
-   [X] scaling text example
-   [X] examples audit
-   [X] NoScaleP needs to be the default for HudOption elements
-   [X] audit ScaleP usage
-   [X] toggle to switch to ScalePArea when the hud is forgotten
-   [X] surface legend
-   [X] check & fix examples
-   [X] fix compound code and compoundExample
-   [X] v06candidate1
-   [X] Revisit anal [all in one](file:///Users/tonyday567/haskell/anal/readme.md)
-   [X] diff test back
-   [X] code ToDos
-   [X] remove canvasStyleBox&rsquo;
-   [X] remove multiple reboxing
-   [X] palette1 ==> palette
-   [X] \#charts -> #chartTree
-   [X] move GlyphShape back to Style
-   [X] fix Rect show instance
-   [X] priority refactor
-   [X] buffered
-   [X] better names for tick elements
-   [X] remove placeText
-   [X] tick lenses
-   [X] remove Buffered, TextTickStyle, LineTickStyle
-   [X] FrameOption anchorTo
-   [X] review ScaleP
-   [X] look at mempty examples for redundant markup snippets
    -   [X] mempty & set #hudOptions defaultHudOptions & set #chartTree mempty produces a hud still?
-   [X] replace .~ ~% ^.


<a id="orgc81a36a"></a>

# AST


<a id="org8fd8e8e"></a>

## ChartOptions

    data ChartOptions = ChartOptions
      { markupOptions :: MarkupOptions,
        hudOptions :: HudOptions,
        chartTree :: ChartTree
      }


<a id="orge4d70a8"></a>

## MarkupOptions

    data MarkupOptions = MarkupOptions
      { markupHeight :: Maybe Double,
        chartAspect :: ChartAspect,
        cssOptions :: CssOptions,
        renderStyle :: RenderStyle
      }

    data ChartAspect
      = FixedAspect Double
      | CanvasAspect Double
      | ChartAspect
      | UnscaledAspect

    data CssOptions = CssOptions
      { shapeRendering :: CssShapeRendering,
        preferColorScheme :: CssPreferColorScheme,
        fontFamilies :: ByteString,
        cssExtra :: ByteString}

    data RenderStyle = Compact | Indented Int

    data CssShapeRendering = UseGeometricPrecision | UseCssCrisp | NoShapeRendering

    data CssPreferColorScheme
      = -- | includes css that switches approriate hud elements between light and dark.
        PreferHud
      | PreferDark
      | PreferLight
      | PreferNormal


<a id="org1139186"></a>

## HudOptions

    data HudOptions = HudOptions
      { axes :: [Priority AxisOptions],
        frames :: [Priority FrameOptions],
        legends :: [Priority LegendOptions],
        titles :: [Priority Title]
      }

    data Priority a = Priority {priority :: Double, item :: a}


<a id="orgaef3150"></a>

### AxisOptions

    data AxisOptions = AxisOptions
      { bar :: Maybe AxisBar,
        adjust :: Maybe Adjustments,
        ticks :: Ticks,
        place :: Place
      }

    data AxisBar = AxisBar
      { style :: Style,
        size :: Double,
        buffer :: Double,
        overhang :: Double,
        anchorTo :: HudChartSection
      }

    data HudChartSection = CanvasSection | CanvasStyleSection | HudSection | HudStyleSection deriving (Eq, Show, Generic)

    data Adjustments = Adjustments
      { maxXRatio :: Double,
        maxYRatio :: Double,
        angledRatio :: Double,
        allowDiagonal :: Bool
      }

    data Ticks = Ticks
      { tick :: Tick,
        glyphTick :: Maybe TickStyle,
        textTick :: Maybe TickStyle,
        lineTick :: Maybe TickStyle
      }

    data Tick
      = TickNone
      | TickLabels [Text]
      | TickRound FormatN Int TickExtend
      | TickExact FormatN Int
      | TickPlaced [(Double, Text)]

    data FormatN = FormatN {fstyle :: FStyle, sigFigs :: Maybe Int, maxDistinguishIterations :: Int, addLPad :: Bool, cutRightZeros :: Bool} deriving (Eq, Show, Generic)

    data FStyle
      = FSDecimal
      | FSExponent (Maybe Int)
      | FSComma
      | FSFixed Int
      | FSPercent
      | FSDollar
      | FSPrec
      | FSCommaPrec
      | FSNone

    data TickExtend = TickExtend | NoTickExtend deriving (Eq, Show, Generic)

    data TickStyle = TickStyle
      { style :: Style,
        anchorTo :: HudChartSection,
        buffer :: Double
      }

    data Place
      = PlaceLeft
      | PlaceRight
      | PlaceTop
      | PlaceBottom
      | PlaceAbsolute (Point Double)


<a id="orgd5d69b3"></a>

### FrameOptions

    data FrameOptions = FrameOptions
      { frame :: Maybe Style,
        anchorTo :: HudChartSection,
        buffer :: Double
      }


<a id="orgb3b56ce"></a>

### LegendOptions

    data LegendOptions = LegendOptions
      { size :: Double,
        buffer :: Double,
        vgap :: Double,
        hgap :: Double,
        textStyle :: Style,
        innerPad :: Double,
        outerPad :: Double,
        frame :: Maybe Style,
        place :: Place,
        overallScale :: Double,
        scaleP :: ScaleP,
        legendCharts :: [(Text, [Chart])]
      }


<a id="org9d66a21"></a>

### Title

    data Title = Title
      { text :: Text,
        style :: Style,
        place :: Place,
        anchor :: Anchor,
        buffer :: Double
      }

    data Anchor = AnchorMiddle | AnchorStart | AnchorEnd


<a id="orgdaa9276"></a>

## ChartTree

    newtype ChartTree = ChartTree {tree :: Tree (Maybe Text, [Chart])} deriving (Eq, Show, Generic)

    data Chart = Chart {style :: Style, chartData :: ChartData} deriving (Eq, Show, Generic)

    data ChartData
      = RectData [Rect Double]
      | LineData [[Point Double]]
      | GlyphData [Point Double]
      | TextData [(Text, Point Double)]
      | PathData [PathData Double]
      | BlankData [Rect Double]

    newtype Rect a = Rect' (Compose Point Range a)
    
    -- | pattern of Rect lowerx upperx lowery uppery
    pattern Rect :: a -> a -> a -> a -> Rect a
    pattern Rect a b c d = Rect' (Compose (Point (Range a b) (Range c d)))

    data Point a = Point
      { _x :: a,
        _y :: a
      }

    data Range a = Range a a

    data PathData a
      = StartP (Point a)
      | LineP (Point a)
      | CubicP (Point a) (Point a) (Point a)
      | QuadP (Point a) (Point a)
      | ArcP (ArcInfo a) (Point a)

    data ArcInfo a = ArcInfo
      { radii :: Point a,
        phi :: a,
        large :: Bool,
        clockwise :: Bool
      }


<a id="org8781204"></a>

## Style

    data Style = Style
      { size :: Double,
        borderSize :: Double,
        color :: Colour,
        borderColor :: Colour,
        scaleP :: ScaleP,
        anchor :: Anchor,
        rotation :: Maybe Double,
        translate :: Maybe (Point Double),
        escapeText :: EscapeText,
        frame :: Maybe Style,
        linecap :: Maybe LineCap,
        linejoin :: Maybe LineJoin,
        dasharray :: Maybe [Double],
        dashoffset :: Maybe Double,
        hsize :: Double,
        vsize :: Double,
        vshift :: Double,
        shape :: GlyphShape
      }

    newtype Colour = Colour'
      { color' :: Color (Alpha RGB) Double
      }
    
    pattern Colour :: Double -> Double -> Double -> Double -> Colour
    pattern Colour r g b a = Colour' (ColorRGBA r g b a)

    data ScaleP
      = NoScaleP
      | ScalePX
      | ScalePY
      | ScalePMinDim
      | ScalePArea

    data Anchor = AnchorMiddle | AnchorStart | AnchorEnd

    data EscapeText = EscapeText | NoEscapeText

    data LineCap = LineCapButt | LineCapRound | LineCapSquare deriving (Eq, Show, Generic)

    data LineJoin = LineJoinMiter | LineJoinBevel | LineJoinRound deriving (Eq, Show, Generic)

    data GlyphShape
      = CircleGlyph
      | SquareGlyph
      | EllipseGlyph Double
      | RectSharpGlyph Double
      | RectRoundedGlyph Double Double Double
      | -- | line width is determined by borderSize
        TriangleGlyph (Point Double) (Point Double) (Point Double)
      | VLineGlyph
      | HLineGlyph
      | PathGlyph ByteString

