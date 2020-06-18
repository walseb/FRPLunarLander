module Level where

import Control.Lens
import FRPEngine.Types
import Linear
import Types

-- I'm inverting here because the program I used to measure the points used a top left model
negateYAxis :: (Number a) => V2 a -> V2 a
negateYAxis = _y `over` ((-) 1)

-- This turns absolute collision points into relative ones
absCollPointsToRelative :: (Number a) => a -> V2 a -> V2 a
absCollPointsToRelative maxDim curr = curr' / maxDim'
  where
    maxDim' = V2 maxDim 3000
    curr' = curr

-- Scale the sizes of everything, ugly hack because I think the positions aren't actually relative for terrain
scaleSize x = x * 8

-- Ugly hack to turn convex shapes into concave ones. This is done by
convex :: (Number a) => [[V2 a]] -> [[V2 a]]
convex = filter (not . null) . mconcat . fmap (convex' [[]])
  where
    convex' sum (x : rest@(x' : _)) = convex' ([[x, x', V2 (x ^. _x) (-1)]] <> sum) rest
    convex' sum (x : _) = sum

initialGame :: (Number a) => GameState a
initialGame =
  GameState
    (CameraState 3)
    ( PhysicalState
        (Player True (CollObj [[(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]] (Obj (scaleSize (V2 500 3000)) (V2 5000 (-500)) 0 (V2 500 500) SobjectSprite True)) 0 10)
        -- Here the collision points are relative to the object position. However since the program I used to get them returned absolute positions, I run them through scale coll to make them relative.
        ( Scene
            ( [ ( CollObj
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 989))
                            [ [ (V2 0 1896),
                                (V2 162 1925),
                                (V2 358 1998),
                                (V2 507 2005),
                                (V2 703 2122),
                                (V2 743 2341),
                                (V2 989 2563)
                              ]
                            ]
                        )
                    )
                    (Obj (V2 (scaleSize 0) 0) 0 0 (scaleSize (V2 989 3000)) Sterr1 False)
                ),
                ( CollObj
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 2290))
                            [ [ (V2 0 2562),
                                (V2 182 2549),
                                (V2 433 2428),
                                (V2 538 2441),
                                (V2 710 2589),
                                (V2 806 2799),
                                (V2 968 2880),
                                (V2 1137 2868),
                                (V2 1354 2702),
                                (V2 1430 2524),
                                (V2 1583 2409),
                                (V2 1653 2208),
                                (V2 1842 2086),
                                (V2 1914 1839),
                                (V2 2151 1741),
                                (V2 2290 1588)
                              ]
                            ]
                        )
                    )
                    (Obj (V2 (scaleSize (989 + 324)) 0) 0 0 (scaleSize (V2 2290 3000)) Sterr2 False)
                ),
                ( CollObj
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 3373))
                            [ [ (V2 0 1587),
                                (V2 83 1382),
                                (V2 115 1165),
                                (V2 182 1037),
                                (V2 309 974),
                                (V2 396 1050),
                                (V2 369 1200),
                                (V2 580 1362),
                                (V2 616 1619),
                                (V2 782 1702),
                                (V2 1102 1791),
                                (V2 1206 1974),
                                (V2 1412 2014),
                                (V2 1642 1775),
                                (V2 1970 1639),
                                (V2 2097 1322),
                                (V2 2313 1194),
                                (V2 2392 950),
                                (V2 2555 733),
                                (V2 2595 392),
                                (V2 2799 212),
                                (V2 3074 239),
                                (V2 3324 520),
                                (V2 3373 760)
                              ]
                            ]
                        )
                    )
                    (Obj (V2 (scaleSize (989 + 324 + 2290 + 256)) 0) 0 0 (scaleSize (V2 3373 3000)) Sterr3 False)
                ),
                ( CollObj
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 1023))
                            [ [ (V2 0 767),
                                (V2 226 1005),
                                (V2 328 1274),
                                (V2 569 1464),
                                (V2 1023 1612)
                              ]
                            ]
                        )
                    )
                    (Obj (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373 + 157)) 0) 0 0 (scaleSize (V2 1023 3000)) Sterr4 False)
                ),
                ( CollObj
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 1109))
                            [ [ (V2 0 1614),
                                (V2 168 1770),
                                (V2 346 1816),
                                (V2 421 1998),
                                (V2 646 2089),
                                (V2 774 2347),
                                (V2 994 2270),
                                (V2 1109 2296)
                              ]
                            ]
                        )
                    )
                    (Obj (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373 + 157 + 1023 + 479)) 0) 0 0 (scaleSize (V2 1109 3000)) Sterr5 False)
                ),
                ( CollObj
                    ( convex
                        ( (fmap . fmap)
                            negateYAxis
                            [ [ (V2 0 0),
                                (V2 0 1),
                                (V2 1 1),
                                (V2 1 0)
                              ]
                            ]
                        )
                    )
                    (Obj (scaleSize (V2 (-10000) (-3000))) 0 0 (scaleSize (V2 50000 3000)) SHidden False)
                )
              ]
            )
            -- Landing spots
            ( [ ( LandingSpot
                    2
                    ( CollObj
                        ( convex
                            ( (fmap . fmap)
                                (negateYAxis . (absCollPointsToRelative 324))
                                [ [ (V2 0 2571),
                                    (V2 324 2571)
                                  ]
                                ]
                            )
                        )
                        (Obj (V2 (scaleSize 989) 0) 0 0 (scaleSize (V2 324 3000)) Sland1 False)
                    )
                ),
                ( LandingSpot
                    1
                    ( CollObj
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 256))
                            [ [ (V2 0 1588),
                                (V2 256 1588)
                              ]
                            ]
                        )
                        (Obj (V2 (scaleSize (989 + 324 + 2290)) 0) 0 0 (scaleSize (V2 256 3000)) Sland2 False)
                    )
                ),
                ( LandingSpot
                    3
                    ( CollObj
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 157))
                            [ [ (V2 0 766),
                                (V2 157 766)
                              ]
                            ]
                        )
                        (Obj (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373)) 0) 0 0 (scaleSize (V2 157 3000)) Sland3 False)
                    )
                ),
                ( LandingSpot
                    2
                    ( CollObj
                        ( (fmap . fmap)
                            (negateYAxis . (absCollPointsToRelative 479))
                            [ [ (V2 0 1613),
                                (V2 479 1613)
                              ]
                            ]
                        )
                        (Obj (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373 + 157 + 1023)) 0) 0 0 (scaleSize (V2 479 3000)) Sland4 False)
                    )
                )
              ]
            )
        )
    )
