module Level where

import Linear
import Types
import Render.Render

initialGame =
  GameState
    (CameraState 3)
    ( PhysicalState
        ( MovingState
            (Player (Living True (Object (V2 0 0) (V2 500 500) 0)) 0)
            [(Living True (Object (V2 0 1200) (V2 500 500) 0))]
        )
        -- Here the points are relative to the object position. In game loop those gets turned into world position
        ( Scene
            ( [ ( Terrain
                    ( (fmap . fmap)
                        negateYAxis
                        [ [ (V2 0 0.36666666666666664),
                            (V2 0.3807291666666667 0.38055555555555554),
                            (V2 0.3807291666666667 1),
                            (V2 0 1)
                          ],
                          [ (V2 0.3807291666666667 0.7666666666666667),
                            (V2 0.7197916666666667 0.7666666666666667),
                            (V2 0.7229166666666667 1),
                            (V2 0.3807291666666667 1)
                          ],
                          [ (V2 0.7322916666666667 0.3435185185185185),
                            (V2 1 0.3675925925925926),
                            (V2 1 1),
                            (V2 0.7322916666666667 1)
                          ]
                        ]
                    )
                    (Object (V2 3000 0) (V2 5000 5000) 0)
                )
                -- (Terrain
                --   [[(V2 0 0), (V2 900 0), (V2 900 900), (V2 0 900)]]
                --   (Object (V2 0 0) (V2 1 1) 0))
              ]
            )
            ( [ ( LandingSpot
                    3
                    ( Terrain
                        ( (fmap . fmap)
                            negateYAxis
                            [ [ (V2 0 0.36666666666666664),
                                (V2 0.3807291666666667 0.38055555555555554),
                                (V2 0.3807291666666667 1),
                                (V2 0 1)
                              ],
                              [ (V2 0.3807291666666667 0.7666666666666667),
                                (V2 0.7197916666666667 0.7666666666666667),
                                (V2 0.7229166666666667 1),
                                (V2 0.3807291666666667 1)
                              ],
                              [ (V2 0.7322916666666667 0.3435185185185185),
                                (V2 1 0.3675925925925926),
                                (V2 1 1),
                                (V2 0.7322916666666667 1)
                              ]
                            ]
                        )
                        (Object (V2 (-2000) 0) (V2 5000 5000) 0)
                    )
                )
              ]
            )
        )
    )
