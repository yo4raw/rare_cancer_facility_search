module Distance exposing (..)

distance : Float -> Float -> Float -> Float -> Int
distance lat1 lng1 lat2 lng2 =
    6371 * acos (cos (lat1 * pi / 180) * cos (lat2 * pi / 180)  * cos ((lng2 * pi / 180) - (lng1 * pi / 180)) + sin (lat1 * pi / 180) * sin (lat2 * pi / 180))
    |> round

