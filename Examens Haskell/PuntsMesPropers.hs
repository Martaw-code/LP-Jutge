closest :: [(Float,Float)] -> Float
closest [p1,p2] = distancia p1 p2
closest (x:xs)
    | cx > cxs = cxs
    | otherwise = cx
    where
        cxs = closest xs
        cx = minimum [distancia x y | y <- xs]

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
