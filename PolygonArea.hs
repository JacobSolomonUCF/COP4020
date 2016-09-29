module PolygonArea where

computeArea :: [(Double,Double)] -> Double
computeArea [] = error "Empty list."
computeArea [(_,_)] = error "Only one pair."
computeArea all@((x1,y1):xs) = recPolyArea (all ++ [(x1,y1)])

recPolyArea :: [(Double, Double)] -> Double
recPolyArea [(x1,y1),(x2,y2)] = (det (x1, y1) (x2, y2)) / 2
recPolyArea all@((x1,y1):(x2,y2):xs) = ((det (x1, y1) (x2, y2)) / 2) + (recPolyArea (tail all))

det :: (Double,Double) -> (Double,Double) -> Double
det (x1, y1) (x2, y2) = x1*y2 - x2*y1

