data Vector a = Vector a a a deriving (Show)

vplus:: (Num t) => Vector t -> Vector t -> Vector t

(Vector a b c) `vplus` (Vector d e f) = Vector (a+d) (b+e) (c+f)
