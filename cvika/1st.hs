nthelement (x:xz) 0 = x
nthelement (x:xz) a = nthelement xz (a-1)

compress [] = []
compress [a] = [a]
compress (x:y:xz) 
    | x == y = compress(y:xz)
    | otherwise = x : compress(y:xz)



compressHelper current count [] = [(current, count)]
compressHelper current count (y:ys)
	| current == y = compressHelper current (count+1) ys
	| otherwise = (current, count) : compressHelper y 1 ys

compress2 [a] = [(a,1)]
compress2 (x:xz) =  compressHelper x 1 xz
