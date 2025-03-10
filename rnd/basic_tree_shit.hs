data Tree a = Lf | Nd a (Tree a) (Tree a)
    deriving (Show)

inorder Lf = []
inorder (Nd x l p) = inorder l ++ (x:inorder p)

neco Lf = []
neco (Nd x l r) = [x] ++ neco l ++ neco r

sampleTree = Nd 10
                (Nd 5
                    (Nd 3 Lf Lf)
                    (Nd 7 Lf Lf))
                (Nd 15
                    (Nd 12 Lf Lf)
                    (Nd 20 Lf Lf)
                    )

s2 = Nd 11
                (Nd 5
                    (Nd 3 Lf Lf)
                    (Nd 7 Lf Lf))
                (Nd 15
                    (Nd 12 Lf Lf)
                    (Nd 20 Lf Lf)
                    )

instance Eq a => Eq (Tree a) where
    Lf == lf = True
    (Nd x l1 r1) == (Nd y l2 r2) = l1 == l2 && r1 == r2 && x == y
    _ == _ = False


instance Functor Tree where
    fmap _ Lf = Lf
    fmap f (Nd x l1 r1) = Nd (f x) (fmap f l1) (fmap f r1)