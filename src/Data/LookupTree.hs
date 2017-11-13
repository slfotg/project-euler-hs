module Data.LookupTree(
    LookupTree,
    (!!!),
    naturals ) where

data LookupTree a = Node a (LookupTree a) (LookupTree a)

(!!!) :: LookupTree a -> Int -> a
(!!!) (Node a l r) 0 = a
(!!!) (Node a l r) n =
    if odd n
        then l !!! top
        else r !!! (top - 1)
            where top = n `div` 2

naturals :: LookupTree Int
naturals = naturals' 1 0
    where
        naturals' :: Int -> Int -> LookupTree Int
        naturals' r n =
            Node n
                ((naturals' $! r2) $! (n+r))
                ((naturals' $! r2) $! (n+r2))
                    where r2 = 2 * r

instance Functor LookupTree where
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
