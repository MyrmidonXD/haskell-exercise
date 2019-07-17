import Data.Maybe

class Heap a where
    insert :: (Ord b) => b -> a -> a
    deleteMin :: (Ord b) => a -> Maybe (b, a)
    peek :: (Ord b) => a -> Maybe b

main = return ()    
