module Queue(
Queue,
emptyQueue,
queueEmpty,
enqueue,
dequeue,
front)
where

newtype Queue a = Q ([a],[a])

instance (Show a) => Show (Queue a) where
	showsPrec p (Q (front, rear)) str
		= showString "Q " (showList (front ++ reverse rear) str)

emptyQueue :: Queue a
emptyQueue = Q ([],[])

queueEmpty :: Queue a -> Bool
queueEmpty (Q ([],[])) = True
queueEmpty (Q _) = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Q ([],[])) = Q ([x],[])
enqueue y (Q (xs,ys)) = Q (xs, y:ys)

dequeue :: Queue a -> Queue a
dequeue (Q ([],[])) = error "dequeue: empty queue"
dequeue (Q ([],ys)) = Q (tail (reverse ys),[])
dequeue (Q (x:xs,ys)) = Q (xs,ys)

front :: Queue a -> a
front (Q ([],[])) = error "front: empty queue"
front (Q ([],ys)) = last ys
front (Q (x:xs,ys)) = x

