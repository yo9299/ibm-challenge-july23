import qualified Data.IntMap.Strict as M
import Data.Maybe
import qualified Data.Array as A
import Control.Monad.State 
import Debug.Trace 
import qualified Data.List as L
 

type Board = A.Array (Int, Int) Piece
type Position = (Int, Int)
type Piece = Int
data Move = U | D | L | R
        deriving (Show, Eq) 
data Color = Blue | Red | Yellow | Orange | Green | Purple | Empty
        deriving (Show, Eq)

pieceToColor :: Int -> Color
pieceToColor k 
                | k == 0 = Empty
                | k == 1 = Orange
                | k == 2 = Green 
                | k == 3 = Red 
                | k == 4 = Orange
                | k == 5 = Blue
                | k == 6 = Blue
                | k == 7 = Yellow
                | k == 8 = Purple
                | k == 9 = Yellow   

boardToGoal :: A.Array (Int, Int) Piece -> A.Array (Int, Int) Color 
boardToGoal b = fmap pieceToColor b
-- b A.// [((i,j), fmap pieceToColor (b A.! (i,j))) | i <-[1..5], j<-[1..5]]

goalArray :: A.Array (Int, Int) Color
goalArray = A.array ((1,1),(5,5)) [((1,1),Yellow), ((1,2),Yellow),((1,3),Empty), ((1,4),Green),((1,5), Green),((2,1),Empty),((2,2),Empty), ((2,3),Red),((2,4),Green),((2,5),Orange), ((3,1),Empty), ((3,2),Red),((3,3),Red), ((3,4),Orange),((3,5),Orange),((4,1),Blue),((4,2),Yellow), ((4,3),Yellow),((4,4),Orange),((4,5),Purple), ((5,1),Empty),((5,2),Blue), ((5,3),Purple),((5,4),Purple),((5,5),Purple)]

initialArray :: Board
--initialArray = A.array ((1,1),(5,5)) [((1,1),1), ((1,2),2),((1,3),2), ((1,4),3),((1,5), 4),((2,1),1),((2,2),2), ((2,3),3),((2,4),3),((2,5),4), ((3,1),5), ((3,2),6),((3,3),7), ((3,4),7),((3,5),8),((4,1),9),((4,2),9), ((4,3),8),((4,4),8),((4,5),8), ((5,1),0),((5,2),0), ((5,3),0),((5,4),0),((5,5),0)]
initialArray = A.array ((1,1),(5,5)) [((1,1),0), ((1,2),7),((1,3),7), ((1,4),2),((1,5), 2),((2,1),0),((2,2),0), ((2,3),3),((2,4),2),((2,5),1), ((3,1),0), ((3,2),3),((3,3),3), ((3,4),4),((3,5),1),((4,1),5),((4,2),9), ((4,3),9),((4,4),4),((4,5),8), ((5,1),0),((5,2),6), ((5,3),8),((5,4),8),((5,5),8)]


{-goalArray :: A.Array (Int, Int) Int
goalArray = A.array ((1,1),(5,5)) [((1,1),7), ((1,2),7),((1,3),0), ((1,4),2),((1,5), 2),((2,1),0),((2,2),0), ((2,3),3),((2,4),2),((2,5),1), ((3,1),0), ((3,2),3),((3,3),3), ((3,4),4),((3,5),1),((4,1),5),((4,2),9), ((4,3),9),((4,4),4),((4,5),8), ((5,1),0),((5,2),6), ((5,3),8),((5,4),8),((5,5),8)]


initialArray :: A.Array (Int, Int) Int
--initialArray = A.array ((1,1),(5,5)) [((1,1),1), ((1,2),2),((1,3),2), ((1,4),3),((1,5), 4),((2,1),1),((2,2),2), ((2,3),3),((2,4),3),((2,5),4), ((3,1),5), ((3,2),6),((3,3),7), ((3,4),7),((3,5),8),((4,1),9),((4,2),9), ((4,3),8),((4,4),8),((4,5),8), ((5,1),0),((5,2),0), ((5,3),0),((5,4),0),((5,5),0)]
initialArray = A.array ((1,1),(3,3)) [((1,1),1), ((1,2),0),((1,3),0),((2,1),0),((2,2),0),((2,3),0),((3,1),0),((3,2),0),((3,3),0)]


goalArray :: A.Array (Int, Int) Int
goalArray = A.array ((1,1),(3,3)) [((1,1),0), ((1,2),0),((1,3),0),((2,1),0),((2,2),0),((2,3),0),((3,1),0),((3,2),0),((3,3),1)]
-}

{-getPiece:: Pieces -> Int -> [Position]
getPiece pieces k = pieces M.! k-}

getCase :: Position -> State Board Piece 
getCase (i,j) = do 
                cur <- get 
                let piece = cur A.! (i,j)
                pure piece 

moveCase :: Position -> Move -> Position
moveCase (i,j) U = (i+1,j)
moveCase (i,j) D = (i-1,j)
moveCase (i,j) R = (i,j+1)
moveCase (i,j) L = (i,j-1)

movePiece :: [Position] -> Move -> [Position]
movePiece listp s = zipWith moveCase listp (repeat s)

{-move:: Pieces -> Move -> Int -> Pieces
-- move piece k Up
move piece s k = M.adjust (f) (k) (piece)
    where 
          f = (\listpos -> movePiece listpos s)-}

execLocally :: State Board a -> State Board Board
execLocally function = do 
                    cur <- get 
                    --pure $ runState function
                    result <- function
                    result <- get
                    put cur
                    pure result
                 
                    


getPositionsM :: Piece -> State Board [Position]
getPositionsM p = do 
    cur <- get 
    let alist = A.assocs cur :: [(Position,Piece)]
        flist = filter f alist
        f:: (Position, Piece) -> Bool 
        f (pos, p') = p==p'
        posList = fmap fst flist
                  
    pure posList


movePieceM :: Piece -> Move -> State Board ()
movePieceM p m = do
            cur <- get
            listPos <- getPositionsM p 
            let newPos = movePiece listPos m 
                newboard2 = cur A.// zip listPos (repeat 0)
                newboard = newboard2 A.// zip newPos (repeat p)
            put newboard


inBoardP :: Board -> (Int,Int) -> Bool 
inBoardP b (k1,k2) = and [0<k1,  k1<(n1+1), 0<k2, k2<(n2+1)]
    where (_,(n1, n2)) =A.bounds b 

availablePieceMoveM :: Piece -> Move -> State Board Bool
availablePieceMoveM p m = do 
                cur <- get 
                listPos <- getPositionsM p 
                let candidatePos = movePiece listPos m
                    available = and [ (cur A.! k)== 0| k <- candidatePos ] -- checks case is empty
                    (_,(n1, n2)) = A.bounds cur 
                    available1 = and $ fmap (inBoardP cur) candidatePos
                    --[ and [0<k1,  k1<(n1+1), 0<k2, k2<(n2+1)] | (k1,k2) <- candidatePos ] 

                pure available1




availablePieceM :: Piece -> State Board [(Piece,Move)]
availablePieceM p = do 
            cur <- get
            --available <- [m | m <-[U, D, L, R], availablePieceMoveM p m ]
            available <- filterM (\m -> availablePieceMoveM p m) [U, D, L, R]
            let listav = zip (repeat p ) available
            pure listav



computeAdjacentM :: State Board [Piece]
computeAdjacentM = do
            cur <- get 
            listPos <- getPositionsM 0 --get the list of free cases
            let candidates = concatMap (\pos -> zip (repeat pos) [ U, D, L, R]) listPos
                adjacents0 = fmap (\(pos, mov) -> moveCase pos mov) candidates
                adjacents = filter (inBoardP cur) adjacents0
            adjpieces0 <- filterM (\pos -> liftM (/= 0) (getCase pos)) adjacents
            adjpieces <- mapM (\pos -> getCase pos) adjpieces0 -- retrieve the pieces using `getCase`
            pure $ L.nub adjpieces                                                  

availableM :: State Board [(Piece, Move)]
availableM = do 
        cur <- get 
        candidatePieces <- computeAdjacentM
        candidateMoves <-  mapM  availablePieceM candidatePieces 
        --let listmoves= candidateMoves
        pure $ concat candidateMoves --i want to concatenate this so i can return [(Piece, Move)]

isGoal :: Board -> Bool
isGoal b = (colorb == goalArray)
    where colorb = boardToGoal b


isGoalM :: State Board Bool
isGoalM = do 
    cur <- get
    let goal = isGoal cur
    pure goal 

distance :: Board -> Int 
distance b = length [k | k<-[1, length elsb], (elsb !! k) == (elsgoal !!k)]
    where elsb = A.elems $ boardToGoal b 
          elsgoal = A.elems goalArray
    --where value = sum [ 1 | (b A.! (k1,k2)) == (goalArray A.! (k1,k2)) , 0<k1, k1<2, 0<k2, k2<2]


heuristic :: State Board Int 
heuristic = do 
        cur <- get 
        pure $ distance cur 


orderQueue :: [(Board, (Piece, Move))] -> [(Piece, Move)]
orderQueue myqueue = fmap (snd. fst) (L.sortBy f pairs)
        where pairs = zip myqueue (fmap distance (fmap fst myqueue))
              f (x,y) (x',y') = y `compare` y'


sortMovesM :: [(Piece, Move)] -> State Board [(Piece, Move)]
sortMovesM listMoves = do 
                    cur <- get
                    let pieces = fmap fst listMoves
                        moves = fmap snd listMoves 
                    result <- zipWithM (\piece move -> execLocally $ movePieceM piece move) pieces moves

                    let result0 = zip result listMoves
                        result' = orderQueue result0
                    pure result'
                    

-- algorithm: at a given state, compute availablemoves with availableM, choose one and update board with movepieceM, check if we are in goal state -> repeat
-- i need heuristic to choose the move: distance to goal state, and i need to store in memory the boards already visited.    
-- A*? genetic alg
-- get :: State Board Board
-- put :: Board -> State Board () 


depthFirst::  [Board] -> Int -> State Board (Maybe [(Piece,Move)])
depthFirst listExplored 0 = pure Nothing
depthFirst listExplored k = do 
                        cur <- get
                        goalPred <- isGoalM 
                        if goalPred 
                            then pure $ Just [] 
                            else do
                                -- add availableM to listMoves
                                listMoves <- availableM
                                sortedMoves <- sortMovesM listMoves
                                result <- foldM f Nothing sortedMoves --it's not a foldM, it should be the queue
                                -- instead of a foldM, apply fst and remove from queue
                                pure result  
    where f (Just result) (p, m) = pure $ Just result
          f (Nothing) (p, m) = do 
            store <- get 
            movePieceM p m 
            newState <-get 
            if newState `elem` listExplored
                then put store >> pure Nothing
                else do
                    --sortedMoves <- sortMovesM queue
                    retM <- depthFirst (store : listExplored) (k-1)
                    case retM of 
                        Nothing -> put store >> pure Nothing 
                        Just r -> pure $ Just $ (p,m):r




depthFirst1::  [Board] -> [(Piece, Move)] -> Int -> State Board (Maybe [(Piece,Move)])
depthFirst1 listExplored queue 0 = pure Nothing
depthFirst1 listExplored queue k = do 
                        cur <- get
                        goalPred <- isGoalM 
                        if goalPred 
                            then pure $ Just [] 
                            else do
                                -- add availableM to listMoves
                                listMoves <- availableM
                                let listMoves' = listMoves ++ queue
                                sortedMoves <- sortMovesM listMoves'
                                result <- f Nothing (head sortedMoves) 
                                let queue = tail sortedMoves
                                --it's not a foldM, it should be the queue
                                -- instead of a foldM, apply fst and remove from queue
                                pure result  
    where f (Just result) (p, m) = pure $ Just result
          f (Nothing) (p, m) = do 
            store <- get 
            movePieceM p m 
            newState <-get 
            if newState `elem` listExplored
                then put store >> pure Nothing
                else do
                    --sortedMoves <- sortMovesM queue
                    retM <- depthFirst1 (store : listExplored) queue (k-1)
                    case retM of 
                        Nothing -> put store >> pure Nothing 
                        Just r -> pure $ Just $ (p,m):r
