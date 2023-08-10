import qualified Data.IntMap.Strict as M
import Data.Maybe
import qualified Data.Array as A
import Control.Monad.State 
import Debug.Trace 
import qualified Data.List as L
 

data GameState = GameState { _board :: A.Array (Int, Int) Piece, _heuristic :: Int }
type Board = A.Array (Int, Int) Piece
type Position = (Int, Int)
type Piece = Int
type Queue = [(GameState, Trajectory)]
data Trajectory = Trajectory { _path :: [(Piece, Move)], _cost :: Int}
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

initialArray :: A.Array (Int, Int) Int
initialArray = A.array ((1,1),(5,5)) [((1,1),1), ((1,2),2),((1,3),2), ((1,4),3),((1,5), 4),((2,1),1),((2,2),2), ((2,3),3),((2,4),3),((2,5),4), ((3,1),5), ((3,2),6),((3,3),7), ((3,4),7),((3,5),8),((4,1),9),((4,2),9), ((4,3),8),((4,4),8),((4,5),8), ((5,1),0),((5,2),0), ((5,3),0),((5,4),0),((5,5),0)]
--initialArray = A.array ((1,1),(3,3)) [((1,1),1), ((1,2),0),((1,3),0),((2,1),0),((2,2),0),((2,3),0),((3,1),0),((3,2),0),((3,3),0)]

initialState = GameState initialArray (distance initialArray)

{-initialArray :: Board
--initialArray = A.array ((1,1),(5,5)) [((1,1),1), ((1,2),2),((1,3),2), ((1,4),3),((1,5), 4),((2,1),1),((2,2),2), ((2,3),3),((2,4),3),((2,5),4), ((3,1),5), ((3,2),6),((3,3),7), ((3,4),7),((3,5),8),((4,1),9),((4,2),9), ((4,3),8),((4,4),8),((4,5),8), ((5,1),0),((5,2),0), ((5,3),0),((5,4),0),((5,5),0)]
initialArray = A.array ((1,1),(5,5)) [((1,1),0), ((1,2),7),((1,3),7), ((1,4),2),((1,5), 2),((2,1),0),((2,2),0), ((2,3),3),((2,4),2),((2,5),1), ((3,1),0), ((3,2),3),((3,3),3), ((3,4),4),((3,5),1),((4,1),5),((4,2),9), ((4,3),9),((4,4),4),((4,5),8), ((5,1),0),((5,2),6), ((5,3),8),((5,4),8),((5,5),8)]


goalArray :: A.Array (Int, Int) Int
goalArray = A.array ((1,1),(5,5)) [((1,1),7), ((1,2),7),((1,3),0), ((1,4),2),((1,5), 2),((2,1),0),((2,2),0), ((2,3),3),((2,4),2),((2,5),1), ((3,1),0), ((3,2),3),((3,3),3), ((3,4),4),((3,5),1),((4,1),5),((4,2),9), ((4,3),9),((4,4),4),((4,5),8), ((5,1),0),((5,2),6), ((5,3),8),((5,4),8),((5,5),8)]




goalArray :: A.Array (Int, Int) Int
goalArray = A.array ((1,1),(3,3)) [((1,1),0), ((1,2),0),((1,3),0),((2,1),0),((2,2),0),((2,3),0),((3,1),0),((3,2),0),((3,3),1)]
-}

{-getPiece:: Pieces -> Int -> [Position]
getPiece pieces k = pieces M.! k-}

getCase :: Position -> State GameState Piece 
getCase (i,j) = do 
                cur <- gets _board 
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

execLocally :: State GameState a -> State GameState GameState
execLocally function = do 
                    cur <- get 
                    result <- function
                    result <- get
                    put cur
                    pure result
                 
                    


getPositionsM :: Piece -> State GameState [Position]
getPositionsM p = do 
    cur <- gets _board 
    let alist = A.assocs cur :: [(Position,Piece)]
        flist = filter f alist
        f:: (Position, Piece) -> Bool 
        f (pos, p') = p==p'
        posList = fmap fst flist
                  
    pure posList


movePieceM :: Piece -> Move -> State GameState ()
movePieceM p m = do
            cur <- gets _board
            listPos <- getPositionsM p 
            let newPos = movePiece listPos m 
                newboard0 = cur A.// zip listPos (repeat 0)
                newboard = newboard0 A.// zip newPos (repeat p)
            modify (\s ->s{_board = newboard, _heuristic = distance newboard})


inBoardP :: Board  -> (Int,Int) -> Bool 
inBoardP b (k1,k2) = and [0<k1,  k1<(n1+1), 0<k2, k2<(n2+1)]
    where (_,(n1, n2)) =A.bounds b 

availablePieceMoveM :: Piece -> Move -> State GameState Bool
availablePieceMoveM p m = do 
                cur <- gets _board 
                listPos <- getPositionsM p 
                let candidatePos = movePiece listPos m
                    available = and [ (cur A.! k)== 0| k <- candidatePos ] -- checks case is empty
                    (_,(n1, n2)) = A.bounds cur 
                    available1 = and $ fmap (inBoardP cur) candidatePos
                    --[ and [0<k1,  k1<(n1+1), 0<k2, k2<(n2+1)] | (k1,k2) <- candidatePos ] 

                pure available1




availablePieceM :: Piece -> State GameState [(Piece,Move)]
availablePieceM p = do 
            --cur <- gets _board
            available <- filterM (\m -> availablePieceMoveM p m) [U, D, L, R]
            let listav = zip (repeat p ) available
            pure listav



computeAdjacentM :: State GameState [Piece]
computeAdjacentM = do
            cur <- gets _board 
            listPos <- getPositionsM 0 --get the list of free cases
            let candidates = concatMap (\pos -> zip (repeat pos) [ U, D, L, R]) listPos
                adjacents0 = fmap (\(pos, mov) -> moveCase pos mov) candidates
                adjacents = filter (inBoardP cur) adjacents0
            adjpieces0 <- filterM (\pos -> liftM (/= 0) (getCase pos)) adjacents
            adjpieces <- mapM (\pos -> getCase pos) adjpieces0 -- retrieve the pieces using `getCase`
            pure $ L.nub adjpieces                                                  

availableM :: State GameState [(Piece, Move)]
availableM = do 
        --cur <- get 
        candidatePieces <- computeAdjacentM
        candidateMoves <-  mapM  availablePieceM candidatePieces 
        pure $ concat candidateMoves --i want to concatenate this so i can return [(Piece, Move)]

isGoal :: Board -> Bool
isGoal b = (colorb == goalArray)
    where colorb = boardToGoal b


isGoalM :: State GameState Bool
isGoalM = do 
    cur <- gets _board
    let goal = isGoal cur
    pure goal 


distance :: Board -> Int 
distance b = (length [v | (v,v')<-zip elsb elsgoal, v /= v']) `div` 8
    --length [k | k<-[0.. (length elsb) -1], (elsb !! k) == (elsgoal !!k)]
    where elsb = A.elems $ boardToGoal b 
          elsgoal = A.elems goalArray
    --where value = sum [ 1 | (b A.! (k1,k2)) == (goalArray A.! (k1,k2)) , 0<k1, k1<2, 0<k2, k2<2]


heuristic :: State GameState Int 
heuristic = distance <$> gets _board


orderQueue :: [(GameState, (Piece, Move))] -> [(Piece, Move)]
orderQueue myqueue = fmap (snd. fst) (L.sortBy f pairs)
        where pairs = zip myqueue (fmap (_heuristic .fst) myqueue)
              f (x,y) (x',y') = y `compare` y'


sortMovesM :: [(Piece, Move)] -> State GameState [(Piece, Move)]
sortMovesM listMoves = do 
                    cur <- gets _board
                    let pieces = fmap fst listMoves
                        moves = fmap snd listMoves 
                    result <- zipWithM (\piece move -> execLocally $ movePieceM piece move) pieces moves

                    let result0 = zip result listMoves
                        result' = orderQueue result0
                    pure result'
                    
aStar:: Int -> Trajectory -> Queue -> State GameState (Maybe Trajectory)
aStar maxCost trajectory queue = do
        goalP <- isGoalM
        if goalP 
            then pure $ Just trajectory
            else aStar' trajectory queue
    where 
          aStar' _ [] = pure Nothing
          aStar' trajectory queue = do
                newstates <- expand
                let ((nextstate, nexttrajectory), queue') = select (queue ++ newstates)
                put nextstate 
                aStar maxCost (nexttrajectory{ _cost = _cost nexttrajectory -1}) queue'
          expand :: State GameState Queue
          expand = do 
                 listMoves <- availableM -- [(piece, move)]
                 --movePieceM              -- piece move
                 expanded <- forM listMoves $ \(p,m) -> do 
                                            nextstate <- execLocally $ movePieceM p m  
                                            pure (nextstate, Trajectory ((p,m):_path trajectory) (_cost trajectory  +1))
                 pure [(gs, traj) | (gs, traj) <- expanded, _cost traj <= maxCost]
          select :: Queue -> ((GameState, Trajectory), Queue) 
          select myqueue = extractMin f myqueue
          f :: (GameState, Trajectory) -> Int 
          f (gs, traj) = _heuristic gs + _cost traj 

extractMin :: (a-> Int) -> [a] -> (a, [a]) 
extractMin feval l = foldl f (head l, []) $ tail l 
    where f (curmin, curlist) el 
                | feval curmin <= feval el = (curmin, el:curlist)
                | feval curmin > feval el = (el, curmin:curlist) 


{-
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

{-breadthFirst:: [Board] -> [(Piece, Move)] -> Int -> State Board (Maybe [(Piece, Move)])
-- initialize queue to initialArray
breadthFirst  listExplored queue 0 = pureNothing
breadthFirst listExplored queue k = do 
                                cur <- get 
                                listMoves0 <- availableM 
                                goalPred <- mapM isGoalM listMoves0
                                if or goalPred 
                                    then pure $ Just []
                                else do 
                                    let listMoves = queue ++ listMoves0 --get availablemoves of all states in queue
                                        sortedMoves = sortedMoves listMoves -- sort queue
                                        --result <- --call breadthFirst of every node of depth k (breadthFirst should only explore next depth)
                                    mapM breadthFirst listExplored queue k-1
-}

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
                    retM <- depthFirst1 (store : listExplored) queue (k-1) --this is wrong
                    case retM of 
                        Nothing -> put store >> pure Nothing 
                        Just r -> pure $ Just $ (p,m):r
-}