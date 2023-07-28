-- intmap Board: for each case, the number of the piece.

-- intmap, for each number, a list of the cases the given piece occupies. 0 for emptycases

-- compute possible moves: given the emptycases, computes adjacent pieces and possible moves

--move: given a piece, computes move

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
--type Pieces = M.IntMap [Position]

{-initialArray :: A.Array (Int, Int) Int
--initialArray = A.array ((1,1),(5,5)) [((1,1),1), ((1,2),2),((1,3),2), ((1,4),3),((1,5), 4),((2,1),1),((2,2),2), ((2,3),3),((2,4),3),((2,5),4), ((3,1),5), ((3,2),6),((3,3),7), ((3,4),7),((3,5),8),((4,1),9),((4,2),9), ((4,3),8),((4,4),8),((4,5),8), ((5,1),0),((5,2),0), ((5,3),0),((5,4),0),((5,5),0)]
initialArray = A.array ((1,1),(5,5)) [((1,1),0), ((1,2),7),((1,3),7), ((1,4),2),((1,5), 2),((2,1),0),((2,2),0), ((2,3),3),((2,4),2),((2,5),1), ((3,1),0), ((3,2),3),((3,3),3), ((3,4),4),((3,5),1),((4,1),5),((4,2),9), ((4,3),9),((4,4),4),((4,5),8), ((5,1),0),((5,2),6), ((5,3),8),((5,4),8),((5,5),8)]


goalArray :: A.Array (Int, Int) Int
goalArray = A.array ((1,1),(5,5)) [((1,1),7), ((1,2),7),((1,3),0), ((1,4),2),((1,5), 2),((2,1),0),((2,2),0), ((2,3),3),((2,4),2),((2,5),1), ((3,1),0), ((3,2),3),((3,3),3), ((3,4),4),((3,5),1),((4,1),5),((4,2),9), ((4,3),9),((4,4),4),((4,5),8), ((5,1),0),((5,2),6), ((5,3),8),((5,4),8),((5,5),8)]
-}

initialArray :: A.Array (Int, Int) Int
--initialArray = A.array ((1,1),(5,5)) [((1,1),1), ((1,2),2),((1,3),2), ((1,4),3),((1,5), 4),((2,1),1),((2,2),2), ((2,3),3),((2,4),3),((2,5),4), ((3,1),5), ((3,2),6),((3,3),7), ((3,4),7),((3,5),8),((4,1),9),((4,2),9), ((4,3),8),((4,4),8),((4,5),8), ((5,1),0),((5,2),0), ((5,3),0),((5,4),0),((5,5),0)]
initialArray = A.array ((1,1),(3,3)) [((1,1),1), ((1,2),0),((1,3),0),((2,1),0),((2,2),0),((2,3),0),((3,1),0),((3,2),0),((3,3),0)]


goalArray :: A.Array (Int, Int) Int
goalArray = A.array ((1,1),(3,3)) [((1,1),0), ((1,2),0),((1,3),0),((2,1),0),((2,2),0),((2,3),0),((3,1),0),((3,2),0),((3,3),1)]


{-getPiece:: Pieces -> Int -> [Position]
getPiece pieces k = pieces M.! k-}

getCase :: Position -> State Board Piece 
getCase (i,j) = do 
                cur <- get 
                let piece = cur A.! (i,j)
                pure piece 


moveCase :: Position -> Move -> Position
moveCase (i,j) U = (i,j+1)
moveCase (i,j) D = (i,j-1)
moveCase (i,j) R = (i+1,j)
moveCase (i,j) L = (i-1,j)

movePiece :: [Position] -> Move -> [Position]
movePiece listp s = zipWith moveCase listp (repeat s)

{-move:: Pieces -> Move -> Int -> Pieces
-- move piece k Up
move piece s k = M.adjust (f) (k) (piece)
    where 
          f = (\listpos -> movePiece listpos s)-}

getPositionsM :: Piece -> State Board [Position]
getPositionsM p = do 
    cur <- get 
    let alist = A.assocs cur :: [(Position,Piece)]
        flist = filter f alist
        f:: (Position, Piece) -> Bool 
        f (pos, p') = p==p'
        posList = fmap fst flist
                  
    pure posList



--movePieceS :: Board -> Piece -> Move -> Board
movePieceM :: Piece -> Move -> State Board ()
movePieceM p m = do
            cur <- get
            listPos <- getPositionsM p 
            let newPos = movePiece listPos m 
                newboard = cur A.// zip newPos (repeat p)
                newboard2 = newboard A.// zip listPos (repeat 0)
            put newboard2


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
            --concat $ fmap moveCase zip listPos (repeat [U, D, L, R]) --computes all cases adjacent to free ones
            --adjpieces <- [getCase pos | pos <- adjacents, getCase pos /= 0]   -- computes pieces adjacent
            adjpieces0 <- filterM (\pos -> liftM (/= 0) (getCase pos)) adjacents
                --(\pos -> getCase pos /= 0) adjacents)
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
isGoal b = (b == goalArray)

isGoalM :: State Board Bool
isGoalM = do 
    cur <- get
    let goal = isGoal cur
    pure goal 


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
                                listMoves <- availableM
                                --let sortedMoves 
                                result <- foldM f Nothing listMoves
                                pure result  
    where f (Just result) (p, m) = pure $ Just result
          f (Nothing) (p, m) = do 
            store <- get 
            movePieceM p m 
            newState <-get 
            if newState `elem` listExplored
                then put store >> pure Nothing
                else do
                    retM <- depthFirst (store : listExplored) (k-1)
                    case retM of 
                        Nothing -> put store >> pure Nothing 
                        Just r -> pure $ Just $ (p,m):r
