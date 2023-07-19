-- intmap Board: for each case, the number of the piece.

-- intmap, for each number, a list of the cases the given piece occupies. 0 for emptycases

-- compute possible moves: given the emptycases, computes adjacent pieces and possible moves

--move: given a piece, computes move

import qualified Data.IntMap.Strict as M
import Data.Maybe
import qualified Data.Array as A
import Control.Monad.State 
 

type Board = A.Array (Int, Int) Piece
type Position = (Int, Int)
type Piece = Int
data Move = U | D | L | R
        deriving (Show, Eq) 
--type Pieces = M.IntMap [Position]

initialArray :: A.Array (Int, Int) Int
initialArray = A.array ((1,1),(5,5)) [((1,1),1), ((1,2),2),((1,3),2), ((1,4),3),((1,5), 4),((2,1),1),((2,2),2), ((2,3),3),((2,4),3),((2,5),4), ((3,1),5), ((3,2),6),((3,3),7), ((3,4),7),((3,5),8),((4,1),9),((4,2),9), ((4,3),8),((4,4),8),((4,5),8), ((5,1),0),((5,2),0), ((5,3),0),((5,4),0),((5,5),0)]


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

availablePieceMoveM :: Piece -> Move -> State Board Bool
availablePieceMoveM p m = do 
                cur <- get 
                listPos <- getPositionsM p 
                let candidatePos = movePiece listPos m
                    available = and [ (cur A.! k)== 0| k <- candidatePos ]
                pure available


-- from her on it doesn't work

availablePieceM :: Piece -> State Board [(Piece,Move)]
availablePieceM p = do 
            cur <- get
            --available <- [m | m <-[U, D, L, R], availablePieceMoveM p m ]
            available <- filterM (\m -> availablePieceMoveM p m) [U, D, L, R]
            let listav = zip (repeat p ) available
            pure listav

{-computeAdjacentM :: State Board [Piece]
computeAdjacentM = do
    cur <- get
    listPos <- getPositionsM 0 -- get the list of free cases
    let adjacents = concatMap (\pos -> zip (repeat pos) [U, D, L, R]) listPos -- generates all cases adjacent to free ones
    adjpieces <- filterM (\pos -> liftM (/= 0) (getCase pos)) listPos -- computes pieces adjacent
    pieces <- mapM (\pos -> getCase pos) adjpieces -- retrieve the pieces using `getCase`
    pure pieces-}

computeAdjacentM :: State Board [Piece]
computeAdjacentM = do
            cur <- get 
            listPos <- getPositionsM 0 --get the list of free cases
            let candidates = concatMap (\pos -> zip (repeat pos) [ U, D, L, R]) listPos
                adjacents = fmap (\(pos, mov) -> moveCase pos mov) candidates
            --concat $ fmap moveCase zip listPos (repeat [U, D, L, R]) --computes all cases adjacent to free ones
            --adjpieces <- [getCase pos | pos <- adjacents, getCase pos /= 0]   -- computes pieces adjacent
            adjpieces0 <- filterM (\pos -> liftM (/= 0) (getCase pos)) adjacents
                --(\pos -> getCase pos /= 0) adjacents)
            adjpieces <- mapM (\pos -> getCase pos) adjpieces0 -- retrieve the pieces using `getCase`
            pure adjpieces                                                  

availableM :: State Board [(Piece, Move)]
availableM = do 
        cur <- get 
        candidatePieces <- computeAdjacentM
        candidateMoves <- concatMap mapM availablePieceM candidatePieces 
        pure candidateMoves

isGoal :: Board -> Bool
isGoal b = (b == goalArray)

isGoalM :: State Board Bool
isGoalM = do 
    cur <- get
    let goal = isGoal cur
    pure goal 


goalArray :: A.Array (Int, Int) Int
goalArray = A.array ((1,1),(5,5)) [((1,1),7), ((1,2),7),((1,3),0), ((1,4),2),((1,5), 2),((2,1),0),((2,2),0), ((2,3),3),((2,4),2),((2,5),1), ((3,1),0), ((3,2),3),((3,3),3), ((3,4),4),((3,5),1),((4,1),5),((4,2),9), ((4,3),9),((4,4),4),((4,5),8), ((5,1),0),((5,2),6), ((5,3),8),((5,4),8),((5,5),8)]


-- algorithm: at a given state, compute availablemoves with availableM, choose one and update board with movepieceM, check if we are in goal state -> repeat
-- i need heuristic to choose the move: distance to goal state, and i need to store in memory the boards already visited.    
-- A*? genetic alg
-- get :: State Board Board
-- put :: Board -> State Board () 