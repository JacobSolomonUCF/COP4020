module Chess where
-- See https://en.wikipedia.org/wiki/Chess for more details
-- We only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)   

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

isLegalPosition :: Position -> Bool
isLegalPosition pos = pos `elem` board
	where board=[(x,y)|x<-['a'..'h'],y<-[1..8]]


isLegalMove :: Color -> Piece -> Position -> Position -> Bool
isLegalMove _ King pos0 pos1  = kgm pos0 pos1
isLegalMove _ Queen pos0 pos1 = qm pos0 pos1
isLegalMove _ Rook pos0 pos1 = rm pos0 pos1
isLegalMove _ Bishop pos0 pos1 = bm pos0 pos1
isLegalMove _ Knight pos0 pos1 = knm pos0 pos1 && isLegalPosition pos0 && isLegalPosition pos1
isLegalMove Black Pawn pos0 pos1 = bpm pos0 pos1 && isLegalPosition pos0 && isLegalPosition pos1
isLegalMove White Pawn pos0 pos1 = wpm pos0 pos1 && isLegalPosition pos0 && isLegalPosition pos1

kgm :: Position ->Position->Bool
kgm (c1, x1) dst = ((isEqual (c1, x1-1)dst)||(isEqual(c1,x1+1)dst)||(isEqual((pred c1),x1)dst)||(isEqual((succ c1),x1)dst)||(isEqual((pred c1),x1-1)dst)||(isEqual((pred c1),x1+1)dst)||(isEqual((succ c1),x1-1)dst)||(isEqual((succ c1),x1+1)dst) ) && isLegalPosition (c1,x1) && isLegalPosition dst

isEqual :: Position -> Position -> Bool
isEqual pos1 pos2 = pos1 == pos2

qm :: Position ->Position->Bool
qm src dst = (rm src dst) || (bm src dst)

rm :: Position->Position->Bool
rm (c1, x1) dst = isLegalPosition(c1, x1) && isLegalPosition dst &&( (rookUp (c1, x1+1) dst) || (rookDown (c1, x1-1) dst) || (rookLeft ((pred c1),x1) dst) ||(rookRight ((succ c1),x1) dst) )


rookUp :: Position->Position->Bool
rookUp (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = rookUp (c1, x1+1) dst

rookDown :: Position->Position->Bool
rookDown (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = rookDown (c1, x1-1) dst

rookLeft :: Position->Position->Bool
rookLeft (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = rookLeft ((pred c1), x1) dst

rookRight :: Position->Position->Bool
rookRight (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = rookRight ((succ c1), x1) dst 

bm :: Position ->Position->Bool
bm (c1, x1) dst = isLegalPosition(c1, x1) && isLegalPosition dst &&( (bishopUL ((pred c1), x1+1) dst) || (bishopUR ((succ c1), x1+1) dst) || (bishopDL ((pred c1),x1-1) dst) ||(bishopDR ((succ c1),x1-1) dst) )

bishopUL :: Position->Position->Bool
bishopUL (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = bishopUL( (pred c1), x1+1) dst

bishopUR :: Position->Position->Bool
bishopUR (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = bishopUR( (succ  c1), x1+1) dst

bishopDL :: Position->Position->Bool
bishopDL (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = bishopDL( (pred c1), x1-1) dst

bishopDR :: Position->Position->Bool
bishopDR (c1, x1) dst
  |  ((c1, x1) == dst) = True
  |  (not $ isLegalPosition (c1,x1)  ) = False
  |  otherwise = bishopDR( (succ c1), x1-1) dst



knm :: Position->Position->Bool
knm (c1,x1) dst = isEqual ((pred (pred(c1))),x1+1) dst || isEqual ((pred (pred(c1))),x1-1) dst || isEqual ((succ (succ(c1))),x1+1) dst || isEqual ((succ (succ(c1))),x1+1) dst || isEqual ((succ c1),x1+2) dst || isEqual ((succ c1),x1-2) dst || isEqual ((pred  c1),x1+2) dst ||isEqual ((pred c1),x1-2) dst

bpm :: Position->Position->Bool
bpm (c1,x1) (c2, x2)
	| (x1 == 8) = False
	| (x1 == 7) = (c1 == c2) && ( (x2 == x1-1) || (x2 == x1-2))
	| otherwise = (c1 == c2) && ( (x2 == x1-1)) 

wpm :: Position->Position->Bool
wpm (c1,x1) (c2, x2)
        | (x1 == 1) = False
        | (x1 == 2) = (c1 == c2) && ( (x2 == x1+1) || (x2 == x1+2))
        | otherwise = (c1 == c2) && ( (x2 == x1+1))

