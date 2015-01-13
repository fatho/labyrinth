module Game where

import Array
import Color
import Dict
import List
import List ((::))
import Maybe
import Set
import Random
import Random.Array as RndArr

import Util

-- | Currently, the treasures to find are simply represented by a string. This might change.
type alias Treasure = String

-- | A piece is represented by which sides are open (<=> corresponding field is True).
type alias Piece = 
  { left   : Bool
  , top    : Bool
  , right  : Bool
  , bottom : Bool
  , treasure : Maybe Treasure
  }

-- | Coordinate in the labyrinth, zero based.
type alias Coord = (Int, Int)

-- | The state of a running game. Consisting of the board itself and the participating players.
type alias Board = 
  { width     : Int
  , height    : Int
  -- ^ size of the board
  , pieces    : Dict.Dict Coord Piece
  -- ^ pieces on the board
  , freePiece : Piece
  -- ^ piece used for shifting
  , players   : List Player
  -- ^ participating players
  , lastShift : Maybe Shift
  -- ^ the last shift that was performed
  , collectedTreasures : Set.Set Treasure
  -- ^ set of treasures already collected
  }

type alias Player =
  { home     : Coord
  -- ^ start position of player (player has to return here after getting all treasures)
  , position : Coord
  -- ^ current position of player
  , color    : Color.Color
  -- ^ player color (distinction on the board)
  , name     : String
  -- ^ players name (just for display purposes)
  , targets  : List Treasure
  -- ^ the treasures this player has to find in that order.
  }

-- | A shift action is characterized by the side where the new piece 
-- is shifted in and the index of that row or column.
type alias Shift = 
  { side : Orientation
  , index : Int }


type Orientation = North | East | South | West

type AngularDir = CW | CCW


-- * Piece manipulation functions

rotate : AngularDir -> Piece -> Piece
rotate dir blk = case dir of 
  CW -> { blk 
    | left <- blk.bottom, top <- blk.left
    , right <- blk.top, bottom <- blk.right }
  CCW -> { blk 
    | right <- blk.bottom, bottom <- blk.left
    , left <- blk.top, top <- blk.right }

-- | Rotates a piece s.t. the current top points in the direction given by Orientation.
rotateTo : Orientation -> Piece -> Piece
rotateTo o p = case o of
  North -> p 
  East -> rotate CW p
  South -> rotate CW <| rotate CW p
  West -> rotate CCW p

-- | Rotates a piece to a random direction.
rotateRandom : Piece -> Random.Seed -> (Piece, Random.Seed)
rotateRandom p seed = let (o,s) = Random.generate orientationGen seed
  in (rotateTo o p, s)

-- | Random generator for Orientation values.
orientationGen : Random.Generator Orientation
orientationGen = 
  let orientations = Array.fromList [North, East, South, West]
      gen seed = let (x,s) = RndArr.sample seed orientations in (Maybe.withDefault North x, s)
  in Random.customGenerator gen

-- | Converts an Orientation value to clockwise radians. 0 is up.
orientationAngle : Orientation -> Float
orientationAngle o = case o of
  North -> 0
  East -> degrees 90
  South -> degrees 180
  West -> degrees 270

-- | Checks if the piece is open on the given side.
isOpenOn : Piece -> Orientation -> Bool  
isOpenOn piece dir = case dir of
  North -> piece.top
  West -> piece.left
  East -> piece.right
  South -> piece.bottom

-- | Returns the opposite side of a given Orientation value.
opposite : Orientation -> Orientation
opposite o = case o of
  West -> East
  East -> West
  North -> South
  South -> North

isHorizontal : Orientation -> Bool
isHorizontal o = o == West || o == East
isVertical : Orientation -> Bool
isVertical = not << isHorizontal

-- | Offsets a coordinate by one in the given direction.
to : Coord -> Orientation -> Coord
to (x,y) dir = case dir of
  West -> (x-1,y)
  North -> (x,y-1)
  East -> (x+1,y)
  South -> (x,y+1)

-- * All piece types modulo rotation

-- | Orientation (modulo 2) determines the direction.
straight : Orientation -> Maybe Treasure -> Piece
straight o t = rotateTo o { left = False, right = False, top = True, bottom = True, treasure = t }

-- | Orientation determines the position of the open side on the opposite of the closed side.
tjunction : Orientation -> Maybe Treasure -> Piece
tjunction o t = rotateTo o { left = True, right = True, top = True, bottom = False, treasure = t }

-- | A junction is open to all four sides.
junction : Maybe Treasure -> Piece
junction t = { left = True, right = True, top = True, bottom = True, treasure = t }

-- | Orientation determines the position of the first open side (clockwisely).
curve : Orientation -> Maybe Treasure -> Piece
curve o t = rotateTo o { left = False, right = True, top = True, bottom = False, treasure = t }

-- * Specific game rules (fixed size and limited number of players)

-- Initial player values for a maximum of four players
playerColors = [Color.red, Color.blue, Color.green, Color.yellow]
playerHomes = [(0,0), (6,6), (0,6), (6,0)]

-- | Initializes the Player values with random targets.
initialPlayers : Random.Seed -> List String -> (List Player, Random.Seed)
initialPlayers seed names = 
  let (targets, seed') = RndArr.shuffle seed allTreasures
      targetsPerPlayer = Array.length targets // List.length names
      targetsFor idx   = Array.slice (idx * targetsPerPlayer) ((idx + 1) * targetsPerPlayer) targets
      mkPlayer name col home idx = { color = col, home = home
        , position = home, name = name, targets = Array.toList (targetsFor idx) }
  in (List.map4 mkPlayer names playerColors playerHomes [0..3], seed')

-- | Creates a random initial board layout.
initialBoard : Random.Seed -> List Player -> (Board, Random.Seed)
initialBoard seed players = 
  let -- | Calculate positions of fixed pieces
      prepareRow r ps = List.map2 (prepareCol r) [0..3] ps
      prepareCol r c p = ((2*c,2*r), p)
      fixedPieces = Dict.fromList <| List.concat <| List.map2 prepareRow [0..3] initialPieces
      -- | Randomize loose pieces
      (randomArr, seed') = loosePieces
        |> Array.fromList
        |> RndArr.shuffle seed
      -- | Rotates loose pieces to random directions and splits of the free piece.
      (freePiece::randomLst, seed'') = Array.toList randomArr
        |> Util.mapAccumL rotateRandom seed'
      -- | Get coordinates of loose pieces (coordinates that are not in the fixedPieces dict)
      loosePieceCoords = Util.cartProd [0..6] [0..6]
        |> List.filter (not << flip Dict.member fixedPieces)
      -- | Associate loose pieces with coordinates
      movablePieces = Dict.fromList <| List.map2 (,) loosePieceCoords randomLst
  in ( { width = 7, height = 7
       , pieces = Dict.union fixedPieces movablePieces
       , freePiece = freePiece, players = players
       , lastShift = Nothing, collectedTreasures = Set.empty }
     , seed'')

-- | Initializes a new game from a seed and a list of player names.
newGame : Random.Seed -> List String -> (Board, Random.Seed)
newGame seed playerNames = 
  let (players, s0) = initialPlayers seed playerNames
      (board, s1)   = initialBoard s0 players
  in (board, s1)

-- | Fixated pieces on the board, row major.
initialPieces : List (List Piece)
initialPieces = 
  [[ curve East Nothing, tjunction South (Just "Book"), tjunction South (Just "Coin Purse"), curve South Nothing ]
  ,[ tjunction East (Just "Map"), tjunction East (Just "Crown"), tjunction South (Just "Keys"), tjunction West (Just "Skull")]
  ,[ tjunction East (Just "Ring"), tjunction North (Just "Chest"), tjunction West (Just "Emerald"), tjunction West (Just "Sword")]
  ,[ curve North Nothing, tjunction North (Just "Chandelier"), tjunction North (Just "Helmet"), curve West Nothing ]]

-- | List of loose pieces in the game.
loosePieces : List Piece
loosePieces = List.concat
  [[ curve North Nothing, tjunction North (Just "Fairy"), straight North Nothing ]
  ,[ curve North (Just "Salamander"), tjunction North (Just "Ghost"), straight North Nothing
   , straight North Nothing, curve North Nothing, curve North Nothing, straight North Nothing]
  ,[ curve North Nothing, straight North Nothing, straight North Nothing]
  ,[ curve North Nothing, straight North Nothing, curve North Nothing, curve North Nothing, straight North Nothing
   , tjunction North (Just "Halfling"), straight North Nothing]
  ,[ curve North Nothing, curve North Nothing, curve North Nothing ]
  ,[ straight North Nothing, curve North (Just "Moth"), straight North Nothing, tjunction North (Just "Djinn"), tjunction North (Just "Bat")
   , tjunction North (Just "Dragon"), curve North (Just "Spider")]
  ,[ straight North Nothing, curve North (Just "Owl"), curve North (Just "Rat"), curve North (Just "Scarab")] ]

-- | A list of all treasures
allTreasures : Array.Array Treasure
allTreasures = List.concat initialPieces ++ loosePieces
  |> List.filterMap .treasure
  |> Array.fromList

-- * Game rules (size and player agnostic)

-- | Assigns a rank to a player. Lower is better.
-- If the player found all its targets and returned to the home base, the rank is 0,
-- otherwise it's the number of remaining targets plus 1.
rank : Player -> Int
rank p = 
  let remaining = List.length p.targets
  in if remaining == 0 && p.position == p.home 
    then 0 else remaining + 1

-- | On a board side of a given length, return the movable indices.
movableIndices : Int -> List Int
movableIndices len = List.map (\p -> p*2 + 1) [0..len // 2 - 1]

movableRows : Board -> List Int
movableRows board = movableIndices board.height

movableCols : Board -> List Int
movableCols board = movableIndices board.width

cellMovable : Coord -> Bool
cellMovable (x,y) = (x % 2 == 1) || (y % 2 == 1)

-- | Next players turn.
nextPlayer : Board -> Board
nextPlayer board = let (p::ps) = board.players
  in { board | players <- ps ++ [p] }

-- | The current player.
currentPlayer : Board -> Player
currentPlayer board = List.head board.players

hasWon : Player -> Bool
hasWon player = player.targets == [] && player.position == player.home

updateCurrentPlayer : Player -> Board -> Board
updateCurrentPlayer newpl board =
  let (_::ps) = board.players
  in { board | players <- newpl::ps }

-- | Rotates the free piece.
rotateFreePiece : AngularDir -> Board -> Board
rotateFreePiece rot board = { board | freePiece <- rotate rot board.freePiece }

-- | Moves the current player's token.
moveTo : Orientation -> Board -> Board
moveTo side board = 
  let player      = currentPlayer board
      updplayer   = { player | position <- player.position `to` side }
      playerPiece = Dict.get player.position board.pieces
      adjacentPiece = Dict.get (player.position `to` side) board.pieces

      canMove = case (playerPiece, adjacentPiece) of
        (Just p, Just a) -> (p `isOpenOn` side) &&  (a `isOpenOn` opposite side)
        _ -> False

  in if canMove
    then updateCurrentPlayer updplayer board
    else board

processTarget : Board -> Board
processTarget board =
  let curPlayer = currentPlayer board
  in case Dict.get curPlayer.position board.pieces of
      Just curPiece -> case (currentPlayer board).targets of
        [] -> board
        (curTarget::rest) -> if curPiece.treasure == Just curTarget
          then { board  | collectedTreasures <- Set.insert curTarget board.collectedTreasures }
            |> updateCurrentPlayer { curPlayer | targets <- rest } 
          else board
      Nothing -> board

shift : Shift -> Board -> Board
shift shiftCmd board = 
  let -- positions of the pieces that are shifted
      shiftCoords = if isHorizontal shiftCmd.side 
        then List.map (\i -> (i, shiftCmd.index)) [0..board.width-1]
        else List.map (\i -> (shiftCmd.index, i)) [0..board.width-1]
      -- the direction we shift in
      shiftDir = opposite shiftCmd.side

      -- the position of the piece that is shifted out
      shiftEdgePos side = case side of
        North -> (shiftCmd.index, board.height-1)
        South -> (shiftCmd.index, 0)
        West -> (board.width-1, shiftCmd.index)
        East -> (0, shiftCmd.index)

      shiftedOut = shiftEdgePos shiftCmd.side
      shiftedIn  = shiftEdgePos shiftDir

      -- Partition pieces by which get shifted and which stay in place
      shifted (x,y) = if isHorizontal shiftCmd.side
        then y == shiftCmd.index else x == shiftCmd.index
      (removed,rest) = Dict.partition (\pos _ -> shifted pos) board.pieces
      -- Insert shifted
      doShift pos dict = if pos == shiftedOut
        then dict else Dict.insert (pos `to` shiftDir) (Util.unsafeGet pos removed) dict
      newBoard = List.foldl doShift rest shiftCoords -- insert shifted pieces
        |> Dict.insert shiftedIn board.freePiece -- insert new piece
      -- piece that has fallen out
      newFree = Util.unsafeGet shiftedOut removed

      relocateToken player = if shifted player.position
        then { player | position <- if player.position == shiftedOut then shiftedIn else player.position `to` shiftDir }
        else player
      -- TODO: Take care of player figures
  in { board | pieces <- newBoard, freePiece <- newFree, players <- List.map relocateToken board.players, lastShift <- Just shiftCmd }

