import Array
import Color
import Char
import Date
import Dict
import List
import List ((::))
-- Workaround, since Elm does not accept qualified names in a record update
-- i.e. { GCol.defaultLine | width <- ... } is not possible.
import Graphics.Collage (defaultLine)
import Graphics.Collage as GCol
import Graphics.Element as GEl
import Graphics.Input as GInp
import Maybe (Maybe (..), andThen)
import Maybe
import Mouse
import Keyboard
import Signal
import Signal ((<~),(~), Signal)
import Random
import Random.Array as RndArr
-- Workaround, same as above
import Text
import Text (defaultStyle)
import Time (..)
import Window

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
  , pieces    : Dict.Dict Coord Piece
  , freePiece : Piece
  , players   : List Player
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

type Orientation = North | East | South | West

type AngularDir = CW | CCW

-- | A shift action is characterized by the side where the new piece 
-- is shifted in and the index of that row or column.
type alias Shift = 
  { side : Orientation
  , index : Int }

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

orientationAngle : Orientation -> Float
orientationAngle o = case o of
  North -> 0
  East -> degrees 90
  South -> degrees 180
  West -> degrees 270

isOpenOn : Piece -> Orientation -> Bool  
isOpenOn piece dir = case dir of
  North -> piece.top
  West -> piece.left
  East -> piece.right
  South -> piece.bottom

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

-- * Piece rendering

-- | One third of the piece size.
smallSize = 30
-- | Size of one labyrinth piece.
pieceSize = 3 * smallSize
-- | Size of the labyrinth
boardRealSize board = { width = board.width * pieceSize, height = board.height * pieceSize }

-- | Currently, treasures are rendered as text
renderTreasure : Treasure -> GCol.Form
renderTreasure t = List.map 
  (\off -> Text.fromString t
    |> Text.style { defaultStyle | color <- Color.black, bold <- True }
    |> Text.centered |> GCol.toForm |> GCol.move off
  ) [(1,1),(1,-1),(-1,1),(-1,-1)] ++ [Text.fromString t
    |> Text.style { defaultStyle | color <- Color.lightGrey, bold <- True }
    |> Text.centered |> GCol.toForm] |> GCol.group

-- | Draws a piece to a form. The origin is at the center of the piece.
renderPiece : Piece -> GCol.Form
renderPiece blk = 
  let wall = GCol.square pieceSize |> GCol.filled Color.darkBrown
      way = GCol.square (1.5*smallSize) |> GCol.filled Color.lightBrown
      outline = GCol.square pieceSize |> GCol.outlined (GCol.solid Color.brown)
      -- which sides are walls depends on piece
      sides = List.map2 (,)
        [ blk.left, blk.top, blk.right, blk.bottom ]
        [ (-0.75 * smallSize, 0), (0, 0.75 * smallSize), (0.75 * smallSize, 0), (0, -0.75 * smallSize) ]
      -- filter predicate
      keepIfOpen (opn, x) = if opn then Just x else Nothing
      -- build walls from a list of coordinates
      wayFromCoords = List.map (flip GCol.move way)
      -- If a treasure is present, render it too
      treasureIcon = blk.treasure 
        |> Maybe.map renderTreasure
        |> maybeToList
  in GCol.group <|
    [ wall, way ] ++ (wayFromCoords <| List.filterMap keepIfOpen sides) 
    ++ treasureIcon ++ [outline]

-- * Board rendering

-- | Transforms a board coordinate to a canvas coordinate.
boardToCanvas : Board -> (Int,Int) -> (Float, Float)
boardToCanvas board (x,y) =
  ( toFloat (x - (board.width // 2)) * pieceSize
  , toFloat ((board.height // 2) - y) * pieceSize )

-- | Renders a player's home.
renderHome : Color.Color -> GCol.Form
renderHome col = 
  let homeStyle   = { defaultLine | width <- 3 }
      homeCircle  = GCol.circle (smallSize - 10)
  in GCol.group [ homeCircle |> GCol.filled col, homeCircle |> GCol.outlined homeStyle ]

-- | Renders a player's token.
playerToken : Color.Color -> GCol.Form
playerToken col = 
  let shape = GCol.square (smallSize - 6)
  in GCol.group 
    [ shape |> GCol.filled col, shape |> GCol.outlined defaultLine ] 
    |> GCol.rotate (degrees 45)

-- | The shift button arrow, pointing inwards from the side where it resides.
shiftButtonArrow : Orientation -> GEl.Element
shiftButtonArrow side = 
  let shape = GCol.ngon 3 smallSize
  in GCol.group [ shape |> GCol.filled Color.yellow, shape |> GCol.outlined defaultLine ]
    |> GCol.rotate (degrees 270)
    |> GCol.rotate -(orientationAngle side)
    |> singleton |> GCol.collage pieceSize pieceSize

-- | Renders the board.
renderBoard : Board -> GCol.Form
renderBoard board = 
  let b2c = boardToCanvas board
      renderCell pos = case Dict.get pos board.pieces of
        Nothing -> []
        Just piece -> [renderPiece piece 
          |> GCol.move (b2c pos)]
      -- rendered pieces
      pieces = (,) `List.map` [0..board.width-1] `ap` [0..board.height-1]
        |> List.concatMap renderCell
      -- player bases
      homes = List.map (\p -> renderHome p.color |> GCol.move (b2c p.home)) board.players
      -- player tokens
      tokens = List.map (\p -> playerToken p.color |> GCol.move (b2c p.position)) board.players
  in GCol.group <| pieces ++ homes ++ tokens

-- * Interface rendering

gameTable : Board -> GEl.Element
gameTable board = 
  let shiftButtonSpec = 
        [ (North, movableCols board )
        , (South, movableCols board)
        , (West, movableRows board)
        , (East, movableRows board) ]

      buttonPos side idx = case side of
        West -> boardToCanvas board (-1, idx)
        East -> boardToCanvas board (board.width, idx)
        North -> boardToCanvas board (idx, -1)
        South -> boardToCanvas board (idx, board.height)

      mkButton side idx = shiftButtonArrow side
        |> GInp.clickable (Signal.send shiftChannel { side = side, index = idx} )
        |> GCol.toForm
        |> GCol.move (buttonPos side idx)

      shiftButtons = List.concatMap (\(s,is) -> List.map (mkButton s) is) shiftButtonSpec

      bsize = boardRealSize board
  in [renderBoard board] ++ shiftButtons
    |> GCol.collage (bsize.width+pieceSize*2) (bsize.height+pieceSize*2)

-- * Specific game rules (fixed size and limited number of players)

-- Initial player values for a maximum of four players
playerColors = [Color.red, Color.blue, Color.green, Color.yellow]
playerHomes = [(0,0), (6,6), (0,6), (6,0)]

initialPlayers : Random.Seed -> List String -> (List Player, Random.Seed)
initialPlayers seed names = 
  let (targets, seed') = RndArr.shuffle seed allTreasures
      targetsPerPlayer = Array.length targets // List.length names
      targetsFor idx   = Array.slice (idx * targetsPerPlayer) ((idx + 1) * targetsPerPlayer) targets
      mkPlayer name col home idx = { color = col, home = home
        , position = home, name = name, targets = Array.toList (targetsFor idx) }
  in (List.map4 mkPlayer names playerColors playerHomes [0..3], seed')

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
        |> mapAccumL rotateRandom seed'
      -- | Get coordinates of loose pieces (coordinates that are not in the fixedPieces dict)
      loosePieceCoords = (,) `List.map` [0..6] `ap` [0..6]
        |> List.filter (not << flip Dict.member fixedPieces)
      -- | Associate loose pieces with coordinates
      movablePieces = Dict.fromList <| List.map2 (,) loosePieceCoords randomLst
  in ({ width = 7, height = 7
     , pieces = Dict.union fixedPieces movablePieces
     , freePiece = freePiece
     , players = players }, seed'')

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
  |> List.filterMap (\p -> p.treasure)
  |> Array.fromList

-- * Game rules (size and player agnostic)

-- | On a board side of a given length, return the movable indices.
movableIndices : Int -> List Int
movableIndices len = List.map (\p -> p*2 + 1) [0..len // 2 - 1]

movableRows : Board -> List Int
movableRows board = movableIndices board.height

movableCols : Board -> List Int
movableCols board = movableIndices board.width

-- | Next players turn.
nextPlayer : Board -> Board
nextPlayer board = let (p::ps) = board.players
  in { board | players <- ps ++ [p] }

-- | The current player.
currentPlayer : Board -> Player
currentPlayer board = List.head board.players

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

-- | Checks if the current player is currently above the target
foundTarget : Board -> Bool
foundTarget board = case Dict.get (currentPlayer board).position board.pieces of
  Just p -> p.treasure == Just (List.head (currentPlayer board).targets)
  Nothing -> False

shift : Shift -> Board -> Board
shift shiftCmd board = 
  let shiftCoords = if isHorizontal shiftCmd.side 
        then List.map (\i -> (i, shiftCmd.index)) [0..board.width-1]
        else List.map (\i -> (shiftCmd.index, i)) [0..board.width-1]
      shiftDir = opposite shiftCmd.side
      -- Partition pieces by which get shifted and which stay in place
      shifted (x,y) _ = if isHorizontal shiftCmd.side
        then y == shiftCmd.index else x == shiftCmd.index
      (removed,rest) = Dict.partition shifted board.pieces
      -- Insert shifted
      newBoard = newBoard --List.foldl (\pos -> Dict.insert (pos `to` shiftDir)) rest
  in shift shiftCmd board



-- * Interactivity

-- | Channel for the buttons to shift a piece in
shiftChannel : Signal.Channel Shift
shiftChannel = Signal.channel { side = North, index = -1 }

testGame = fst (newGame (Random.initialSeed 42) ["John", "Max", "Hinz"])
  |> moveTo South

main : Signal GEl.Element
main = (\shift -> gameTable testGame) <~ Signal.subscribe shiftChannel


-- * Utility functions

roundedRect : Float -> Float -> Float -> (GCol.Shape -> GCol.Form) -> GCol.Form
roundedRect w h radius style = GCol.group
  [ GCol.circle radius |> style |> GCol.move ( w / 2 - radius,  h / 2 - radius)
  , GCol.circle radius |> style |> GCol.move (-w / 2 + radius,  h / 2 - radius)
  , GCol.circle radius |> style |> GCol.move ( w / 2 - radius, -h / 2 + radius)
  , GCol.circle radius |> style |> GCol.move (-w / 2 + radius, -h / 2 + radius)
  , GCol.rect (w - 2 * radius) h |> style
  , GCol.rect w (h - 2 * radius) |> style
  ]

maybeToList : Maybe a -> List a
maybeToList m = case m of
  Just x -> [x]
  Nothing -> []

ap : List (a -> b) -> List a -> List b
ap fs xs = List.concatMap (flip List.map xs) fs

mapAccumL : (a -> acc -> (b, acc)) -> acc -> List a -> (List b, acc)
mapAccumL f acc xxs = case xxs of
  [] -> ([], acc)
  (x::xs) -> 
    let (y, acc') = f x acc
        (ys, acc'') = mapAccumL f acc' xs
    in (y::ys, acc'')

singleton : a -> List a
singleton x = [x]

last : List a -> a
last = List.reverse >> List.head

init : List a -> List a
init xs = case xs of 
  [] -> List.tail []
  [x] -> []
  (x::xs) -> x :: init xs

-- | When you know there must be an element.
getUnsafe : comparable -> Dict.Dict comparable v -> v
getUnsafe key = Dict.get key >> maybeToList >> List.head
