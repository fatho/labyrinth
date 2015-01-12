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
import Graphics.Input.Field as GInp
import Html
import Html.Attributes as Html
import Maybe (Maybe (..), andThen)
import Maybe
import Mouse
import Keyboard
import Set
import Signal
import Signal ((<~),(~), Signal)
import String
import Random
import Random.Array as RndArr
-- Workaround, same as above
import Text
import Text (defaultStyle)
import Time
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

type Orientation = North | East | South | West

type AngularDir = CW | CCW

-- | Defines possible user interactions.
type UserEvent
  = StartGame Time.Time
  | UpdatePlayerNames (List GInp.Content)
  | RotateFreePiece AngularDir
  | ShiftRow Shift
  | MoveToken Orientation
  | EndTurn
  | RestartWithSamePlayers Time.Time
  | RestartWithNewPlayers

-- | Defines possible application outputs.
type Scene
  = AskPlayerNames (List GInp.Content)
  | InGame TurnState Board
  | GameOver (List Player)

type TurnState
  = Shifting
  | Moving

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

-- * Piece rendering

-- | One third of the piece size.
smallSize = 30
-- | Size of one labyrinth piece.
pieceSize = 3 * smallSize
-- | Size of the labyrinth
boardRealSize board = { width = board.width * pieceSize, height = board.height * pieceSize }

-- | Currently, treasures are rendered as text
renderTreasure : Treasure -> GCol.Form
renderTreasure t = Text.fromString t
    |> Text.style { defaultStyle | color <- Color.lightGrey, bold <- True }
    |> Text.centered |> GCol.toForm

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
  in GCol.group <|
    [ wall, way ] ++ (wayFromCoords <| List.filterMap keepIfOpen sides) 
    ++ [outline]

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
shiftButtonArrow : Orientation -> Color.Color -> GEl.Element
shiftButtonArrow side color = 
  let shape = GCol.ngon 3 smallSize
  in GCol.group [ shape |> GCol.filled color, shape |> GCol.outlined defaultLine ]
    |> GCol.rotate (degrees 270)
    |> GCol.rotate -(orientationAngle side)
    |> singleton |> GCol.collage pieceSize pieceSize

-- | Renders the board.
renderBoard : Board -> GCol.Form
renderBoard board = 
  let b2c = boardToCanvas board
      -- render ground of cell
      renderGroundAt pos = case Dict.get pos board.pieces of
        Nothing -> []
        Just piece -> [renderPiece piece |> GCol.move (b2c pos)]
      -- render treasure of cell
      renderTreasureAt pos = case Dict.get pos board.pieces of
        Nothing -> []
        Just piece -> piece.treasure 
          `andThen` (\tr -> if Set.member tr board.collectedTreasures then Nothing else Just tr)
            |> Maybe.map renderTreasure 
            |> Maybe.map (GCol.move (b2c pos))
            |> maybeToList
      -- rendered pieces
      pieces = (,) `List.map` [0..board.width-1] `ap` [0..board.height-1]
        |> List.concatMap renderGroundAt
      -- rendered treasures
      treasures = (,) `List.map` [0..board.width-1] `ap` [0..board.height-1]
        |> List.concatMap renderTreasureAt
      -- player bases
      homes = List.map (\p -> renderHome p.color |> GCol.move (b2c p.home)) board.players
      -- player tokens
      tokens = List.map (\p -> playerToken p.color |> GCol.move (b2c p.position)) (List.reverse board.players)
  in GCol.group <| pieces ++ homes ++ tokens ++ treasures

-- * Interface rendering

-- CSS to make element unselectable
unselectable : Html.Attribute
unselectable = Html.style
  [ ("-webkit-touch-callout", "none")
  , ("-webkit-user-select", "none")
  , ("-khtml-user-select", "none")
  , ("-moz-user-select", "none")
  , ("-ms-user-select", "none")
  , ("user-select", "none")
  ]

-- | Wraps an element in a div tag which prevents text selections.
makeUnselectable : GEl.Element -> GEl.Element
makeUnselectable el =
  let (w,h) = GEl.sizeOf el
  in Html.div [ unselectable ] [ Html.fromElement el ] |> Html.toElement w h

colorIndicator : Color.Color -> GEl.Element -> GEl.Element
colorIndicator color el = 
  let h = snd (GEl.sizeOf el)
  in GCol.collage 20 h [GCol.rect 10 (toFloat h * 0.9) |> GCol.filled color]
    `GEl.beside` el

-- | Displays the player list next to the game board.
viewPlayerList : List Player -> GEl.Element
viewPlayerList players = 
  let displayCurPlayer p = GEl.flow GEl.down 
        [ Text.leftAligned <| Text.fromString <| p.name ++ " (" ++ toString (List.length p.targets) ++ " remaining)" 
        , case p.targets of
            [] -> Text.plainText "All targets reached, return to base!"
            (x::_) -> Text.fromString "Next target is: " ++ Text.color Color.white (Text.fromString x)
              |> Text.leftAligned
        ] |> colorIndicator p.color
      displayNextPlayer i p = toString i ++ ". " ++ p.name ++ " (" ++ toString (List.length p.targets) ++ " remaining)"
        |> Text.plainText |> colorIndicator p.color
      headingCur = Text.centered <| Text.bold <| Text.fromString "Current Player:"
      headingNext = Text.centered <| Text.bold <| Text.fromString "Next Players:"
      nextPlayers = GEl.flow GEl.down <| List.map2 displayNextPlayer [2..4] (List.tail players)
  in GEl.flow GEl.down 
    [ headingCur
    , displayCurPlayer (List.head players)
    , headingNext
    , nextPlayers
    ]

-- | Displays the free piece and buttons to rotate it.
viewFreePiece : Piece -> GEl.Element
viewFreePiece piece = 
  let rotateCCWButton = shiftButtonArrow East Color.blue
        |> GInp.clickable (Signal.send freeRotateChannel CCW)
      rotateCWButton = shiftButtonArrow West Color.blue
        |> GInp.clickable (Signal.send freeRotateChannel CW)
      controls = GEl.flow GEl.right 
        [ rotateCCWButton
        , GCol.collage pieceSize pieceSize 
            [ renderPiece piece
            , Maybe.map renderTreasure piece.treasure |> maybeToList |> GCol.group]
        , rotateCWButton ]
  in controls

-- | Display in-game view.
viewInGame : TurnState -> Board -> GEl.Element
viewInGame turnState board = 
  let -- where should the shift buttons be places
      shiftButtonSpec = 
        [ (North, movableCols board )
        , (South, movableCols board)
        , (West, movableRows board)
        , (East, movableRows board) ]
      -- calculates the position of a button
      buttonPos side idx = case side of
        West -> boardToCanvas board (-1, idx)
        East -> boardToCanvas board (board.width, idx)
        North -> boardToCanvas board (idx, -1)
        South -> boardToCanvas board (idx, board.height)
      -- creates a clickable button
      mkButton side idx = 
        ( if Just { side = opposite side, index = idx } == board.lastShift
            then shiftButtonArrow side Color.grey
            else shiftButtonArrow side Color.yellow
             |> GInp.clickable (Signal.send shiftChannel { side = side, index = idx} )
        ) |> GCol.toForm
          |> GCol.move (buttonPos side idx)
      -- creates all buttons
      shiftButtons = List.concatMap (\(s,is) -> List.map (mkButton s) is) shiftButtonSpec
      -- size in pixels of the board
      bsize = boardRealSize board

      turnStateInfo = case turnState of
        Shifting -> Text.plainText "Please shift a row or column. You may rotate the free piece with the buttons above."
        Moving -> Text.plainText "Move your token with the arrow keys. Press 'Enter' when you want to finish your move."
      -- game board
      boardElement = [renderBoard board] ++ shiftButtons
        |> GCol.collage (bsize.width+pieceSize*2) (bsize.height+pieceSize*2)

      infoElement = GEl.flow GEl.down
        [ GEl.spacer 0 100
        , Text.fromString "Players"
            |> Text.bold
            |> Text.height 20
            |> Text.leftAligned
        , viewPlayerList board.players
        , GEl.spacer 0 100
        , Text.fromString "Piece to shift in"
            |> Text.bold
            |> Text.height 20
            |> Text.leftAligned
        , viewFreePiece board.freePiece
        , GEl.spacer 0 100
        , Text.fromString "Instructions"
            |> Text.bold
            |> Text.height 20
            |> Text.leftAligned
        , turnStateInfo
        ]

      content = makeUnselectable boardElement `GEl.beside` infoElement
    in content

-- | Shows the startup screen asking for player names.
viewAskPlayerNames : List GInp.Content -> GEl.Element
viewAskPlayerNames contents = 
  let mkField idx chan content = GInp.field GInp.defaultStyle (Signal.send chan) ("Player " ++ toString idx) content
        `GEl.above` GEl.spacer 0 20

      heading = (Text.fromString "Participating Players" |> Text.height 30 |> Text.centered)
        `GEl.above` GEl.spacer 0 20

      containerWidth = max (fst <| GEl.sizeOf fields) (fst <| GEl.sizeOf heading)
      containerHeight = snd (GEl.sizeOf fields) + 20

      fields = GEl.flow GEl.down (List.map3 mkField [1..4] playerNameChannels contents )

      startButton = GInp.button (Signal.send startGameChannel ()) "Start Game"
  in GEl.container 1200 810 GEl.middle
    <| heading 
      `GEl.above` GEl.container containerWidth containerHeight GEl.middle fields
      `GEl.above` GEl.container containerWidth (snd (GEl.sizeOf startButton) + 2) GEl.middle startButton 

viewGameOver : List Player -> GEl.Element
viewGameOver (first::rest) = 
  let displayEntry n player = toString n ++ ". " ++ player.name 
          |> Text.fromString |> Text.height 20 |> Text.leftAligned
          |> colorIndicator player.color 
      displayRanked n cur ps = displayEntry n cur :: 
        case ps of
          [] -> [ ]
          (p::pps) -> if rank p == rank cur 
            then displayRanked n p pps
            else displayRanked (n+1) p pps

      heading = (Text.fromString "Game Over!" |> Text.height 40 |> Text.centered)

      rankedList = GEl.flow GEl.down (displayRanked 1 first rest)

      buttons = GEl.flow GEl.right
        [ GInp.button (Signal.send restartSameChannel ()) "Restart with same players"
        , GEl.spacer 20 20
        , GInp.button (Signal.send restartNewChannel ()) "Restart with new players" ]

      containerWidth = List.maximum [fst <| GEl.sizeOf heading, fst <| GEl.sizeOf rankedList, fst <| GEl.sizeOf buttons ]
      headingHeight = snd (GEl.sizeOf heading) + 20
      listHeight = snd (GEl.sizeOf rankedList) + 20
      buttonHeight = snd (GEl.sizeOf buttons) + 20

  in GEl.container 1200 810 GEl.middle
    <| GEl.container containerWidth headingHeight GEl.middle heading
        `GEl.above` GEl.container containerWidth listHeight GEl.middle rankedList
        `GEl.above` GEl.container containerWidth buttonHeight GEl.middle buttons

viewMain : Scene -> GEl.Element
viewMain scene = case scene of 
  AskPlayerNames ps -> viewAskPlayerNames ps
  InGame turnState board -> viewInGame turnState board
  GameOver winner -> viewGameOver winner

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
        |> mapAccumL rotateRandom seed'
      -- | Get coordinates of loose pieces (coordinates that are not in the fixedPieces dict)
      loosePieceCoords = (,) `List.map` [0..6] `ap` [0..6]
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
  |> List.filterMap (\p -> p.treasure)
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
        then dict else Dict.insert (pos `to` shiftDir) (unsafeGet pos removed) dict
      newBoard = List.foldl doShift rest shiftCoords -- insert shifted pieces
        |> Dict.insert shiftedIn board.freePiece -- insert new piece
      -- piece that has fallen out
      newFree = unsafeGet shiftedOut removed

      relocateToken player = if shifted player.position
        then { player | position <- if player.position == shiftedOut then shiftedIn else player.position `to` shiftDir }
        else player
      -- TODO: Take care of player figures
  in { board | pieces <- newBoard, freePiece <- newFree, players <- List.map relocateToken board.players, lastShift <- Just shiftCmd }



-- * Interactivity

-- | Channel for the buttons to shift a piece in
shiftChannel : Signal.Channel Shift
shiftChannel = Signal.channel { side = North, index = -1 }

-- | Channel for the buttons to rotate the free piece
freeRotateChannel : Signal.Channel AngularDir
freeRotateChannel = Signal.channel CW

-- | Receiving events from the "start game" button
startGameChannel : Signal.Channel ()
startGameChannel = Signal.channel ()

restartSameChannel : Signal.Channel ()
restartSameChannel = Signal.channel ()

restartNewChannel : Signal.Channel ()
restartNewChannel = Signal.channel ()

-- | Receiving events from the player name widget
playerNameChannels : List (Signal.Channel GInp.Content)
playerNameChannels = List.map (\f -> Signal.channel GInp.noContent) playerColors

moveCommands : Signal Orientation
moveCommands = Signal.mergeMany
  [ always North <~ keyPresses keyArrowUp
  , always South <~ keyPresses keyArrowDown
  , always West  <~ keyPresses keyArrowLeft
  , always East  <~ keyPresses keyArrowRight
  ]

-- | Subscribe a list of channels and get a signal of lists.
subscribeMany : List (Signal.Channel a) -> Signal (List a)
subscribeMany chs = case chs of
  [] -> Signal.constant []
  (x::xs) -> (::) <~ Signal.subscribe x ~ subscribeMany xs

-- | Collect all user events
collectEvents : Signal UserEvent
collectEvents = Signal.mergeMany
  [ (StartGame << fst) <~ Time.timestamp (Signal.subscribe startGameChannel)
  , UpdatePlayerNames <~ subscribeMany playerNameChannels
  , RotateFreePiece <~ Signal.subscribe freeRotateChannel
  , ShiftRow <~ Signal.subscribe shiftChannel
  , MoveToken <~ moveCommands
  , always EndTurn <~ keyPresses keyEnter
  , always RestartWithNewPlayers <~ Signal.subscribe restartNewChannel
  , (RestartWithSamePlayers << fst) <~ Time.timestamp (Signal.subscribe restartSameChannel)
  ]

-- | Central state handling function.
stepGame : UserEvent -> Scene -> Scene
stepGame event scene = case scene of
  AskPlayerNames ps -> case event of
    UpdatePlayerNames ps' -> AskPlayerNames ps'
    StartGame t -> 
      let players = List.filterMap (\c -> if String.length c.string > 0 then Just c.string else Nothing) ps
      in if List.length players >= 2 
        then newGame (Time.inMilliseconds t |> floor |> Random.initialSeed) players
          |> fst |> InGame Shifting
        else AskPlayerNames ps
    _ -> scene
  InGame turnState board -> case turnState of
    Shifting -> case event of
      RotateFreePiece dir -> InGame Shifting (rotateFreePiece dir board)
      ShiftRow cmd -> InGame Moving (shift cmd board)
      _ -> scene
    Moving -> case event of
      MoveToken dir -> InGame Moving (moveTo dir board)
      EndTurn -> 
        let afterTargetCheck = processTarget board
        in if hasWon (currentPlayer afterTargetCheck) 
          then GameOver (List.sortBy rank afterTargetCheck.players)
          else InGame Shifting (nextPlayer afterTargetCheck)
        -- TODO: Check targets, check winning condition
      _ -> scene
  GameOver players -> case event of
    RestartWithSamePlayers time -> newGame 
      (Time.inMilliseconds time |> floor |> Random.initialSeed) 
      (List.map (\p -> p.name) players) |> fst |> InGame Shifting
    RestartWithNewPlayers -> initialState 
    _ -> GameOver players

initialState : Scene
initialState = AskPlayerNames (List.repeat 4 GInp.noContent)

testGameOverState : Scene
testGameOverState = GameOver <| List.sortBy rank
  [ { name = "Max Muster", color = Color.blue, targets = [], home = (6,6), position = (0,0) }
  , { name = "Hinz", color = Color.green, targets = ["Djinn"], home = (0,6), position = (0,6) }
  , { name = "John Doe", color = Color.red, targets = [], home = (0,0), position = (0,0) }
  ]

-- | Takes user events and produces a scene to display.
runGame : Signal UserEvent -> Signal Scene
runGame = Signal.foldp stepGame initialState

-- | Glue everything together.
main : Signal GEl.Element
main = viewMain <~ runGame collectEvents


-- * Utility functions

keyArrowUp = 38 
keyArrowDown = 40
keyArrowLeft = 37
keyArrowRight = 39
keyEnter = 13

keyPresses : Keyboard.KeyCode -> Signal ()
keyPresses key = always () <~ Signal.keepIf identity False (Keyboard.isDown key)

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
unsafeGet : comparable -> Dict.Dict comparable v -> v
unsafeGet key = Dict.get key >> maybeToList >> List.head
