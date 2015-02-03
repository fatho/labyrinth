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
import Keyboard.Keys as Keys
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

import Game (..)
import Util

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
  | ShiftAnimation Time.Time

-- | Defines possible application outputs.
type Scene
  = AskPlayerNames (List GInp.Content)
  | InGame TurnState Board
  | GameOver (List Player)

type TurnState
  = WaitForShift
  | Shifting Shift Float
  | Moving

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
renderPiece : Bool -> Piece -> GCol.Form
renderPiece movable blk = 
  let wall = GCol.square pieceSize 
        |> if movable 
            then GCol.filled Color.darkBrown
            else GCol.filled (Color.rgb 120 68 33)
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
    |> Util.singleton |> GCol.collage pieceSize pieceSize

rotateButtonArrow : AngularDir -> Bool -> GEl.Element
rotateButtonArrow angularDir enabled =
  let dirStr = toString angularDir
      enabledStr = if enabled then "enabled" else "disabled"
      imgSrc = "rotate" ++ dirStr ++ enabledStr ++ ".svg"
  in GEl.image pieceSize pieceSize imgSrc

-- | Renders the board.
renderBoard : Maybe (Shift, Float) -> Board -> GCol.Form
renderBoard shiftProgress board = 
  let b2c = boardToCanvas board
      -- render ground of cell
      renderGroundAt pos = case Dict.get pos board.pieces of
        Nothing -> []
        Just piece -> [renderPiece (cellMovable pos) piece 
          |> GCol.move (b2c pos) |> shiftAnim pos ]
      -- render treasure of cell
      renderTreasureAt pos = case Dict.get pos board.pieces of
        Nothing -> []
        Just piece -> piece.treasure 
          `andThen` (\tr -> if Set.member tr board.collectedTreasures then Nothing else Just tr)
            |> Maybe.map renderTreasure 
            |> Maybe.map (GCol.move (b2c pos) >> shiftAnim pos)
            |> Util.maybeToList

      freePieceAnimated = GCol.group (renderPiece True board.freePiece 
            :: Util.maybeToList (Maybe.map renderTreasure board.freePiece.treasure)
          ) |> GCol.move (b2c freePiecePos)
            |> shiftAnim freePiecePos

      freePiecePos = case shiftCmd.side of
        South -> (shiftCmd.index, board.height)
        North -> (shiftCmd.index, -1)
        East -> (board.width, shiftCmd.index)
        West -> (-1, shiftCmd.index)

      -- Check if a coordinate is part of the shifting animation
      isShifted (x,y) = (isHorizontal shiftCmd.side && shiftCmd.index == y)
        || (isVertical shiftCmd.side && shiftCmd.index == x)
      shiftOff = case shiftCmd.side of
        North -> (0, -shiftFactor * pieceSize)
        South -> (0,  shiftFactor * pieceSize)
        East -> (-shiftFactor * pieceSize, 0)
        West -> (shiftFactor * pieceSize, 0)

      shiftAnim pos = GCol.move <| if isShifted pos then shiftOff else (0,0)
      shiftCmd = Maybe.withDefault { side = North, index = -1 } <| Maybe.map fst shiftProgress
      shiftFactor = Maybe.withDefault 0 <| Maybe.map snd shiftProgress

      isShiftAnimation = shiftProgress /= Nothing
      -- rendered pieces
      pieces = (if isShiftAnimation then [freePieceAnimated] else [])
        ++ ( Util.cartProd [0..board.width-1] [0..board.height-1]
              |> List.concatMap renderGroundAt )
      -- rendered treasures
      treasures = Util.cartProd [0..board.width-1] [0..board.height-1]
        |> List.concatMap renderTreasureAt
      -- player bases
      homes = List.map (\p -> renderHome p.color |> GCol.move (b2c p.home)) board.players
      -- player tokens
      tokens = List.map (\p -> playerToken p.color |> GCol.move (b2c p.position) |> shiftAnim p.position) (List.reverse board.players)
  in GCol.group <| pieces ++ homes ++ tokens ++ treasures

-- * Interface rendering

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
            (x::_) -> GEl.flow GEl.right
              [ Text.fromString "Next target is: " |> Text.leftAligned
              , Util.withClass "hover_disclose" (Text.fromString x |> Text.leftAligned)
              ]
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
viewFreePiece : TurnState -> Piece -> GEl.Element
viewFreePiece turnState piece = 
  let rotateButton dir = if isShiftState
        then rotateButtonArrow dir True
          |> GInp.clickable (Signal.send freeRotateChannel dir)
        else rotateButtonArrow dir False

      isShiftState = turnState == WaitForShift
      isShifting = case turnState of
        Shifting _ _ -> True
        _ -> False

      controls = GEl.flow GEl.right 
        [ rotateButton CCW
        , if isShifting 
            then GEl.spacer pieceSize pieceSize
            else GCol.collage pieceSize pieceSize 
                  [ renderPiece True piece
                  , Maybe.map renderTreasure piece.treasure 
                    |> Util.maybeToList |> GCol.group ]
        , rotateButton CW ]
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
        ( if Just { side = opposite side, index = idx } == board.lastShift || turnState /= WaitForShift
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
        WaitForShift -> Text.plainText
          <| "Your current target is displayed in white and can be\n"
          ++ "revealed by hovering that part of the page with the cursor.\n"
          ++ "Make sure the other players don't see it.\n"
          ++ "Now shift a row or column.\n"
          ++ "You may rotate the free piece with the buttons above."
        Shifting _ _ -> Text.plainText "Shifting..."
        Moving -> Text.plainText 
          <| "Move your token with the arrow keys.\n" 
          ++ "Press 'Enter' when you want to finish your move."
      -- game board
      shiftProgress = case turnState of
        Shifting cmd p -> Just (cmd, p)
        _ -> Nothing
      boardElement = [renderBoard shiftProgress board] ++ shiftButtons
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
        , viewFreePiece turnState board.freePiece
        , GEl.spacer 0 100
        , Text.fromString "Instructions"
            |> Text.bold
            |> Text.height 20
            |> Text.leftAligned
        , turnStateInfo
        ]

      content = Util.makeUnselectable boardElement `GEl.beside` infoElement
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

      startButton =
        if List.length (extractPlayerNames contents) >= 2
          then GInp.button (Signal.send startGameChannel ()) "Start Game"
          else GEl.spacer 0 40
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
  -- NOTE: Keys.arrowUp.keyCode is not accepted
  [ always North <~ Util.keyPresses (.keyCode Keys.arrowUp) 
  , always South <~ Util.keyPresses (.keyCode Keys.arrowDown)

   -- NOTE: the library swapped left and right
  , always West  <~ Util.keyPresses (.keyCode Keys.arrowRight)
  , always East  <~ Util.keyPresses (.keyCode Keys.arrowLeft)
  ]

-- | Collect all user events
collectEvents : Signal UserEvent
collectEvents = Signal.mergeMany
  [ (StartGame << fst) <~ Time.timestamp (Signal.subscribe startGameChannel)
  , UpdatePlayerNames <~ Util.subscribeMany playerNameChannels
  , RotateFreePiece <~ Signal.subscribe freeRotateChannel
  , ShiftRow <~ Signal.subscribe shiftChannel
  , MoveToken <~ moveCommands
  , always EndTurn <~ Util.keyPresses (.keyCode Keys.enter)
  , always RestartWithNewPlayers <~ Signal.subscribe restartNewChannel
  , (RestartWithSamePlayers << fst) <~ Time.timestamp (Signal.subscribe restartSameChannel)
  , ShiftAnimation <~ Time.fps 30
  ]

-- | Transforms a list of text field contents to a list of player names ignoring empty fields.
extractPlayerNames : List GInp.Content -> List String
extractPlayerNames = List.filterMap (\c -> if String.length c.string > 0 then Just c.string else Nothing)

-- | Central state handling function.
stepGame : UserEvent -> Scene -> Scene
stepGame event scene = case scene of
  AskPlayerNames ps -> case event of
    UpdatePlayerNames ps' -> AskPlayerNames ps'
    StartGame t -> 
      let players = extractPlayerNames ps
      in if List.length players >= 2 
        then newGame (Time.inMilliseconds t |> floor |> Random.initialSeed) players
          |> fst |> InGame WaitForShift
        else AskPlayerNames ps
    _ -> scene
  InGame turnState board -> case turnState of
    WaitForShift -> case event of
      RotateFreePiece dir -> InGame WaitForShift (rotateFreePiece dir board)
      ShiftRow cmd -> InGame (Shifting cmd 0) board
        --
      _ -> scene
    Shifting cmd progress -> case event of
      ShiftAnimation delta -> let progress' = progress + Time.inSeconds delta
        in if progress' >= 1
            then InGame Moving (shift cmd board)
            else InGame (Shifting cmd progress') board
    Moving -> case event of
      MoveToken dir -> InGame Moving (moveTo dir board)
      EndTurn -> 
        let afterTargetCheck = processTarget board
        in if hasWon (currentPlayer afterTargetCheck) 
          then GameOver (List.sortBy rank afterTargetCheck.players)
          else InGame WaitForShift (nextPlayer afterTargetCheck)
        -- TODO: Check targets, check winning condition
      _ -> scene
  GameOver players -> case event of
    RestartWithSamePlayers time -> newGame 
      (Time.inMilliseconds time |> floor |> Random.initialSeed) 
      (List.map .name players) |> fst |> InGame WaitForShift
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
  >> Signal.dropRepeats

-- | Glue everything together.
main : Signal GEl.Element
main = viewMain <~ runGame collectEvents
