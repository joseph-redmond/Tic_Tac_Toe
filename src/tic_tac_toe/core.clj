(ns tic-tac-toe.core
  (:gen-class))


(def startingBoard (vector 0 1 2 "X" 4 5 6 7 8))
(defn getAllEmptyCellIndexes
  "Returns a list of cells without a player marker"
  [currentBoardState]
  (mapv identity (filter #(integer? %) currentBoardState)))
(getAllEmptyCellIndexes startingBoard)

(defn checkIfWinnerFoundHorizontally [currentBoardState currentMark]
  "Returns true if the mark is a winner in the horizontal direction otherwise false"
  (cond
    (and (= (nth currentBoardState 0) currentMark) (= (nth currentBoardState 1) currentMark) (= (nth currentBoardState 2) currentMark)) true
    (and (= (nth currentBoardState 3) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 5) currentMark)) true
    (and (= (nth currentBoardState 6) currentMark) (= (nth currentBoardState 7) currentMark) (= (nth currentBoardState 8) currentMark)) true
    :else false))
(checkIfWinnerFoundHorizontally startingBoard "X")

(defn checkIfWinnerFoundVertically [currentBoardState currentMark]
  "Returns true if the mark is a winner in the vertical direction otherwise false"
  (cond
    (and (= (nth currentBoardState 0) currentMark) (= (nth currentBoardState 3) currentMark) (= (nth currentBoardState 6) currentMark)) true
    (and (= (nth currentBoardState 1) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 7) currentMark)) true
    (and (= (nth currentBoardState 2) currentMark) (= (nth currentBoardState 5) currentMark) (= (nth currentBoardState 8) currentMark)) true
    :else false))
(checkIfWinnerFoundVertically startingBoard "O")

(defn checkIfWinnerFoundDiagonally [currentBoardState currentMark]
  "Returns true if the mark is a winner in the diagonal direction otherwise false"
  (cond
    (and (= (nth currentBoardState 0) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 8) currentMark)) true
    (and (= (nth currentBoardState 2) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 6) currentMark)) true
    :else false))
(checkIfWinnerFoundDiagonally startingBoard "X")

(defn checkIfWinnerFound [currentBoardState currentMark]
  "Return true if the mark is a winner in any direction false otherwise"
  (cond
    (checkIfWinnerFoundHorizontally currentBoardState currentMark) true
    (checkIfWinnerFoundVertically currentBoardState currentMark) true
    (checkIfWinnerFoundDiagonally currentBoardState currentMark) true
    :else false))
(checkIfWinnerFound startingBoard "X")

(defn minimax [currentBoardState currentMark])

(defn printBoard [currentBoardState]
  "Loops through the provided board and prints it to the console"
  (loop [y 0] (when (< y (count currentBoardState))
                (print (nth currentBoardState y) " ")
                (if (= 0 (mod (+ y 1) 3))
                  (println ""))
                (recur (+ y 1)))))
(printBoard startingBoard)

(defn checkIfTie [currentBoardState]
  "Return true if board is full and there is no winner false otherwise"
  (cond
    (checkIfWinnerFound startingBoard "X") false
    (checkIfWinnerFound startingBoard "O") false
    (empty? (getAllEmptyCellIndexes currentBoardState)) true
    :else false))
(checkIfTie startingBoard)


(defn nextBestMove [currentBoardState]
  "Returns the index of the next best move provided a board state"
  (nth (getAllEmptyCellIndexes currentBoardState) (rand-int (count (getAllEmptyCellIndexes currentBoardState)))))
(nextBestMove startingBoard)

(defn placeNextMove [currentBoardState moveIndex playerMarker]
  "Returns a new board state with the provided move played"
  (assoc currentBoardState moveIndex playerMarker))
(placeNextMove startingBoard 2 "X")

(defn isValidPiece? [piece]
  (cond
    (= (clojure.string/upper-case piece) "X") true
    (= (clojure.string/upper-case piece) "O") true
    :else false))
(isValidPiece? "X")
(isValidPiece? 4)

(defn grabPlayersPiece []
  (println "Which piece would you like to play? [X,O]")
  (let [playerPiece (read-line)]
    (if (isValidPiece? playerPiece)
      playerPiece
      (recur))))
(grabPlayersPiece)


(defn coinToss []
  (let [coin (rand-int 2)]
    coin))
(coinToss)

(defn playerStarts? []
  (if (= (coinToss) 0)
    true
    false))
(playerStarts?)

(defn contains-value? [element coll]
  (boolean (some #(= element %) coll)))
(contains-value? 4 (getAllEmptyCellIndexes startingBoard))

(defn playerMoveValid? [move board]
  (try
    (contains-value? (Integer/parseInt move) (getAllEmptyCellIndexes board))
    (catch Exception ex false)))
(playerMoveValid? "8" startingBoard)

(defn getPlayersNextMove [board]
  (let [playerMove (read-line)]
    (if (playerMoveValid? playerMove board)
      (Integer/parseInt playerMove)
      (recur board))))
(getPlayersNextMove startingBoard)

(defn printMessageIfTieAndReturnTrue [board]
  (let [boardIsTie (checkIfTie board)]
    (if boardIsTie
      (println "Game ended in a tie. Better luck next time."))
    boardIsTie))
(printMessageIfTieAndReturnTrue startingBoard)

(defn wouldLikeToPlayAgain? []
  (println "Would you like to play again? (y/n)")
  (def playAgain (read-line))
  (if (= (clojure.string/upper-case playAgain) "Y")
    true
    false))
(wouldLikeToPlayAgain?)

(defn changePlayer [player]
  (if (= player "X")
    "O"
    "X"))
(changePlayer "X")
(changePlayer "Y")

(defn printWinnerMessageAndReturnTrue [board player]
  (let [boardIsWinner (checkIfWinnerFound board player)]
    (if boardIsWinner
      (println "Congragulations " player " You've won! :)"))
    boardIsWinner))
(printWinnerMessageAndReturnTrue startingBoard "X")


(defn writeBoardToFile [board path]
  (spit path
        board))
(writeBoardToFile startingBoard "C:/Users/me/Desktop/boardState.txt")

(defn readFileToBoard [path]
  (let [board (slurp path)]
    (identity (clojure.edn/read-string board))))

(readFileToBoard "C:/Users/me/Desktop/boardState.txt")


(defn ai_plays [board playerPiece]
  (placeNextMove board (nextBestMove board) (changePlayer playerPiece)))
(ai_plays (readFileToBoard "C:/Users/me/Desktop/boardState.txt") "X")
(readFileToBoard "C:/Users/me/Desktop/boardState.txt")
(ai_plays startingBoard "X")

(defn getStartingPlayer [playerPiece]
  (if (playerStarts?)
    playerPiece
    (changePlayer playerPiece)))
(getStartingPlayer "X")

(defn runGame [startingPlayer playersPiece]
  (def board (identity startingBoard))
  (loop [playingBoard board
         playerIdentity (identity startingPlayer)
         maxPlays 9]
    (when (> 0 maxPlays)
    (printBoard board)
    (if (= playerIdentity startingPlayer)
      (writeBoardToFile (placeNextMove playingBoard (getPlayersNextMove playingBoard) playersPiece) "C:/Users/me/Desktop/boardState.txt")
      (writeBoardToFile (ai_plays playingBoard playersPiece) "C:/Users/me/Desktop/boardState.txt"))
    (recur (readFileToBoard "C:/Users/me/Desktop/boardState.txt") (changePlayer playerIdentity) (dec maxPlays)))))


  ;; (while (not-empty (getAllEmptyCellIndexes playingBoard))
  ;;   (printBoard playingBoard)
  ;;   (def playingBoard (placeNextMove playingBoard (if (= currentPlayer playerPiece)
  ;;                                                   (getPlayersNextMove playingBoard)
  ;;                                                   (nextBestMove playingBoard)) currentPlayer))
  ;;   (def currentPlayer (changePlayer currentPlayer)))
  ;; (if (printMessageIfTieAndReturnTrue playingBoard)
  ;;   false
  ;;   (printWinnerMessageAndReturnTrue playingBoard currentPlayer))

  ;; (if (wouldLikeToPlayAgain?)
  ;;   (def playingBoard (identity board))
  ;;   (println "Thank you for playing :)")))
(def playersPiece (grabPlayersPiece))
(runGame (getStartingPlayer playersPiece) playersPiece)

(readFileToBoard "C:/Users/me/Desktop/boardState.txt")






(doc checkIfWinnerFound)



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
