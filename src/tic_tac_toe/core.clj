;; #!/usr/bin/env bb
;; (require '[clojure.java.shell :refer [sh]])
(ns tic-tac-toe.core
  (:gen-class))

;; (identity startingBoard)



;; (clearConsole)

(defn clearConsole []
  (println "\033[H\033[2J")
  (flush))
(defn endMessage [] 
  (println "Thanks for playing, have a great day :)"))
(defn getAllEmptyCellIndexes
  "Returns a list of cells without a player marker"
  [currentBoardState]
  (mapv identity (filter #(integer? %) currentBoardState)))
;; (getAllEmptyCellIndexes startingBoard)

(defn checkIfWinnerFoundHorizontally [currentBoardState currentMark]
  "Returns true if the mark is a winner in the horizontal direction otherwise false"
  (cond
    (and (= (nth currentBoardState 0) currentMark) (= (nth currentBoardState 1) currentMark) (= (nth currentBoardState 2) currentMark)) true
    (and (= (nth currentBoardState 3) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 5) currentMark)) true
    (and (= (nth currentBoardState 6) currentMark) (= (nth currentBoardState 7) currentMark) (= (nth currentBoardState 8) currentMark)) true
    :else false))
;; (checkIfWinnerFoundHorizontally startingBoard "X")

(defn checkIfWinnerFoundVertically [currentBoardState currentMark]
  "Returns true if the mark is a winner in the vertical direction otherwise false"
  (cond
    (and (= (nth currentBoardState 0) currentMark) (= (nth currentBoardState 3) currentMark) (= (nth currentBoardState 6) currentMark)) true
    (and (= (nth currentBoardState 1) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 7) currentMark)) true
    (and (= (nth currentBoardState 2) currentMark) (= (nth currentBoardState 5) currentMark) (= (nth currentBoardState 8) currentMark)) true
    :else false))
;; (checkIfWinnerFoundVertically startingBoard "O")

(defn checkIfWinnerFoundDiagonally [currentBoardState currentMark]
  "Returns true if the mark is a winner in the diagonal direction otherwise false"
  (cond
    (and (= (nth currentBoardState 0) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 8) currentMark)) true
    (and (= (nth currentBoardState 2) currentMark) (= (nth currentBoardState 4) currentMark) (= (nth currentBoardState 6) currentMark)) true
    :else false))
;; (checkIfWinnerFoundDiagonally startingBoard "X")

(defn checkIfWinnerFound [currentBoardState currentMark]
  "Return true if the mark is a winner in any direction false otherwise"
  (cond
    (checkIfWinnerFoundHorizontally currentBoardState currentMark) true
    (checkIfWinnerFoundVertically currentBoardState currentMark) true
    (checkIfWinnerFoundDiagonally currentBoardState currentMark) true
    :else false))
;; (checkIfWinnerFound startingBoard "X")

;; (defn minimax [currentBoardState currentMark])

(defn printBoard [currentBoardState playersPiece]
  "Loops through the provided board and prints it to the console"
  (clearConsole)
  (println "Your piece is" playersPiece)
   (loop [y 0] (when (< y (count currentBoardState))
                 (print (nth currentBoardState y) " ")
                 (if (= 0 (mod (+ y 1) 3))
                   (println ""))
                 (recur (+ y 1)))))
;; (printBoard startingBoard)

(defn checkIfTie [currentBoardState]
  "Return true if board is full and there is no winner false otherwise"
  (cond
    (checkIfWinnerFound currentBoardState "X") false
    (checkIfWinnerFound currentBoardState "O") false
    (empty? (getAllEmptyCellIndexes currentBoardState)) true
    :else false))
;; (checkIfTie startingBoard)


(defn nextBestMove [currentBoardState]
  "Returns the index of the next best move provided a board state"
  (nth (getAllEmptyCellIndexes currentBoardState) (rand-int (count (getAllEmptyCellIndexes currentBoardState)))))
;; (nextBestMove startingBoard)

(defn placeNextMove [currentBoardState moveIndex playerMarker]
  "Returns a new board state with the provided move played"
  (assoc currentBoardState moveIndex playerMarker))
;; (placeNextMove startingBoard 2 "X")

(defn isValidPiece? [piece]
  (cond
    (= (clojure.string/upper-case piece) "X") true
    (= (clojure.string/upper-case piece) "O") true
    :else false))
;; (isValidPiece? "X")
;; (isValidPiece? 4)

(defn grabPlayersPiece []
  (println "Which piece would you like to play? [X,O]")
  (let [playerPiece (read-line)]
    (if (isValidPiece? playerPiece)
      (clojure.string/upper-case playerPiece)
      (recur))))
;; (grabPlayersPiece)


(defn coinToss []
  (let [coin (rand-int 2)]
    coin))
;; (coinToss)

(defn playerStarts? []
  (if (= (coinToss) 0)
    true
    false))
;; (playerStarts?)

(defn contains-value? [element coll]
  (boolean (some #(= element %) coll)))
;; (contains-value? 3 (getAllEmptyCellIndexes startingBoard))

(defn playerMoveValid? [move board]
  (try
    (contains-value? (Integer/parseInt move) (getAllEmptyCellIndexes board))
    (catch Exception ex false)))
;; (playerMoveValid? "8" startingBoard)

(defn getPlayersNextMove [board]
  (let [playerMove (read-line)]
    (if (playerMoveValid? playerMove board)
      (Integer/parseInt playerMove)
      (recur board))))
;; (getPlayersNextMove startingBoard)

(defn printMessageIfTieAndReturnTrue [board playersPiece]
  (let [boardIsTie (checkIfTie board)]
    (if boardIsTie
      (printBoard board playersPiece))
    (if boardIsTie
      (println "Game ended in a tie. Better luck next time."))
    boardIsTie))
;; (printMessageIfTieAndReturnTrue startingBoard)

(defn wouldLikeToPlayAgain? []
  (println "Would you like to play again? (y/n)")
  (def playAgain (read-line))
  (if (= (clojure.string/upper-case playAgain) "Y")
    true
    false))
;; (wouldLikeToPlayAgain?)

(defn changePlayer [player]
  (if (= player "X")
    "O"
    "X"))
;; (changePlayer "X")
;; (changePlayer "Y")



(defn writeBoardToFile [path board]
  (spit path
        board))
(writeBoardToFile "/workspace/Tic_Tac_Toe/boardState.txt" (vector 0 1 2 3 4 5 6 7 8))

(defn readFileToBoard [path]
  (let [board (slurp path)]
    (identity (clojure.edn/read-string board))))

;; (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt")

(defn writeCurrentPlayerToFile [path currentPlayer]
  (spit path
        currentPlayer))
;; (writeCurrentPlayerToFile "/workspace/Tic_Tac_Toe/currentPlayer.txt" "Y")

(defn readCurrentPlayerFromFile [path]
  (identity (slurp path)))
;; (readCurrentPlayerFromFile "/workspace/Tic_Tac_Toe/currentPlayer.txt")

(defn deleteFile [path]
  (try
    (clojure.java.io/delete-file path)
    true
    (catch Exception ex false)))


(defn ai_plays [board playerPiece]
  (placeNextMove board (nextBestMove board) (changePlayer playerPiece)))
;; (ai_plays (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt") "X")
;; (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt")
;; (ai_plays startingBoard "X")

(defn getStartingPlayer [playerPiece]
  (if (playerStarts?)
    playerPiece
    (changePlayer playerPiece)))
(getStartingPlayer "X")
(defn printWinnerMessageAndReturnTrue [board player]
  (let [boardIsWinner (checkIfWinnerFound board player)]
    (if boardIsWinner
      (printBoard board player))
    (if boardIsWinner
      (println "Congragulations" player "You've won! :)"))
    boardIsWinner))
;; (printWinnerMessageAndReturnTrue startingBoard "X")

(defn printLoserMessageAndReturnTrue [board aiPiece]
  (let [boardIsWinner (checkIfWinnerFound board aiPiece)]
    (if boardIsWinner
      (printBoard board (changePlayer aiPiece)))
    (if boardIsWinner
      (println "Sorry" (changePlayer aiPiece) "Looks like you've lost. Better Luck Next Time :)"))
    boardIsWinner))
(printLoserMessageAndReturnTrue (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt") "X")
(defn deleteTemporaryFiles []
  (deleteFile "/workspace/Tic_Tac_Toe/boardState.txt")
  (deleteFile "/workspace/Tic_Tac_Toe/currentPlayer.txt"))
(defn runGame []
  (deleteTemporaryFiles)
  (def playersPiece (grabPlayersPiece))
  (def startingPlayer (getStartingPlayer playersPiece))
  (def startingBoard (vector 0 1 2 3 4 5 6 7 8))
  (writeCurrentPlayerToFile "/workspace/Tic_Tac_Toe/currentPlayer.txt" startingPlayer)
  (writeBoardToFile "/workspace/Tic_Tac_Toe/boardState.txt" startingBoard)
  (loop [maxPlays 9]
    (when (and  (< 0 maxPlays) (not (checkIfWinnerFound (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt") playersPiece)) (not (checkIfWinnerFound (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt") (changePlayer playersPiece))) (not (checkIfTie (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt"))))
      (let [currentPlayer (readCurrentPlayerFromFile "/workspace/Tic_Tac_Toe/currentPlayer.txt")
            playingBoard (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt")]
    ;; (clearConsole)
        (printBoard playingBoard playersPiece)
        (cond
          (= currentPlayer playersPiece) (writeBoardToFile "/workspace/Tic_Tac_Toe/boardState.txt" (placeNextMove playingBoard (getPlayersNextMove playingBoard) playersPiece)) 
          (not (= currentPlayer playersPiece)) (writeBoardToFile "/workspace/Tic_Tac_Toe/boardState.txt" (ai_plays playingBoard playersPiece)))
        (writeCurrentPlayerToFile "/workspace/Tic_Tac_Toe/currentPlayer.txt" (changePlayer currentPlayer))
        (recur (dec maxPlays)))))
  (cond
    (printMessageIfTieAndReturnTrue (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt") playersPiece) true
    (printWinnerMessageAndReturnTrue (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt") playersPiece) true
    (printLoserMessageAndReturnTrue (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt") (changePlayer playersPiece)) true
    :else false))


;; (checkIfTie (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt"))

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
;; (def playersPiece (grabPlayersPiece))

;; (readFileToBoard "/workspace/Tic_Tac_Toe/boardState.txt")






;; (doc checkIfWinnerFound)



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [playAgain 1]
    (when (< 0 playAgain)
      (clearConsole)
      (runGame)
      (if (wouldLikeToPlayAgain?)
        (recur (identity playAgain))
        (recur (dec playAgain)))))
  (endMessage)
  (deleteTemporaryFiles))

;; (-main)