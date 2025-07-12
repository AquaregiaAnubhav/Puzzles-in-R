library(R6)

GameEnv <- R6Class(
  "GameEnv",
  public = list(
    # Fields
    pos = NULL, #integer from 1 to 25, indicating the position of the marble
    mask = NULL, #logical vector for ruling out visited squares
    player = NULL, #+1 for A and -1 for B
    
    # Initialise/reset part
    reset = function(){
      self$pos <- 1L
      self$mask <- rep(FALSE, 25)
      self$mask[1] <- TRUE
      self$player <- 1L
      invisible(self$get_state())
    },
    
    # State encoding as a string key
    get_state = function(){
      paste0(
        paste0(as.integer(self$mask), collapse = ""),
        "_", self$pos,
        "_", self$player
      )
    },
    
    # Legal moves, in order - U,D,L and R
    valid_moves = function(){
      row <- ceiling(self$pos/5)
      col <- ((self$pos - 1)%%5) + 1
      
      up <- (row  > 1) && !self$mask[self$pos -5]
      down <- (row < 5) && !self$mask[self$pos + 5]
      left <- (col > 1) && !self$mask[self$pos - 1]
      right <- (col < 5) && !self$mask[self$pos + 1]
      
      #returns logical vector of length 4,
      c(up, down, left, right)
    },
    
    
    # Applying an action (1 for U, 2 for D, 3 for L and 4 for R)
    step = function(action){
      #returns list (state, reward, done)
      
      moves <- self$valid_moves() # checks for valid move
      
      # if invalid move then punish the RL agent
      if (!moves[action]){
        return( list ( state = self$get_state(), reward = -10, done = TRUE))
      }
      
      new_pos <- switch(as.character(action),
                        "1" = self$pos - 5,
                        "2" = self$pos + 5,
                        "3" = self$pos - 1,
                        "4" = self$pos + 1,
      )
      
      # moving the marble
      self$pos <- new_pos
      self$mask[new_pos] <- TRUE
      
      #checking if opponent has a valid move after this
      #to decide loss or victory
      next_moves_available <- any(self$valid_moves())
      
      #Rewards
      reward <- if (!next_moves_available) +1 else 0
      done <- !next_moves_available
      
      self$player <- - self$player #giving turn to other player
      
      list( state = self$get_state(), reward= reward, done = done)
    },
    
    
    #Simple ASCII render of the 5x5 grid
    render = function(){
      grid <- matrix("_|", nrow=5, ncol=5)
      row <- ceiling(self$pos / 5)
      col <- ((self$pos-1) %% 5) + 1
      grid[row, col] <- "M|"
      
      # Marking visited cells on the grid
      visited <- which(self$mask)
      for (p in visited){
        if (p != self$pos){
          r <- ceiling(p/5); c <- ((p-1) %% 5) + 1
          grid[r,c] <- "X|"
        }
      }
      cat(apply(grid, 1, paste, collapse = " "), sep="\n")
      invisible(grid)
    }
  )
)















