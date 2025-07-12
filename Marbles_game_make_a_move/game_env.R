library(R6)

GameEnv <- R6Class(
  "GameEnv",
  public = list(
    # Fields
    pos = NULL, #integer from 1 to 25, indicating the position of the marble
    mask = NULL, #logical vector for ruling out visited squares
    player = NULL,
    
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
      #returns logical vector of length 4,
      logical(4)
    },
    
    
    # Applying an action (1 for U, 2 for D, 3 for L and 4 for R)
    step = function(action){
      #returns list (state, reward, done)
      list(
        state = self$get_state(),
        reward = 0,
        done = FALSE
      )
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















