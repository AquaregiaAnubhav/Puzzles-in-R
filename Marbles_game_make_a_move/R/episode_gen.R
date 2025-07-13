source("R/game_env.R")

gen_episode <- function(env) {
  # this will run a full random gameplay
  transitions <- list() # we will collect game states as rows here
  state<- env$reset() # initial state of game
  
  repeat{
    legal <-which(env$valid_moves()) #get indices for valid moves
    if(length(legal)==0) break #finish if no valid move left
    action <- sample(legal, 1) # taking a random step to execute
    result <- env$step(action) #executing the move
    
    #storing the transitions
    transitions[[length(transitions)+1]] <- data.frame(
      State = state,
      Action = action,
      Reward = result$reward,
      NextState = result$state,
      stringsAsFactors = FALSE
    )
    
    state <- result$state #moving to the next state
    
    if (result$done) break #exiting the game if DONE
  }
  do.call(rbind, transitions)
}