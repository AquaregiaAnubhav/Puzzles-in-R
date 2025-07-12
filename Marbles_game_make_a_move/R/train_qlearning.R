library(ReinforcementLearning)
source("game_env.R")
source("episode_gen.R")

# Generate random-play episodes
n_episodes <- 1000
episodes_list <- lapply(seq_len(n_episodes), function(i){
  gen_episode(GameEnv$new())
})
episodes_df <- do.call(rbind,episodes_list)

episodes_df$State <- as.character(episodes_df$State)
episodes_df$Action <- as.character(episodes_df$Action)
episodes_df$Reward <- as.numeric(episodes_df$Reward)
episodes_df$NextState <- as.character(episodes_df$NextState)

# inspect the episodes

cat("Total transitions:", nrow(episodes_df), "\n")
print(table(episodes_df$Reward))

#Setting parameters for Q-learning
control <- list(alpha = 0.1, gamma =0.9, epsilon =0.1)

#learning q-table
model <- ReinforcementLearning( data = episodes_df,
                                s = "State", a = "Action",
                                r = "Reward", 
                                s_new = "NextState",
                                control = control)

# check the policy and the q-values

cat("Sample Policy:\n")
print(head(model$Policy))

cat("Sample Q=values:\n")
print(head(model$Q))

#Evaluating policy against randon opponent

evaluate_policy <- function(policy, n_games = 200){
  wins <- 0
  for (i in seq_len(n_games)){
    env <- GameEnv$new()
    state <- env$reset()
    repeat{
      moves <- env$valid_moves()
      if (env$player == 1){
        action_char <- policy[state]
        if (is.na(action_char)) {
          # fallback: pick a random legal move
          legal <- which(moves)
          action <- sample(legal, 1)
        } else {
          action <- as.integer(action_char)
        }
      }else{
        legal <- which(env$valid_moves())
        action <- sample(legal, 1)
      }
      res <- env$step(action)
      state <- res$state
      if (res$done){
        if (env$player == -1) wins <- wins+1
        break
      }
    }
  }
  win_rate <- wins/n_games
  invisible(win_rate)
}

win_rate <- evaluate_policy(model$Policy, n_games= 200)

cat(sprintf("Win rate vs random : %.1f%%\n", win_rate * 100))