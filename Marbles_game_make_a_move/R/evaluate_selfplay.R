# R/evaluate_selfplay.R

library(data.table)
source("R/game_env.R")

# 1) Load your two Q-tables
Q1 <- readRDS("data/Q1_selfplay_double.rds")
Q2 <- readRDS("data/Q2_selfplay_double.rds")

# 2) Merge them just like in training to get Combined = V1 + V2
combined <- merge(
  Q1[, .(State, Action, V1 = Value)],
  Q2[, .(State, Action, V2 = Value)],
  by = c("State","Action"), all = TRUE
)
# Fill in any missing entries with the optimistic init value 1.0
combined[is.na(V1), V1 := 1.0]
combined[is.na(V2), V2 := 1.0]
combined[, Combined := V1 + V2]

# 3) Extract greedy policy: for each State pick the Action with max(Combined)
policy_dt <- combined[
  , .SD[which.max(Combined)],   # .SD is the slice of rows for each State
  by = State,
  .SDcols = c("State","Action")
]
setnames(policy_dt, "Action", "BestAction")

# 4) Turn that into a named character vector
policy_vec <- setNames(as.character(policy_dt$BestAction),
                       policy_dt$State)

# 5) Evaluation function (falls back to random on unseen states)
evaluate_policy <- function(policy, n_games = 200) {
  wins <- 0L
  for (i in seq_len(n_games)) {
    env   <- GameEnv$new()
    state <- env$reset()
    repeat {
      moves <- env$valid_moves()
      legal <- which(moves)
      if (env$player == 1) {
        act_char <- policy[state]
        if (is.na(act_char)) {
          action <- sample(legal, 1)
        } else {
          action <- as.integer(act_char)
        }
      } else {
        action <- sample(legal, 1)
      }
      res   <- env$step(action)
      state <- res$state
      if (res$done) {
        if (env$player == -1) wins <- wins + 1L
        break
      }
    }
  }
  wins / n_games
}

# 6) Run and print
win_rate <- evaluate_policy(policy_vec, n_games = 2000)
cat(sprintf("Win rate vs random: %.1f%%\n", win_rate * 100), "\n")
