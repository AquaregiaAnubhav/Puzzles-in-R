source ("R/game_env.R")
library(data.table)
library(magrittr)

# Initializing Q-table

actions <- as.character(1:4)
# Q <- data.table(State =character(0), Action = character(0), Value =numeric(0))
Q1 <- data.table(State=character(), Action=character(), Value=1.0)
Q2 <- data.table(State=character(), Action=character(), Value=1.0)

get_Q1 <- function(s,a) {
  v <- Q1[State==s & Action==a, Value]
  if (length(v)==0) return(1.0) else return(v)
}
get_Q2 <- function(s,a) {
  v <- Q2[State==s & Action==a, Value]
  if (length(v)==0) return(1.0) else return(v)
}
update_Q1 <- function(s,a,val) {
  if (nrow(Q1[State==s & Action==a])==0) {
    Q1 <<- rbind(Q1, data.table(State=s,Action=a,Value=val))
  } else {
    Q1[State==s & Action==a, Value:=val]
  }
}
update_Q2 <- function(s,a,val) {
  if (nrow(Q2[State==s & Action==a])==0) {
    Q2 <<- rbind(Q2, data.table(State=s,Action=a,Value=val))
  } else {
    Q2[State==s & Action==a, Value:=val]
  }
}
# get_Q <- function(state, action){
#   # returns 0 if unseen
#   qt <- Q[State == state & Action == action, Value]
#   if (length(qt)==0) return(0)
#   qt
# }
# 
# update_Q <- function(state, action, new_value) {
#   key <- Q[State == state & Action == action]
#   if (nrow(key) == 0) {
#     Q <<- rbind(Q, data.table(State = state, Action = action, Value = new_value))
#   } else {
#     Q[State == state & Action == action, Value := new_value]
#   }
# }

#Self‐play training loop
alpha   <- 0.01
gamma   <- 0.999
epsilon <- 1.0
n_episodes <- 10000

#Self-play + Double Q-learning
for (ep in seq_len(n_episodes)) {
  env   <- GameEnv$new()
  state <- env$reset()
  repeat {
    moves <- env$valid_moves()
    legal <- which(moves)
    
    #both players ε-greedy on Q1+Q2
    if (runif(1) < epsilon) {
      a <- sample(legal, 1)
    } else {
      # sum Q1+Q2 for each legal action
      qsum <- sapply(as.character(legal),
                     function(act) get_Q1(state,act) + get_Q2(state,act))
      a <- legal[which.max(qsum)]
    }
    
    res        <- env$step(a)
    next_state <- res$state
    
    #update only on player-1 moves (env$player flips to -1)
    if (env$player == -1) {
      #coin flip: decide whether to update Q1 or Q2
      if (runif(1) < 0.5) {
        old    <- get_Q1(state, as.character(a))
        # find best next under Q1
        nmoves <- env$valid_moves(); legal2 <- which(nmoves)
        if (length(legal2)) {
          q1n   <- sapply(as.character(legal2),
                          function(act) get_Q1(next_state,act))
          best2 <- legal2[which.max(q1n)]
          target<- res$reward + gamma * get_Q2(next_state, as.character(best2))
        } else {
          target <- res$reward
        }
        newval <- old + alpha * (target - old)
        update_Q1(state, as.character(a), newval)
        
      } else {
        old    <- get_Q2(state, as.character(a))
        nmoves <- env$valid_moves(); legal2 <- which(nmoves)
        if (length(legal2)) {
          q2n   <- sapply(as.character(legal2),
                          function(act) get_Q2(next_state,act))
          best2 <- legal2[which.max(q2n)]
          target<- res$reward + gamma * get_Q1(next_state, as.character(best2))
        } else {
          target <- res$reward
        }
        newval <- old + alpha * (target - old)
        update_Q2(state, as.character(a), newval)
      }
    }
    
    state <- next_state
    if (res$done) break
  }
  
  #decay exploration
  epsilon <- max(0.05, epsilon * 0.999)
}

#Building the final greedy policy from Q1+Q2
combined <- merge(
  Q1[, .(State, Action, V1=Value)],
  Q2[, .(State, Action, V2=Value)],
  by=c("State","Action"), all=TRUE
)
combined[is.na(V1), V1 := 1.0]
combined[is.na(V2), V2 := 1.0]
combined[, Combined := V1 + V2]
policy_dt <- combined[, .SD[which.max(Combined)], by=State, .SDcols=c("State","Action")]
setnames(policy_dt, "Action", "BestAction")

#Persist and report
# dir.create("data", showWarnings=FALSE)
saveRDS(Q1,        "data/Q1_selfplay_double.rds")
saveRDS(Q2,        "data/Q2_selfplay_double.rds")
saveRDS(policy_dt, "data/policy_selfplay_double.rds")

cat(sprintf(
  "Done: %d episodes, %d unique states\n",
  n_episodes, nrow(policy_dt)
), "\n")

# for (ep in seq_len(n_episodes)) {
#   env   <- GameEnv$new()
#   state <- env$reset()
#   
#   repeat {
#     #Choose action for current player (always train player 1 against random B)
#     legal <- which(env$valid_moves())
#     if (env$player == 1) {
#       if (runif(1) < epsilon) {
#         a <- sample(legal, 1)
#       } else {
#         # both players pick argmax-Q
#         qvals <- sapply(as.character(legal), function(act) get_Q(state, act))
#         a     <- legal[which.max(qvals)]
#         # pick best Q among legal actions
#         # qs <- sapply(as.character(legal), function(act) get_Q(state, act))
#         # best_act <- legal[which.max(qs)]
#         # a <- best_act
#       }
#     } else {
#       # opponent B is random
#       a <- sample(legal, 1)
#     }
#     
#     #Step and observe
#     res <- env$step(a)
#     
#     # Q‐value update (only update for player 1 moves)
#     if (env$player == -1) { 
#       # just moved player 1 → update
#       old_q <- get_Q(state, as.character(a))
#       # estimate of next state's best value
#       future_vals <- sapply(as.character(which(env$valid_moves())), 
#                             function(act) get_Q(res$state, act))
#       best_next <- if (length(future_vals)) max(future_vals) else 0
#       target <- res$reward + gamma * best_next
#       new_q  <- old_q + alpha * (target - old_q)
#       update_Q(state, as.character(a), new_q)
#     }
#     
#     state <- res$state
#     if (res$done) break
#   }
#   
#   #Decay epsilon
#   # epsilon <- max(0.01, epsilon * 0.999)
#   epsilon <- 1 - (ep / n_episodes) * 0.95    # from 1.0 to 0.05 over training
#   epsilon <- max(epsilon, 0.05)
# }
# 
# # Derive greedy policy from Q‐table
# policy <- Q[
#   , .(Action = Action[which.max(Value)]),
#   by = State
# ] %>% setNames(c("State","BestAction"))
# 
# saveRDS(Q, "data/tabular_Q_selfplay.rds")
# saveRDS(policy, "data/tabular_policy_selfplay.rds")
# 
# cat("Trained on", n_episodes, "self-play episodes.\n")
# cat("Unique states seen:", length(unique(Q$State)), "\n")
# head(policy)