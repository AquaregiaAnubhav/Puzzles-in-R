gcd <- function(a, b) {
  a <- abs(a); b <- abs(b)
  while (b != 0) {
    tmp <- b
    b <- a %% b
    a <- tmp
  }
  a
}

#Lcm for two numbers
lcm <- function(a, b) abs(a * b) / gcd(a, b)

#LCM for a whole vector
lcm_vec <- function(x) Reduce(lcm, x)

#Rolling the 10-sided dice
dice_roll <- function(n) sample(c(1,2,3,4,5,6,7,8,9,10),n, replace = TRUE)

n_iter <- function(n){
  iterations <- 0
  n_count_sum <- 0
  while (iterations < n) {
    count <- 3
    lcm_val <- lcm_vec(dice_roll(3))
    while (lcm_val <= 2000) {
      lcm_val <- lcm(lcm_val,dice_roll(1))
      count <- count + 1
    }
    iterations <- iterations + 1
    n_count_sum <- n_count_sum + count
  }

return(n_count_sum/iterations)
}

system.time({print(n_iter(100000))})

############################################################################
## Chatgpt-o3 code for same problem (slower than mine for n ~ 1e5 or larger)
############################################################################

# ## --- helpers -------------------------------------------------------------
# gcd2 <- function(a, b) {              # Euclidean algorithm, integer only
#   while (b) {                         # slightly faster than "b != 0"
#     tmp <- b;  b <- a %% b;  a <- tmp
#   }
#   a
# }
# 
# lcm2 <- function(a, b) a / gcd2(a, b) * b   # divide first → avoid overflow
# 
# ## --- simulate ONE experiment --------------------------------------------
# sim_one <- function(threshold = 2000L) {
#   lcm_val <- 1L            # we’ll fold the first 3 rolls into one loop
#   count   <- 0L
#   
#   ## 1) take the first three rolls
#   for (i in 1:3L) {
#     d <- sample.int(10L, 1L)          # slightly faster than sample()
#     lcm_val <- lcm2(lcm_val, d)
#     count   <- count + 1L
#   }
#   
#   ## 2) keep rolling until the LCM gets big enough
#   while (lcm_val <= threshold) {
#     d <- sample.int(10L, 1L)
#     lcm_val <- lcm2(lcm_val, d)
#     count   <- count + 1L
#   }
#   count
# }
# 
# ## --- run N experiments ---------------------------------------------------
# n_iter_fast <- function(n, threshold = 2000L) {
#   mean(replicate(n, sim_one(threshold), simplify = TRUE))
# }
# ############################################################################
# 
