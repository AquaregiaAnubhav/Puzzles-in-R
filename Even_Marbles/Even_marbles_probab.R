k <- 8 # Size of the board
n <- 100000 # number of iteration you want to estimate on, larger the better.

#Strategy to find the solution

# Since the value is very small and not a trivial value like 0, or 1, etc,
# you will not be able to find the solution just by plotting the distribution,
# and eyeing out the mean of it. You would need to solve for smaller board,
# starting from k=2 and so on, until you see a pattern. 

# I have also implemented the formula I got based on observing the pattern.

generate_n_mat <- function(n, k){
  # n is the number of matrices you want to generate
  # So we will have (k^2)*n elements in them
  #we have 0 as white and 1 as black

  samp <- sample(0:1, k*k*n, replace=TRUE)
  # We generate nk^2 elements and then we will form n matrices from these
  matrices <- vector("list", n)

  for (i in seq_along(matrices)){
    matrices[[i]] <- matrix(samp[(k*k*(i-1) + 1) : (k*k*i)], ncol=k, nrow=k)
  }
  return(matrices)
}

evaluate_even_matrix <- function(matrix){
  if ((sum(colSums(matrix)%%2) == 0) && (sum(rowSums(matrix)%%2) == 0)){
    return(1)
  }
  else {
    return(0)
  }
}

even_sum_probab<- function(n,k){
  mean(sapply(generate_n_mat(n,k),evaluate_even_matrix))
}

# ######################################################################################
# #ChatGPT's implementation for the problem
# ######################################################################################
# #Following is my exact prompt to generate the following Chatgpt code with o4-mini-high
# #"Think as hard and possible & find the fastest way to implement the same problem in R.
# #Above is my implementation, which may or maynot be the fastest implementation.
# #The problem is: You have a kxk board and each cell of it has either a black marble or
# #white marble. what is the probability that in each row and in each column, the number
# #of white marbles is even"
#
# #Guess what, my code is almost twice as fast!
# ######################################################################################
# even_sum_probab <- function(n, k) {
#   # create one big 3D array of 0/1’s: dimensions k × k × n
#   A <- array(sample.int(2, k*k*n, TRUE) - 1L, dim = c(k, k, n))
# 
#   # compute parity (mod 2) of each row in each matrix: result k × n
#   row_parity <- apply(A, c(2, 3), sum) %% 2L
#   # compute parity of each column: result k × n
#   col_parity <- apply(A, c(1, 3), sum) %% 2L
# 
#   # for each of the n trials, check “all zero” in both row_parity and col_parity
#   ok <- colSums(row_parity) == 0L & colSums(col_parity) == 0L
# 
#   mean(ok)
# }
# ######################################################################################




# ########################################################
# # Formula based implementation 
# # (Look at it only if you want to spoil the solution)
# ########################################################
# even_sum_probab <- function(k){
#   2^(2*k-1)
# }
# ########################################################

