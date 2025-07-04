generate_n_mat <- function(n){
  # n is the number of matrices you want to generate
  # So we will have 16*n elements in them

  samp <- sample(1:15, 16*n, replace=TRUE)
  # We generate 16n elements and then we will form n matrices from these
  matrices <- vector("list", n)

  for (i in seq_along(matrices)){
    matrices[[i]] <- matrix(samp[(16*(i-1) + 1) : (16*i)], ncol=4, nrow=4)
  }
  return(matrices)
}

average_det <- function(matrices){ #Takes the average of determinants of n random matrices
  print(mean(sapply(matrices, det)))
  # return(sapply(matrices, det))
}

n <- 1000000 #larger n, better approximation
#Note that the probability value gets closer and closer with higher value of n, but it is a very slow convergence
time <-system.time({k<-average_det(generate_n_mat(n)) #Approximation of the expected determinant value of the random matrix
})
print(time) #To measure the time taken for the code to run

# ########################################################################
# #ChatGPT's improved(memory wise) version of my code but slower than mine
# ########################################################################
#
# #This is cleaner and better for memory as it doesn't store
# #all the matrices, it immediately generate and calculate
# #the det of the random matrix and doesn't store the matrix
# #but only the det value.
#
# estimate_expected_det <- function(n) {
#   dets <- numeric(n) # preallocate vector of determinants
# 
#   for (i in seq_len(n)) {
#     mat <- matrix(sample(1:15, 16, replace=TRUE), 4, 4)
#     dets[i] <- det(mat)
#   }
# 
#   mean(dets)
# }
# n <- 10000
# time <- system.time({print(estimate_expected_det(n))})
# print(time)
# ###########################################################


