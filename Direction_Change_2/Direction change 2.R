#"1" for upward and "0" for a right-ward step

tot_paths <- function(grid_size){ # Calculates the total number of paths possible
  return(factorial(2*grid_size)/factorial(grid_size)^2)
}

increment_binary_n <- function(n,binary){ # gives you the next n-digit binary number
  stopifnot( is.numeric(binary), length(binary) == n, all(binary %in% 0:1))
  carry <- 1L
  for (i in n:1){
    s <- binary[i] + carry
    
    binary[i] <- s%%2
    carry <- s%/%2
    
    if (carry == 0) break
  }
  return(binary)
}

odd_path_probab <- function(n){ # Calculates the probability of having a path with odd number of direction changes
  #Works by manually counting all the possible paths and counting the number of those
  #with odd number of direction changes
  count <- 0
  path <- rep(c(0,1), each = n)
  stop_val <- increment_binary_n(2*n, rep(c(1,0), each = n))
  while (!identical(path, stop_val)){
    if (sum(path) == n){ # this considers only those with equal number of up and right steps (valid paths)
      digit <- NaN
      dir_change_count <- 0
      for (val in path) { #Iterates over the paths individually to count the number of direction changes
        if (is.nan(digit)) digit = val
        if (val != digit) {
          dir_change_count <- dir_change_count + 1
          digit <- val
        }
      }
      if (dir_change_count %% 2 == 1) count = count + 1 #counts the number of odd paths
    }
    path = increment_binary_n(2*n,path)
  }
  return(count/tot_paths(n)) #this is the probability of the odd paths.
}

######################################################
# #Faster implementation from ChatGPT (Try if you wish)
######################################################
# odd_path_probab <- function(n){ 
#   # total number of valid paths
#   total <- tot_paths(n)
#   
#   # 1) generate all combinations of 'n' ones in 2n positions
#   combs <- combn(2*n, n)
#   m     <- ncol(combs)
#   
#   # 2) build a 2n × m matrix of 0/1 paths
#   paths <- matrix(0L, nrow = 2*n, ncol = m)
#   # for each column j, set rows combs[,j] to 1
#   paths[cbind(as.vector(combs), rep(seq_len(m), each = n))] <- 1L
#   
#   # 3) count direction‐changes = number of times bit flips 0↔1
#   #    diff(paths) is (2n−1)×m, so (diff != 0) is a logical matrix
#   changes <- colSums(diff(paths) != 0L)
#   
#   # 4) how many have an odd number of changes?
#   odd_ct <- sum(changes %% 2L == 1L)
#   
#   odd_ct / total
# }


############################################################
# #Look at this only if you theoretically solved the problem
############################################################
# odd_path_probab <- function(n){n/(2*n-1)} 
# #This is the general closed form expression of the answer 
# #for nxn grid size
############################################################

n <- 8 #The grid size
print(odd_path_probab(n))