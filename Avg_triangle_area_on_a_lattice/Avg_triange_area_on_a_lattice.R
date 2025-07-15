#Calculates euclidean distance
distance <- function(point_a,point_b) sqrt((point_a[1]-point_b[1])^2 + (point_a[2]-point_b[2])^2)



# Calculates area by Heron's formula and returns the square of the area, 
# because the question asks to find average of the square of area!
sqr_of_area_by_heron <- function(p,q,r) {
  
  # Finding the side lengths and the semi-perimeter
  a <- distance(p,q) ; b <- distance(q,r); c <- distance(r,p); s <- (a+b+c)/2;
  
  return(s*(s-a)*(s-b)*(s-c))
}


expected_sqr_of_area <- function(n){
  sums<- 0 # To keep track of the running sum
  for (i in 1:n){
    #generating points in the 5x5 lattice uniformly
    point_1 <- sample(c(0,1,2,3,4), 2, replace = TRUE)
    point_2 <- sample(c(0,1,2,3,4), 2, replace = TRUE)
    point_3 <- sample(c(0,1,2,3,4), 2, replace = TRUE)
    area_sq <- sqr_of_area_by_heron(point_1, point_2, point_3)
    sums <- sums + area_sq
  }
  return(sums/n) 
}

# print(sqr_of_expected_area(1000000))# Wait for 10-20 seconds for it to run.


################################################################################
### ChatGPT_o3 code ############################################################
################################################################################

## Much faster than mine ~30x-50x, uses Shoelace formula which I didn't know about!

expected_sqr_area_vec <- function(n = 1e6) {
  # 6 integers per trial → n × 6 matrix
  pts <- matrix(sample(0:4, 6 * n, replace = TRUE), nrow = n)
  
  x1 <- pts[, 1];  y1 <- pts[, 2]
  x2 <- pts[, 3];  y2 <- pts[, 4]
  x3 <- pts[, 5];  y3 <- pts[, 6]
  
  cross <- (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)
  mean(cross^2) / 4            # ⟨A²⟩ = ⟨cross²⟩ / 4
}
################################################################################
