matrix_vals <- c(0, 0.2, 0.5, 0.3, 0.45, 0.2, 0.1, 0.25, 0, 1, 0, 0, 0, 0, 0, 1);

A <- matrix(matrix_vals, byrow = T, ncol = 4, nrow = 4)

# for(i in 1:nrow(A))
# {
#   for(j in 1:ncol(A))
#   {
#     if(A[i, j] > 0 && A[j, i] > 0)
#     {
#       cat("State", i, "and state", j, "communicate\n")
# 
#     }
#     else if(A[i, j] > 0)
#     {
#       cat("State", j, "is accessible from state", i, "\n")
#     }
# 
#   }
# }



numberOfSteps <- matrix(0, ncol = 5, nrow = 4)


for(i in 1:nrow(A))
{
  An = A
  n = 1
  j = 1
  found = 0
  
  while(found < 5)
  {
    if(An[i, i] > 0)
    {
      numberOfSteps[i, j] = n
      found = found + 1
      j = j + 1
    }
    n = n + 1
    (An <-  An %*% A)
  }
  
}

# calculate GCD
library(FRACTION)
for(i in 1:nrow(numberOfSteps))
{
  cat("Period of state", i, "is", gcd(numberOfSteps[1], gcd(numberOfSteps[2], gcd(numberOfSteps[3], gcd(numberOfSteps[4], numberOfSteps[5])))), "\n")
}
