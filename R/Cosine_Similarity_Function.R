### cosine similarity function
#*
#* Input is 2 matrices with dimensions (A x B) (A x C)
#* One side of each matrix must have the same length as the other
#* Returns a matrix of cosine similarity values between the two input matrices
#*
plot.W.correlation <- function(W1,W2) {
  K1 <- ncol(W1)
  K2 <- ncol(W2)
  x <- array(0,dim=c(K1,K2))
  for (i in 1:K1) {
    for (j in 1:K2) {
      x[i,j] <- W1[,i]%*%W2[,j]/sqrt(sum(W1[,i]^2))/sqrt(sum(W2[,j]^2))
    }
  }
  rownames(x) <- colnames(W1)
  colnames(x) <- colnames(W2)
  return(x)
}
