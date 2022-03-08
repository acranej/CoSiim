#' @name cosine_similarity
#' @description determines the cosine similarity for two matrices
#' @param W1 matrix
#' @param W2 matrix
#' @return matrix of cosine similarity between dimensions of input matrices
#' @export
cosine_similarity = function(W1,W2) {
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
