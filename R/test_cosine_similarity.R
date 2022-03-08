#' @name test_cosine_similarity
#' @description determines the cosine similarity for two matrices
#' @param W1 matrix
#' @param W2 matrix
#' @return matrix of cosine similarity between dimensions of input matrices
#' @export

test_cosine_similarity = function()  {
  W1 <- readRDS(system.file("data","example_mat1.rds", package = 'CoSim'))
  W2 <- readRDS(system.file("data","example_mat2.rds", package = 'CoSim'))
  result = cosine_similarity(W1, W2)
  return(result)
}
