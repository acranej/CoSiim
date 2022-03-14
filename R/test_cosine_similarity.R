#' @name test_cosine_similarity
#' @title shows example output of cosine similarity
#' @description determines the cosine similarity for two matrices
#' @return matrix of cosine similarity between dimensions of input matrices
#' @examples
#'
#' # test_cosine_similarity()
#'
#' @export
test_cosine_similarity = function()  {
  matrix1 <- readRDS(system.file("data","example_mat1.rds", package = 'CoSim'))
  matrix2 <- readRDS(system.file("data","example_mat2.rds", package = 'CoSim'))
  result = cosine_similarity(matrix1, matrix2)
  return(result)
}


#' @name example_inputs
#' @title shows example matrices for input
#' @description returns a list of 2 matrices to unlist and view for example input
#' @return list of example input matrices
#' @examples
#'
#' # example_inputs()[1]
#' # example_inputs()[2]
#'
#' @export
example_inputs = function() {
  matrix1 <- readRDS(system.file("data","example_mat1.rds", package = 'CoSim'))
  matrix2 <- readRDS(system.file("data","example_mat2.rds", package = 'CoSim'))
  return(list(matrix1, matrix2))
}
