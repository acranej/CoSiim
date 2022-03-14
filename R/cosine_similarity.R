#' @name cosine_similarity
#' @title Determines cosine similarity between two \code{\link{matrix}} objects
#' @description Determines the cosine similarity for two matrices. Requires that a dimension of the two input matrices is equal
#' @param mat1 \code{\link{matrix}} with row and column names
#' @param mat2 \code{\link{matrix}} with row and column names
#' @examples
#'
#' # cosine_similarity(my_matrix1, my_matrix2)
#'
#' @return \code{\link{matrix}} of cosine similarity between the input matrices
#' @export
cosine_similarity = function(mat1 = NULL, mat2 = NULL) {

  ### checks if the inputs are in matrix format
  if(!(is.matrix(mat1) | is.matrix(mat2))) {
    stop("Please make input as a matrix")
  }

  ### checks the matrix is numeric
  if(!(is.numeric(mat1[]))) {
    tryCatch(
      {
        ### try to coerce into numerical matrix
        cat("Attempting to coerce into numeric matrix...\n")
        class(mat1) <- "numeric"
        cat('Coerced into numeric')
      },
      error = function(cond) {
        message(cond)
      },
      warning=function(cond) {
        message(cond)
        stop("Cannot allow NAs in matrix, please ensurer numerical matrix")
      }
    )
  }

  ### checks the matrix is numeric
  if(!(is.numeric(mat2[]))) {
    tryCatch(
      {
        ### try to coerce into numerical matrix
        cat("Attempting to coerce into numeric matrix...\n")
        class(mat2) <- "numeric"
        cat('Coerced into numeric')
      },
      error = function(cond) {
        message(cond)
      },
      warning=function(cond) {
        message(cond)
        stop("Cannot allow NAs in matrix, please ensurer numerical matrix")
      }
    )
  }

  ### check for no NA values
  if(any(is.na(mat1))) {
    stop("The first argument has NA values")
  }

  if(any(is.na(mat2))) {
    stop("The second argument has NA values")
  }

  ### check for rrow and column names
  if(is.null(rownames(mat1))  | is.null(rownames(mat2)) | is.null(colnames(mat1)) | is.null(colnames(mat2))) {
    stop("Provide row names and column names for input matrices. Row count and row names should be the same for both inputs.")
  }

  ### try to transpose matrices for the input, rows need to be the same # of dimensions
  ### same row number
  if((nrow(mat1) == nrow(mat2))) {
    ### do the names match?
    if(!(all(rownames(mat1) == rownames(mat2)))) {
      if(all(rownames(mat1) == colnames(mat2))) { ### do the row names of 1 match column names of 2?
        mat2 <- t(mat2)
      } else if(all(colnames(mat1) == rownames(mat2))) { ### do the column names of 1  match rows of 2?
        mat1 <- t(mat1)
      } else {
        stop("Names in the matrices don't match, input two matries that have the same number of rows and row names")
      }
    }
    ## rows don't match
  } else {
    ### do the row counts of 1 match the column count of 2?
    if(nrow(mat1) == ncol(mat2)) {
      mat2 <- t(mat2)
    } else if(ncol(mat1) == nrow(mat2)) { ### do the col counts of 1 match the row count of 2?
      mat1 <- t(mat1)
    } else if(ncol(mat1) == ncol(mat2)) {  ### do the column counts match?
      mat1 <- t(mat1)
      mat2 <- t(mat2)
    } else {
      stop("Dimensions of input matrices are incompatible. Please read about cosine similarity.")
    }
  }
  if(all(!(rownames(mat1) == rownames(mat2)))) { ### names match?
    stop("Names don't match. Please have at least 1 dimension in each matrix with the same length and name values")
  }

  x <- array(0,dim=c(ncol(mat1),ncol(mat2)))
  for (i in 1:ncol(mat1)) {
    for (j in 1:ncol(mat2)) {
      x[i,j] <- mat1[,i]%*%mat2[,j]/sqrt(sum(mat1[,i]^2))/sqrt(sum(mat2[,j]^2))
    }
  }
  rownames(x) <- colnames(mat1)
  colnames(x) <- colnames(mat2)
  return(x)
}
