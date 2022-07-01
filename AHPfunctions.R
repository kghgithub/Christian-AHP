#This sets the number of decimals places to 3
options(digits = 3)

#These are the set of functions used to compute the overall relative weights of the AHP
#You must run all the functions, but you will only use the last one
normalize <- function(c_vector, item) {
  c <- c_vector/c_vector[item]

  return (c)
}
matrix_filler <- function(matrix, c_vector, n) {

  for (j in 1:n) {
    for (i in j:n) {
      matrix[i,j] <- normalize(c_vector, j)[i]
    }}

  for (j in 2:n) {
    for (i in 1:n-1) {
      matrix[i,j] <- ifelse(is.na(matrix[i,j]), matrix[j,i]^-1, matrix[i,j])
    }}

  return(matrix)
}
relative_weights <- function(matrix) {

  f_col <- matrix[,1]
  weights = f_col/colSums(matrix)[1]

  return(weights)
}
criteria_matrix <- function(c_vector, criteria) {

  n <- length(c_vector)
  c_mat <- matrix(nrow = n, ncol = n)
  dimnames(c_mat) =list(
    criteria,
    criteria
  )

  fill_mat <- matrix_filler(c_mat, c_vector, n)

  w <- relative_weights(fill_mat)

  return(w)
}
crit_alt_matrix <-  function(ac_vector, alternatives){
  #This will return the relative weights not the matrix
  n <- length(ac_vector)
  ac_mat <- matrix(nrow = n, ncol = n)
  dimnames(ac_mat) =list(
    alt,
    alt
  )

  fill_mat <- matrix_filler(ac_mat, ac_vector, n)

  w <- relative_weights(fill_mat)

  return(w)
}
rel_wei_list <- function(criteria, alter, list_of_alternative_weights) {

  m <- length(criteria)
  n <- length(alter)
  rel_w <- list()
  for (i in 1:m) {
    w = crit_alt_matrix(list_of_alternative_weights[[i]],alter)
    rel_w[[i]] =w
  }

  return(rel_w)
}
ove_weight <- function(alt_weights, crit_weights) {

  alter = names(alt_weights[[1]])

  ow <- list()
  for (i in 1:length(alt_weights)) {
    w <- alt_weights[[i]]*crit_weights[i]
    ow[[i]] <- w
  }

  orw <- numeric()
  ap <- 0

  for (i in 1:length(crit_weights)) {
    for (j in 1:length(alt_weights)) {

      a <- ow[[j]][i]
      ap <- ap+a
    }

    orw[i] = ap
    ap=0
  }

  listing <- setNames(orw, alter)
  l <- listing[!is.na(listing)]

  return(l)
}
AHP <- function(alter, list_of_alternative_weights, c_vector, criteria) {

  orw = ove_weight(alt_weights = rel_wei_list(criteria, alter, list_of_alternative_weights), crit_weights = criteria_matrix(c_vector, criteria))

  return(orw)
}

gitinit
