

### Create a contact matrix with filters

## Put filter = TRUE
## Then put the value to filter by in the various filter variables

cm_filter <- function(
  df  = df,
  matrix_output = TRUE,
  #country = "United States",
  age_limits =c(0,10,20,30,40,50,60),
  symmetric =TRUE,
  filter = FALSE,
  phys_contact = NULL,
  home  = NULL,
  other_house = NULL,
  work = NULL,
  school = NULL,
  gender = NULL,
  other = NULL,
  boots = 1
){
  
  filter_text <- list()
  
  if(filter){
    filter_text$phys_contact <- phys_contact
    filter_text$cnt_home <- home
    filter_text$cnt_other_house <- other_house
    filter_text$cnt_work <- work
    filter_text$cnt_school <- school
    filter_text$cnt_otherplace <- other
    filter_text$cnt_gender <- gender
  }
  
  x <- contact_matrix(
    df, 
    #countries = country, 
    age.limits = age_limits, 
    symmetric = symmetric,
    filter = filter_text,
    estimated.contact.age = "sample",
    n = boots
  )
  if(matrix_output){
    if(boots == 1){
      return(x$matrix)
    } else {
      return(x$matrices)
    }  
  } else {
    
    return(x)
  }  
}

scale_factor_R <- function(mat1, mat2, i = 1, ...){
  x1 <- update_cm(mat1, mat2, i = i, ...)
  x2 <- mat2[[i]]
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  list(max_eigen = max_eigen_ratio(x1, x2),
       x1=x1)
}

update_cm <- function(
  mat1, mat2, 
  impute_rows = 1:2, impute_cols = 1:7, 
  i = 1,  
  observed_rows = 2:7, 
  observed_col = 2:7,
  school = FALSE,
  ...
){
  
  mat1 <- mat1[[i]]$matrix
  mat2 <- mat2[[i]]
  
  update_mat <- matrix(0, nrow = nrow(mat1), ncol = ncol(mat1))
  row.names(update_mat) <- colnames(mat1)
  colnames(update_mat)  <- colnames(mat1)
  
  #non_imputed_values <- mat1[row, col]
  update_mat <- mat1
  
  imputed_values <- impute_values(i = i, school = school, ... )[impute_rows, impute_cols]
  update_mat[impute_rows, impute_cols] <- imputed_values
  
  symm_mat(update_mat)
}

# Split contact matrix to only the required rows and columns
## This is because we do not have data for participants <18
split_cm <- function(mat, i = 1, row = 3:7, col = 3:7, ...){
  mat[[i]]$matrix[row,col]
}

## Make the matrix symmetric
symm_mat <- function(x){
  (x + t(x))/ 2  
}

## Calculate ratio of max eigen values 
# This gives a sense of different magnitudes of the matrices
max_eigen_ratio <- function(x, y){
  max(eigen(x, only.values = TRUE)$values)/max(eigen(y, only.values = TRUE)$values)
  
}


# Take two matrices and calculate the scale factor for them
## Repeats the above functions to get the ratio of the max eigen values
## Of each matrix
scale_factor1 <- function(mat1, mat2, i=i... ){
  #x1 <- split_cm(mat1, ...)
  #x2 <- split_cm(mat2, ...)
  x1 <- mat1[[i]]$matrix[3:7,3:7]
  x2<- mat2[[i]][3:7,3:7]
  
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}

scale_factor <- function(mat1, mat2, ...){
  #x1 <- split_cm(mat1, ...)
  #x2 <- split_cm(mat2, ...)
  x1 <- mat1[[i]]$matrix[3:7,3:7]
  x2<- mat2[[i]][3:7,3:7]
  
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}




# Take a full matrix and scale it based on ratio of mat1 and mat2
scale_matrix <- function(fullmat, mat1, mat2, i = 1, ...){
  #mat1 <- mat1[[i]]$matrix
  
  fullmat[[i]] * scale_factor(mat1, mat2, i = i, ...)
  
}