baing <- function(X, kmax, jj) {
  # =========================================================================
  # DESCRIPTION
  # This function determines the number of factors to be selected for a given
  # dataset using one of three information criteria specified by the user.
  # The user also specifies the maximum number of factors to be selected.
  #
  # Authour : Anna SIMONI - Macroeconometrics and Machine Learning course
  # -------------------------------------------------------------------------
  # INPUTS
  #           X       = dataset (one series per column)
  #           kmax    = an integer indicating the maximum number of factors
  #                     to be estimated
  #           jj      = an integer indicating the information criterion used
  #                     for selecting the number of factors; it can take on
  #                     the following values:
  #                           1 (information criterion PC_p1)
  #                           2 (information criterion PC_p2)
  #                           3 (information criterion PC_p3)
  #
  # OUTPUTS
  #           ic1     = number of factors selected
  #           chat    = values of X predicted by the factors
  #           Fhat    = factors
  #           eigval  = eivenvalues of X'*X (or X*X' if N>T)
  # -------------------------------------------------------------------------
  #
  # PART 1: SETUP
  
  # Number of observations per series (i.e. number of rows)
  T <- nrow(X)
  
  # Number of series (i.e. number of columns)
  N <- ncol(X)
  
  # Total number of observations
  NT <- N * T
  
  # Number of rows + columns
  NT1 <- N + T
  
  # PART 2: OVERFITTING PENALTY
  # Determine penalty for overfitting based on the selected information criterion.
  
  # Allocate memory for overfitting penalty
  #CT <- numeric(kmax)
  
  # Array containing possible number of factors that can be selected (1 to kmax)
  ii <- 1:kmax
  
  # The smaller of N and T
  GCT <- min(N, T)
  
  # Calculate penalty based on criterion determined by jj.
  if (jj == 1){
    # Criterion PC_p1
    CT <- log(NT / NT1) * ii * NT1 / NT
  } else if (jj == 2){
    # Criterion PC_p2
    CT <- (NT1 / NT) * log(min(N, T)) * ii
  } else if (jj == 3){
    # Criterion PC_p3
    CT <- ii * log(GCT) / GCT
  }
  
  # PART 3: SELECT NUMBER OF FACTORS
  # Perform principal component analysis on the dataset and select the number
  # of factors that minimizes the specified information criterion.
  
  # RUN PRINCIPAL COMPONENT ANALYSIS
  
  # Get components, loadings, and eigenvalues
  XX <-as.matrix(X)
  if (T < N) {
    # Singular value decomposition
    svd_result <- svd(XX %*% t(XX))
    
    # Components
    Fhat0 <- sqrt(T) * svd_result$u
    
    # Loadings
    Lambda0 <- t(XX) %*% Fhat0 / T
    
  } else {
    # Singular value decomposition
    svd_result <- svd(t(XX) %*% XX)       # Alternatively, you can use "eigen(t(XX) %*% XX)" and then "svd_result$vectors"
    
    # Loadings
    Lambda0 <- sqrt(N) * svd_result$u
    
    # Components
    Fhat0 <- XX %*% Lambda0 / N
    
  }
  
  # SELECT NUMBER OF FACTORS
  
  # Preallocate memory
  Sigma <- numeric(kmax + 1)  # sum of squared residuals divided by NT
  # "numeric()": it creates numeric vectors or matrices.
  # It initializes the vector or matrix with numeric values (defaulting to zeros).
  IC1 <- matrix(0, nrow = 1, ncol = kmax + 1)  # information criterion value
  
  # Loop through all possibilities for the number of factors
  for (i in kmax:1) {
    
    # Identify factors as first i components
    Fhat <- Fhat0[, 1:i]
    
    # Identify factor loadings as first i loadings
    lambda <- Lambda0[, 1:i]
    
    # Predict X using i factors
    chat <- Fhat %*% t(lambda)
    
    # Residuals from predicting X using the factors
    ehat <- X - chat
    
    # Sum of squared residuals divided by NT
    Sigma[i] <- mean(colSums(ehat^2/T))
    
    # Value of the information criterion when using i factors
    IC1[i] <- log(Sigma[i]) + CT[i]
  }
  
  # Sum of squared residuals when using no factors to predict X (i.e.
  # fitted values are set to 0)
  Sigma[kmax + 1] <- mean(colSums(X^2) / T)
  
  # Value of the information criterion when using no factors
  IC1[, kmax + 1] <- log(Sigma[kmax + 1])
  
  # Number of factors that minimizes the information criterion
  ic1 <- minindc(t(IC1))
  
  # Set ic1 = 0 if ic1 > kmax (i.e. no factors are selected if the value of the
  # information criterion is minimized when no factors are used)
  ic1 <- ifelse(ic1 > kmax, 0, ic1)
  
  # PART 4: SAVE OTHER OUTPUT
  
  # Factors and loadings when the number of factors set to kmax
  Fhat <- Fhat0[, 1:kmax]  # factors
  Lambda <- Lambda0[, 1:kmax]  # factor loadings
  
  # Predict X using kmax factors
  chat <- Fhat %*% t(Lambda)
  
  # Get the eigenvalues corresponding to X'*X (or X*X' if N > T)
  eigval <- svd_result$d
  
  # Return the results
  return(list(ic1 = ic1, chat = chat, Fhat = Fhat, eigval = eigval, Lambda = Lambda))
}


#########################
## SUBFUNCTION minindc ##
#########################
minindc <- function(x) {
  
  # DESCRIPTION
  # This function finds the index of the minimum value for each column of a given matrix. The function assumes that
  # the minimum value of each column occurs only once within that column. The function returns an error if this is
  # not the case.
  apply(x, 2, which.min)    # "2": it specifies that the function ("which.min" in this case) should be applied to
  # each column. If it were 1, the function would be applied to each row.
  # "which.min": it returns the index of the first minimum value in a vector.
}
