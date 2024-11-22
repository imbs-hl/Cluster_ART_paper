#' function to simulate data for simulation study based on 
#' Seifert, S. et al. (2019). Bioinformatics, 35:3663–3671
#' @param n_train           number of observations in training data set
#' @param n_test            number of observations in test data set
#' @param p_cor             number of correlated variables with variables X1, X2, X5 and X6
#' @param nGenesncorr       number of additional noise variables (2 noise variables X5 and X6 with correlated variables not included)
#' @param minCor1           minimal correlation for correlated variables with X1 and X5
#' @param maxCor1           maximum correlation for correlated variables with X1 and X5
#' @param minCor2           minimal correlation for correlated variables with X2 and X6
#' @param maxCor2           maximum correlation for correlated variables with X2 and X6
#' @param b1                effect size for X1 and X3
#' @param b2                effect size for X2 and X4
#' @param mtry              hyperparameter of ranger to select mtry split candidates
#' @param min.node.size     hyperparameter of ranger to stop tree growth at minimal node size
#' @param num.trees         hyperparameter of ranger that defines the number of trees in the random forest


simulate_data <- function(n_train, n_test, p_cor, nGenesncorr,
                          minCor1, maxCor1, minCor2, maxCor2, b1, b2, 
                          mtry, min.node.size, num.trees = 500, ...){

    
    ## simulate data for train and test data set combined
    nSamples <- n_train + n_test
  
    # 2 main effect variables X1, X2 with correlated variables
    MEX1 <- rnorm(nSamples)
    datX1 <- simulateModule(MEX1,p_cor,minCor=minCor1,maxCor=maxCor1)

    MEX2 <- rnorm(nSamples)
    datX2 <- simulateModule(MEX2,p_cor,minCor=minCor2,maxCor=maxCor2)

    # 2 main effect variables X3, X4 without correlated variables
    MEX3 <- rnorm(nSamples)
    MEX4 <- rnorm(nSamples)
    
    # 2 noise variables X5, X6 with correlated variables
    MEX5 <- rnorm(nSamples)
    datX5 <- simulateModule(MEX5,p_cor,minCor=minCor1,maxCor=maxCor1)

    MEX6 <- rnorm(nSamples)
    datX6 <- simulateModule(MEX6,p_cor,minCor=minCor2,maxCor=maxCor2)

    # uncorrelated noise variables
    datncorr <- replicate(nGenesncorr,rnorm(nSamples))
    
    # combine data to one matrix
    x <- as.matrix(cbind(MEX1,MEX2,MEX3,MEX4,MEX5,MEX6,
                         datX1,datX2,datX5,datX6,datncorr))
    
    # variable names
    s1 = c(1:p_cor)
    s2 = c(1:p_cor)
    t = c(1:(nGenesncorr))
    cp1 = "cp1"
    cp2 = "cp2"
    cp5 = "cp5"
    cp6 = "cp6"
    namecp1 = paste("cp1",s1,sep = "_")
    namecp2 = paste("cp2",s2,sep = "_")
    namecp5 = paste("cp5",s1,sep = "_")
    namecp6 = paste("cp6",s2,sep = "_")
    namecgn = paste("cgn",t,sep = "_")
    varnames = c("X1","X2","X3","X4","X5","X6",namecp1,namecp2,namecp5,namecp6,namecgn)
    colnames(x) <- varnames
    
    # simulate outcome 
    y <- as.vector(t(b1*MEX1 + b2*MEX2 + b1*MEX3 + b2*MEX4 + rnorm(nSamples,0,0.2)))
    
    all_data <- cbind(y,x)
    
    # divide data in train and test data 
    train_data <- all_data[1:n_train,] %>% as.data.frame()
    test_data <- all_data[(n_train+1):(n_train+n_test),] %>% as.data.frame()
    
    # train ranger object
    rf <- ranger(y ~ ., data = train_data, mtry = mtry, num.trees = num.trees, min.node.size = min.node.size, importance = "permutation")
    
    # set effect_var_ids
    effect_var_ids <- c("X1","X2","X3", "X4")
    correlated_effect_var_ids <- c(namecp1, namecp2)
    noise_var_ids <- c("X5", "X6", namecp5, namecp6, namecgn)
    
    return(list(information_df = data.frame(model = "Regression",
                                            n_train = n_train,
                                            n_test = n_test,
                                            p_cor = p_cor,
                                            b1 = b1,
                                            b2 = b2,
                                            num.trees = num.trees,
                                            min.node.size = min.node.size,
                                            mtry = mtry
                                            ),
                                            train_data = train_data,
                                            test_data = test_data,
                                            effect_var_ids = effect_var_ids,
                                            correlated_effect_var_ids = correlated_effect_var_ids,
                                            noise_var_ids = noise_var_ids,
                                            rf = rf
                                            )
           )
    }
    