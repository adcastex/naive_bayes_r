library(R6)
library(doParallel)

NaiveBayes <- R6Class("NaiveBayes",
                      public = list(
                        
                        ###Function  -- Fit
                        #Function that calculates the probabilities associated with the Naive Bayes categorical model
                        
                        fit = function(X, y, preproc = TRUE, nb_classe = 6, epsilon = NULL, g_na = TRUE) {
                          
                          # Cancel function if X and y are not entered and number of classes (discretization) < 0
                          if(missing(X) || missing(y) || nb_classe<0){
                            stop("fit requires a data frame and a target variable. Moreover nb_classe should be an integer greater than 0")
                          }
                          
                          #Cancel function if X is not a data frame
                          if(!is.data.frame(X)){
                            stop("The data source X must be a data frame")
                          }
                          
                          #Cancel function if y is not a vector
                          if(!is.vector(y)){
                            stop("The target variable must be a vector")
                          }
                          
                          #Cancel function if y is not a character
                          if(!is.character(y)){
                            stop("The target variable must be a character")
                          }
                          
                          #Cancel function if data missing from target variable y
                          
                          if(any(is.na(y))){
                            stop("Function interrupted: there are missing values in the target variable 'y'")
                          }
                          
                          
                          private$etu_data(X,y)
                          
                          private$g_na=g_na
                          # NA management in features
                          
                          if(g_na && any(is.na(X))){
                            type_col = lapply(X, class)
                            X = private$rem_na(X,type_col)
                          }
                          
                          
                          # Preprocessing
                          if (preproc==TRUE) {
                            private$preproc = preproc
                            private$nb_classe = nb_classe
                            X = private$gen_disc(X,"fit")
                            
                          }
                          
                          #Get the number of features
                          private$compt_val(X)
                          
                          
                          ## Probabilities of each class in training data
                          prior_prob <- table(y) / nrow(X)
                          cond_probs <- list()
                          features <-names(X)
                          
                          # Addition of an epsilon to avoid biasing a posteriori probability results in the model
                          if (!is.null(epsilon)) {
                            private$epsilon <- epsilon
                          }
                          
                          ## Conditional probabilities
                          
                          #Parallelization
                          cl <- makeCluster(detectCores() - 1)  # Detecting the number of cores
                          registerDoParallel(cl)
                          
                          
                          # Apply a parallel function to each value in the training data class
                          
                          cond_probs <- foreach(i = 1:ncol(X)) %dopar% {
                            cross_table <- table(X[, i], y) + private$epsilon
                            t(prop.table(cross_table, margin = 2))
                          }
                          
                          # Stop parallel cluster
                          stopCluster(cl)
                          
                          # Assigning values to our attributes
                          private$prior_prob <- prior_prob
                          private$cond_probs <- cond_probs
                          private$features<-features
                          return(self)
                        },
                        
                        ###Function -- Features importance
                        #Function that returns a graph showing the importance of features in the model
                        
                        compute_and_plot_importance = function() {
                          
                          ## Deviation calculation
                          deviations <- list()
                          
                          cond_probs <- private$cond_probs
                          
                          # Extraction of features names
                          features<-private$features
                          
                          #Parallelization
                          cl <- makeCluster(detectCores() - 1)  # Detecting the number of cores
                          registerDoParallel(cl)
                          
                          
                          # Browse conditional probability matrices
                          deviations <- foreach(i = seq_along(cond_probs)) %dopar% {
                            cond_prob_matrix <- cond_probs[[i]]
                            # Calculate the standard deviation for each column (modality)
                            deviation_matrix <- apply(cond_prob_matrix, 2, sd)
                            deviation_matrix
                          }
                          
                          # Stop parallel cluster
                          stopCluster(cl)
                          
                          
                          ## Calculation of indicators
                          
                          # Sum the standard deviations for each feature
                          indicateurs <- sapply(deviations, function(deviation_matrix) sum(deviation_matrix))
                          
                          # indicators vector
                          indicateurs_vecteur <- unlist(indicateurs)
                          
                          
                          # Create a data frame to facilitate plotting
                          data_plot <- data.frame(Feature = features, Indicateur = indicateurs_vecteur)
                          
                          # Sort data frame by indicator in descending order
                          data_plot <- data_plot[order(-data_plot$Indicateur), ]
                          
                          # Barplot layout
                          barplot_data <- barplot(data_plot$Indicateur, names.arg = data_plot$Feature, horiz = FALSE,
                                                  col = "lightblue", main = "Indicateur d'importance des variables",
                                                  cex.names = 0.7, xlab = "Variables", ylab = "Somme des écarts-types", ylim = c(0, max(data_plot$Indicateur)+0.5))
                          
                          # Ajout d'étiquettes de valeurs
                          text(x = barplot_data, y = data_plot$Indicateur, label = round(data_plot$Indicateur, 2),
                               pos = 3, cex = 0.7, col = "black")
                        },
                        
                        ### Function -- Predict class
                        #Function that returns the predicted class of the new dataset
                        
                        predict = function(new_data) {
                          
                          # Cancel function if new_data is not entered
                          if(missing(new_data)){
                            stop("predict requires a data frame.")
                          }
                          
                          #Cancel function if new_data is not a data frame
                          if(!is.data.frame(new_data)){
                            stop("The data source new_data must be a data frame")
                          }
                          
                          
                          #Preprocessing
                          if (private$preproc==TRUE) {
                            new_data = private$gen_disc(new_data,"pred")
                          }
                          
                          
                          # Extract a priori and conditional probabilities from the model
                          prior_prob <- private$prior_prob
                          cond_probs <- private$cond_probs
                          
                          # Initialize a vector to store predictions
                          predictions <- vector("character", length = nrow(new_data))
                          
                          ## Calculation of posterior probabilities
                          
                          for (i in 1:nrow(new_data)) {
                            # Initialize posterior probabilities for each class
                            posterior_probs <- rep(1, length(prior_prob))
                            for (j in 1:ncol(new_data)) {
                              # Update posterior probabilities based on conditional probabilities
                              feature_values <- as.character(new_data[i, j])
                              if (!is.null(cond_probs[[j]]) && feature_values %in% colnames(cond_probs[[j]])) {
                                posterior_probs <- posterior_probs * cond_probs[[j]][, feature_values]
                              } else {
                                posterior_probs <- posterior_probs * private$epsilon
                              }
                              
                            }
                            
                            # Normalize posterior probabilities
                            posterior_probs <- posterior_probs * prior_prob
                            posterior_probs <- posterior_probs / sum(posterior_probs)
                            
                            # Select the class with the highest probability
                            predicted_class <- names(prior_prob)[which.max(posterior_probs)]
                            
                            # Store prediction in prediction vector
                            predictions[i] <- predicted_class
                            
                          }
                          
                          return(predictions)
                        },
                        
                        ###Function -- Predict Probability
                        #Function that returns membership probabilities for each class in the model
                        
                        predict_proba = function(new_data) {
                          
                          # Cancel function if new_data is not entered
                          if(missing(new_data)){
                            stop("predict_proba requires a data frame.")
                          }
                          
                          #Cancel function if new_data is not a data frame
                          if(!is.data.frame(new_data)){
                            stop("The data source new_data must be a data frame")
                          }
                          
                          
                          if (private$preproc==TRUE) {
                            new_data = private$gen_disc(new_data,"pred")
                          }
                          
                          
                          # Extraire les probabilités a priori et conditionnelles du modèle
                          prior_prob <- private$prior_prob
                          cond_probs <- private$cond_probs
                          
                          # Initialize a matrix to store probabilities for each class
                          probabilities <- matrix(0, nrow = nrow(new_data), ncol = length(prior_prob), dimnames = list(NULL, names(prior_prob)))
                          
                          ## Calculation of probabilities
                          
                          for (i in 1:nrow(new_data)) {
                            # Initialize posterior probabilities for each class
                            posterior_probs <- rep(1, length(prior_prob))
                            
                            #For each feature (column) in the new data
                            for (j in 1:ncol(new_data)) {
                              # Update posterior probabilities based on conditional probabilities
                              feature_values <- as.character(new_data[i, j])
                              
                              if (!is.null(cond_probs[[j]]) && feature_values %in% colnames(cond_probs[[j]])) {
                                posterior_probs <- posterior_probs * cond_probs[[j]][, as.character(new_data[i, j])]
                              } else {
                                posterior_probs <- posterior_probs * private$epsilon
                              }
                            }
                            
                            # Normalize posterior probabilities
                            posterior_probs <- posterior_probs * prior_prob
                            posterior_probs <- posterior_probs / sum(posterior_probs)
                            
                            # Store probabilities in the matrix
                            probabilities[i, ] <- posterior_probs
                          }
                          
                          
                          return(probabilities)
                        },
                        
                        ###Function -- Print
                        #Function that returns a short description of the model
                        
                        Print=function(){
                          cat("Naive Bayes Categorical Model Summary\n")
                          cat("------------------------------------\n")
                          cat("Number of Predictive Variables: ", private$nb_variables, "\n")
                          cat("Number of Classes in the Target Variable: ", length(private$nb_out_classe), "\n")
                          cat("Training Data Observations: ", private$nb_data_train, "\n")
                          
                          
                          invisible(NULL)  # Return NULL invisibly
                        },
                        
                        ###Function -- Summary
                        #Function that returns descriptive statistics for the features and target variable
                        
                        Summary=function(){
                          
                          cat("--------------SUMMARY--------------\n")
                          # Summary of Targer variable
                          cat("Target variable Summary:\n")
                          cat("Number of Predictive Variables: ", private$nb_variables, "\n")
                          cat("Number of Classes in the Target Variable: ", length(private$nb_out_classe), "\n")
                          
                          cat("------------------------------------\n")
                          # Summary of Preprocessing
                          cat("Preprocessing Summary:\n")
                          cat("   Discretization: ", ifelse(private$preproc, "Applied", "Not Applied"), "\n")
                          cat("   Number of Discretization Classes: ", private$nb_classe, "\n")
                          cat("   NA Handling: ", ifelse(private$g_na, "Applied", "Not Applied"), "\n")
                          
                          cat("------------------------------------\n")
                          # Summary of Training Data
                          cat("Training Data Summary:\n")
                          cat("   Number of Observations: ", private$nb_data_train, "\n")
                          cat("   Min Values: ", private$min_parc_df, "\n")
                          cat("   Max Values: ", private$max_parc_df, "\n")
                          cat("   Number of Unique Values for Each Variable: ", private$nb_valu, "\n")
                          
                          cat("------------------------------------\n")
                          # Prior Probabilities
                          cat("Prior Probabilities:\n")
                          print(private$prior_prob)
                          
                          
                          invisible(NULL)  # Return NULL invisibly
                        }
                        
                        
                      ),
                      
                      ###Constructor
                      private = list(
                        g_na = NULL,
                        preproc = NULL,
                        nb_classe = NULL,
                        prior_prob = NULL,
                        cond_probs = NULL,
                        features = NULL,
                        nb_data_train = NULL,
                        nb_variables = NULL,
                        nb_out_classe = NULL,
                        nb_valu = NULL,
                        min_parc_df = NULL,
                        max_parc_df = NULL,
                        epsilon = 0.001,
                        list_remp_NA=list(),
                        med = NULL,
                        classe_maj = NULL,
                        
                        matrice_param_preproc = NULL,
                        
                        ###Function -- NA management
                        #Function that replaces the NA of a numeric variable with the mean and of a qualitative variable with the mode.
                        
                        rem_na = function(X,liste_class ){
                          
                          # Loop through each column in the dataframe
                          for (i in 1:length(liste_class)){
                            # Check if the column is numeric
                            if(liste_class[i]=="numeric"){
                              
                              # For numeric columns, replace missing values with the mean of the column
                              private$list_remp_NA = append(private$list_remp_NA,round(median(X[,i], na.rm = TRUE),digits = 2))
                              liste=X[,i]
                              liste[is.na(liste)] = private$list_remp_NA[i]
                              liste=as.double(liste)
                              X[,i] = liste
                              
                            }else{ 
                              # For non-numeric columns, replace missing values with the most frequent value (mode)
                              liste=X[,i]
                              table_occurrences <- table(X[,i])
                              classe_majoritaire <- names(table_occurrences)[which.max(table_occurrences)]
                              private$list_remp_NA = append(private$list_remp_NA,classe_majoritaire)
                              liste[is.na(liste)] = private$list_remp_NA[i]
                              X[,i]=liste
                            }
                          }
                          # Return the dataframe with missing values replaced
                          return(X)
                        },
                        
                        ###Function -- Discretization
                        # Function for discretization of numerics features
                        
                        dis = function(X, col, place){
                          
                          # If the function is in the "fit" phase
                          if (place=="fit"){
                            
                            # If the column is numeric
                            if(class(X)=="numeric"){
                              
                              # Calculate the minimum value of the column
                              mini=min(X, na.rm = TRUE)
                              private$matrice_param_preproc[1,col] = mini
                              
                              # Calculate the maximum value of the column
                              maxi=max(X, na.rm = TRUE)
                              private$matrice_param_preproc[2,col] = maxi
                              
                              # Calculate the interval for discretization
                              inter=(maxi-mini)/private$nb_classe
                              private$matrice_param_preproc[3,col] = inter
                              
                              # Generate breakpoints for discretization
                              points_de_coupure <- seq(from = mini, to = maxi, by = inter)
                              
                              # Discretize the column using the calculated breakpoints
                              disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
                              
                              # Return the discretized column
                              return(disc)
                              
                            }else{
                              # If the column is not numeric, return the original column
                              return(X)
                            }
                            
                          }else{
                            # If the function is in the "predict" phase
                            if(class(X)=="numeric"){
                              # Retrieve the parameters calculated during the "fit" phase
                              mini = private$matrice_param_preproc[1,col]
                              if(min(X, na.rm = TRUE) < mini){
                                mini = min(X, na.rm = TRUE)
                              }
                              maxi = private$matrice_param_preproc[2,col]
                              if(max(X, na.rm = TRUE) > maxi){
                                maxi = max(X, na.rm = TRUE)
                              }
                              
                              # Calculate the interval for discretization
                              inter = private$matrice_param_preproc[3,col]
                              
                              # Generate breakpoints for discretization
                              points_de_coupure <- seq(from = mini, to = maxi, by = inter)
                              
                              # Discretize the column using the calculated breakpoints
                              disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
                              
                              # Return the discretized column
                              return(disc)
                            }else{
                              # If the column is not numeric, return the original column
                              return(X)
                            }
                          }
                        },
                        
                        ###Function -- Application discretization
                        #Function for general discretization
                        
                        gen_disc = function(X, place){
                          # If the function is in the "fit" phase
                          if(place=="fit"){
                            
                            # Initialize a matrix to store discretization parameters
                            private$matrice_param_preproc = matrix(0, nrow = 3, ncol = length(X))
                          }
                          
                          # Iterate over each column in the dataset
                          for (i in 1:length(X)){
                            # Apply the discretization function to each column
                            X[,i] <- private$dis(X[,i], i, place)
                          }
                          
                          # Return the dataset with discretized columns
                          return(X)
                        },
                        
                        ###Function -- Descriptive statistics
                        #Function for studying data statistics
                        
                        etu_data = function(X,y){
                          # Set private attributes related to data statistics
                          private$nb_variables = ncol(X)
                          private$nb_data_train = nrow(X)
                          private$nb_out_classe = unique(y)
                          private$min_parc_df = sapply(X, min)
                          private$max_parc_df = sapply(X, max)
                        },
                        
                        ###Function -- Count the number of features
                        #Function for counting the number of predictive variables
                        
                        compt_val = function(X){
                          # Count the number of unique values in each column and store the result
                          private$nb_valu <- sapply(X, function(col) length(unique(col)))
                        }
                      )
)

