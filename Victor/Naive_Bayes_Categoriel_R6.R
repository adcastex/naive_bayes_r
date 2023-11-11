library(R6)

NaiveBayes <- R6Class("NaiveBayes",
                      public = list(
                        prior_prob = NULL,
                        cond_probs = NULL,
                        fit = function(X, y) {
                          # Probabilités a priori de chaque classe des données d'entraînement
                          prior_prob <- table(y) / nrow(X)
                          cond_probs <- list()
                          
                          # Probabilités conditionnelles
                          # On applique à chaque valeur de la classe des données d'entraînement une fonction
                          for (i in 1:ncol(X)) {
                            cond_probs[[i]] <- matrix(0, nrow = length(levels(X[, i])), ncol = length(unique(y)), dimnames = list(levels(X[, i]), levels(y)))
                            
                            # Ajout d'un epsilon = ne pas avoir de proba conditionnelle = 0 et ne pas biaiser les résultats des probas postérieures
                            epsilon <- 0.001
                            cross_table <- table(X[, i], y) + epsilon
                            
                            # Calculer les probabilités conditionnelles
                            cond_probs[[i]] <- t(prop.table(cross_table, margin = 2))
                          }
                          
                          # Set the attributes
                          self$prior_prob <- prior_prob
                          self$cond_probs <- cond_probs
                          return(self)
                        },
                        predict = function(new_data) {
                          # Extraire les probabilités a priori et conditionnelles du modèle
                          prior_prob <- self$prior_prob
                          cond_probs <- self$cond_probs
                          
                          # Initialiser un vecteur pour stocker les prédictions
                          predictions <- vector("character", length = nrow(new_data))
                          
                          # Pour chaque individu dans les nouvelles données
                          for (i in 1:nrow(new_data)) {
                            # Initialiser les probabilités postérieures pour chaque classe
                            posterior_probs <- rep(1, length(prior_prob))
                            
                            # Pour chaque caractéristique (colonne) dans les nouvelles données
                            for (j in 1:ncol(new_data)) {
                              # Mettre à jour les probabilités postérieures en fonction des probabilités conditionnelles
                              feature_values <- as.character(new_data[i, j])
                              posterior_probs <- posterior_probs * cond_probs[[j]][, feature_values]
                            }
                            
                            # Normaliser les probabilités postérieures
                            posterior_probs <- posterior_probs * prior_prob
                            posterior_probs <- posterior_probs / sum(posterior_probs)
                            
                            # Sélectionner la classe avec la probabilité la plus élevée
                            predicted_class <- names(prior_prob)[which.max(posterior_probs)]
                            
                            # Stocker la prédiction dans le vecteur de prédictions
                            predictions[i] <- predicted_class
                          }
                          
                          # Renvoyer le vecteur de prédictions
                          return(predictions)
                        },
                        predict_proba = function(new_data) {
                          # Extraire les probabilités a priori et conditionnelles du modèle
                          prior_prob <- self$prior_prob
                          cond_probs <- self$cond_probs
                          
                          # Initialiser une matrice pour stocker les probabilités pour chaque classe
                          probabilities <- matrix(0, nrow = nrow(new_data), ncol = length(prior_prob), dimnames = list(NULL, names(prior_prob)))
                          
                          # Pour chaque individu dans les nouvelles données
                          for (i in 1:nrow(new_data)) {
                            # Initialiser les probabilités postérieures pour chaque classe
                            posterior_probs <- rep(1, length(prior_prob))
                            
                            # Pour chaque caractéristique (colonne) dans les nouvelles données
                            for (j in 1:ncol(new_data)) {
                              # Mettez à jour les probabilités postérieures en fonction des probabilités conditionnelles
                              posterior_probs <- posterior_probs * cond_probs[[j]][, as.character(new_data[i, j])]
                            }
                            
                            # Normaliser les probabilités postérieures
                            posterior_probs <- posterior_probs * prior_prob
                            posterior_probs <- posterior_probs / sum(posterior_probs)
                            
                            # Stocker les probabilités dans la matrice
                            probabilities[i, ] <- posterior_probs
                          }
                          
                          # Renvoyer la matrice de probabilités
                          return(probabilities)
                        }
                      )
)

############### DATA FRAME IRIS ########################
dis<-function(X,nb_classe=6){
  
  mini=min(X)
  print(mini)
  maxi=max(X)
  print (maxi)
  inter=(maxi-mini)/nb_classe
  points_de_coupure <- seq(from = mini, to = maxi, by = inter)
  
  disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
  return(disc)
}



gen_disc<-function(X,nb_classe=6){
  X <- data.frame(apply(X, MARGIN = 2, FUN = function(i) dis(i,nb_classe)))
  return(X)
}

############## discretisation ##########################

# Charger le jeu de données Iris
data(iris)
set.seed(42)
n_rows <- nrow(iris)
train_index <- sample(1:n_rows, 0.7 * n_rows)
test_index <- setdiff(1:n_rows, train_index)
train_data <- iris[train_index, ]
test_data <- iris[test_index, ]

X= gen_disc(train_data[, -ncol(train_data)])
y=train_data$Species
new_data <- gen_disc(test_data[, -5])

################ UTILISER LA CLASSE  ##########
# Create an instance of the NaiveBayes class
nb_model <- NaiveBayes$new()

# Fit the model
nb_model$fit(X, y)

# Make predictions
predictions <- nb_model$predict(new_data)
cat("Prédictions:", predictions, "\n")

# Get class probabilities
probabilities <- nb_model$predict_proba(new_data)
cat("Probabilités:\n")
print(probabilities)
