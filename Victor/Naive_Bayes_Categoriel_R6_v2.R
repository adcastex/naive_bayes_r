library(R6)

### CLASSE NB ####
NaiveBayes <- R6Class("NaiveBayes",
                      public = list(
                        fit = function(X, y, preproc = NULL, nb_classe = 6) {
                          
                          private$etu_data(X,y)
                          
                          # Je set les varable preproc et nb_classe si initialisé
                          if(!is.null(preproc)){
                           private$preproc=preproc
                           private$nb_classe=nb_classe
                           X=private$gen_disc(X)
                          }
                          
                          # A voir pour faire une petite Etude de données ici 
                          
                          private$compt_val(X)
                          
                          
                          # Probabilités a priori de chaque classe des données d'entraînement
                          prior_prob <- table(y) / nrow(X)
                          cond_probs <- list()
                          features <-names(X)
                          
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
                          private$prior_prob <- prior_prob
                          private$cond_probs <- cond_probs
                          private$features<-features
                          return(self)
                        },
                        
                        compute_and_plot_importance = function() {
                          # Calcul des déviations
                          deviations <- list()
                          cond_probs <- private$cond_probs
                          
                          # Extraire les noms des variables (features)
                          features<-private$features
                          
                          # Parcourir les matrices de probabilités conditionnelles
                          for (i in seq_along(cond_probs)) {
                            # Récupérer la matrice de probabilités conditionnelles
                            cond_prob_matrix <- cond_probs[[i]]
                            # Calculer l'écart-type pour chaque colonne (modalités)
                            deviation_matrix <- apply(cond_prob_matrix, 2, sd)
                            deviations[[i]] <- deviation_matrix
                          }
                          
                          # Calcul des indicateurs
                          # Faire la somme des écart-type pour chacune des variables
                          indicateurs <- sapply(deviations, function(deviation_matrix) sum(deviation_matrix))
                          
                          # Concaténer les indicateurs dans un vecteur
                          indicateurs_vecteur <- unlist(indicateurs)
                          
                          
                          # Créer un data frame pour faciliter le tracé
                          data_plot <- data.frame(Feature = features, Indicateur = indicateurs_vecteur)
                          
                          # Trier le data frame par l'indicateur de manière décroissante
                          data_plot <- data_plot[order(-data_plot$Indicateur), ]
                          
                          # Tracé du graphique en barre
                          barplot_data <- barplot(data_plot$Indicateur, names.arg = data_plot$Feature, horiz = FALSE,
                                                  col = "lightblue", main = "Indicateur d'importance des variables",
                                                  cex.names = 0.7, xlab = "Variables", ylab = "Somme des écarts-types", ylim = c(0, max(data_plot$Indicateur)+0.5))
                          
                          # Ajout d'étiquettes de valeurs
                          text(x = barplot_data, y = data_plot$Indicateur, label = round(data_plot$Indicateur, 2),
                               pos = 3, cex = 0.7, col = "black")
                        },
                        
                        predict = function(new_data) {
                          # Extraire les probabilités a priori et conditionnelles du modèle
                          prior_prob <- private$prior_prob
                          cond_probs <- private$cond_probs
                          
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
                          prior_prob <- private$prior_prob
                          cond_probs <- private$cond_probs
                          
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
                        },
                        print=function(){
                          print(private$nb_variables)
                          sortie=paste("Le Model sur le quel l'apprentissage a été réalisé est un Naive Bayes Catégorielle. l'apprentissage a été réalisé sur",
                                     private$nb_variables,
                                     "variables et sur un enssemble de",
                                     private$nb_data_train,
                                     "Données d'entrainement")
                          print(sortie)
                        },
                        
                        summary=function(){
                          print(paste("Votre varriabla à prédire a ", length(private$nb_out_classe)," classes"))

                          df=rbind(private$min_parc_df, private$max_parc_df, private$nb_valu)
                          row.names(df) <- c("min","max", "Nombre de valeurs")
                          print(df)
                          
                        }
                      ),
                      private = list(
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
                        
                        
                        dis = function(X){
                          
                          mini=min(X)
                          print(mini)
                          maxi=max(X)
                          print (maxi)
                          inter=(maxi-mini)/self$nb_classe
                          points_de_coupure <- seq(from = mini, to = maxi, by = inter)
                          
                          disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
                          return(disc)
                        },
                        
                        gen_disc = function(X){
                          X <- data.frame(apply(X, MARGIN = 2, FUN = function(i) self$dis(i,private$nb_classe)))
                          return(X)
                        },
                        
                        etu_data = function(X,y){
                          print("on est dans etudata")
                          private$nb_variables = ncol(X)
                          private$nb_data_train = nrow(X)
                          private$nb_out_classe = unique(y)
                          private$min_parc_df = sapply(X, min)
                          private$max_parc_df = sapply(X, max)
                        },
                        
                        compt_val = function(X){
                          private$nb_valu <- sapply(X, function(col) length(unique(col)))
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

print(X)

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
print(cat("Probabilités:\n",probabilities))
#print(probabilities)

nb_model$summary()
nb_model$print()

nb_model$compute_and_plot_importance()