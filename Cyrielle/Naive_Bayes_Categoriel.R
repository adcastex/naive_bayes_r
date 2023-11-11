############## CREATION DATA FRAME DE TEST ############## 


data <- data.frame(
  Feature1 = c("A", "B", "C","B","C","C"),
  Feature2 = c("X", "X", "Z","Y","Z","Z"),
  Class = c("Positive", "Negative","Positive","Positive","Negative","Positive")
)


X=data[, c("Feature1", "Feature2")]
y=data$Class

############## FONCITON FIT -> CALCULS DES PROBAS A PRIORI ET CONDITIONNELLES ############## 


fit_categorial_naive_bayes <- function(X, y) {
  
  # Probabilités a priori de chaque classe des données d'entraînement
  prior_prob <- table(y) / nrow(X)
  cond_probs<-list()
  # Probabilités conditionnelles
  # On applique à chaque valeur de la classe des données d'entraînement une fonction
  for(i in 1:ncol(X)){
    cond_probs[[i]] <- matrix(0, nrow = length(levels(X[,i])), ncol = length(unique(y)), dimnames = list(levels(X[,i]), levels(y)))
    
    #Ajout d'un epsilon = ne pas avoir de proba conditionnelle = 0 et ne pas biaiser les résultats des probas postérieures
    epsilon<-0.0001
    cross_table <- table(X[,i], y)+epsilon
    
    # Calculer les probabilités conditionnelles
    cond_probs[[i]] <- t(prop.table(cross_table, margin = 2))
    
    }
  
  return(list(y=levels(as.factor(y)),prior_prob = prior_prob, cond_probs = cond_probs))
}



############## TEST FONCTION FIT SUR LES DONNEES D'EXEMPLE ############## 
#On fit notre modèle sur les données X et y 
model=fit_categorial_naive_bayes(X,y)

print(model)

# Exemple d'utilisation
new_data <- data.frame(
  Feature1 = c("A", "B", "C"),
  Feature2 = c("X", "Y", "Z")
)


############## FONCTION PREDICT -> RENVOIE LES CLASSES PREDITES ############## 

#On fait une fonction qui renvoie les classes prédites pour nos nouveaux individus 
predict_categorial_naive_bayes <- function(model, new_data) {
  # Extraire les probabilités a priori et conditionnelles du modèle
  prior_prob <-model$prior_prob
  cond_probs <- model$cond_probs
  
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
      posterior_probs <- posterior_probs * cond_probs[[j]][,feature_values]
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
}



############## TEST FONCTION PREDICT SUR NOUVELLES DONNEES TEST ############## 

predictions <- predict_categorial_naive_bayes(model, new_data)
cat("Prédictions:", predictions, "\n")


############## FONCTION PREDICT_PROBA SUR NOUVELLES DONNEES TEST ##############

#On fait la fonction qui renvoie les probabilités d'appartenance pour chaque classe des nouveaux individus

predict_proba_categorial_naive_bayes <- function(model, new_data) {
  # Extraire les probabilités a priori et conditionnelles du modèle
  prior_prob <- model$prior_prob
  cond_probs <- model$cond_probs
  
  # Initialiser une matrice pour stocker les probabilités pour chaque classe
  probabilities <- matrix(0, nrow = nrow(new_data), ncol = length(prior_prob), dimnames = list(NULL, names(prior_prob)))
  
  # Pour chaque individu dans les nouvelles données
  for (i in 1:nrow(new_data)) {
    # Initialiser les probabilités postérieures pour chaque classe
    posterior_probs <- rep(1, length(prior_prob))
    
    # Pour chaque caractéristique (colonne) dans les nouvelles données
    for (j in 1:ncol(new_data)) {
      # Mettez à jour les probabilités postérieures en fonction des probabilités conditionnelles
      posterior_probs <- posterior_probs * cond_probs[[j]][,as.character(new_data[i, j])]
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






############## TEST FONCTION PREDICT_PROBA SUR NOUVELLES DONNEES TEST ##############
probabilities <- predict_proba_categorial_naive_bayes(model, new_data)
cat("Probabilités:\n")
print(probabilities)


############## TEST AVEC NAIVE_BAYES DE R ##############

library(e1071)



# Créer le modèle naive bayes
model_r <- naiveBayes(y ~ ., data = X)

# Prédictions avec le modèle R
predictions_r <- predict(model_r, newdata = new_data, type = "class")
probabilities_r <- predict(model_r, newdata = new_data, type = "raw")


############## COMPARAISON AVEC R ##############

print(probabilities)
print(probabilities_r)
