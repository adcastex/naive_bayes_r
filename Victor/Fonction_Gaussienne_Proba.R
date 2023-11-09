data("iris")
colnames(iris)[colnames(iris) == "Species"] <- "Class"



######################################################
# Fonction qui retourne la valeur proba gausienne   ##
######################################################
pdf_gaussian <- function(x, mean, std_dev) {
  exponent <- -((x - mean) ^ 2) / (2 * (std_dev ^ 2))
  coefficient <- 1 / (std_dev * sqrt(2 * pi))
  return(coefficient * exp(exponent))
}

######################################################
# Fonction pour calculer les probabilités de classe ##
######################################################
calculate_class_probabilities <- function(new_data, training_data) {
  # Estimation des moyennes et des écarts-types pour chaque classe
  means <- aggregate(. ~ Class, data = training_data, FUN = mean)
  std_devs <- aggregate(. ~ Class, data = training_data, FUN = sd)
  
  # Créer une matrice pour stocker les probabilités de classe
  class_probabilities <- matrix(0, nrow = nrow(new_data), ncol = length(unique(training_data$Class)))
  
  # Calcul des probabilités de chaque classe pour les nouvelles données
  for (i in 1:nrow(new_data)) {
    class_probabilities[i, ] <- sapply(unique(training_data$Class), function(class) {
      prob_class_given_data <- sum(training_data$Class == class) / nrow(training_data)
      prob_features_given_class <- 1  # Initialisation à 1
      for (j in 1:ncol(new_data)) {
        feature_name <- names(new_data)[j]
        prob_feature_given_class <- pdf_gaussian(new_data[i, feature_name], means[, feature_name][means$Class == class], std_devs[, feature_name][std_devs$Class == class])
        prob_features_given_class <- prob_features_given_class * prob_feature_given_class
      }
      prob_class_given_data * prob_features_given_class
    })
  }
  
  # Normalisation des probabilités
  class_probabilities <- class_probabilities / rowSums(class_probabilities)
  
  return(class_probabilities)
}

######################################################
# Fonction pour prédire la classe d'appartennace    ##
######################################################
predict_class <- function(class_probabilities, training_data) {
  predicted_classes <- apply(class_probabilities, 1, which.max)
  class_names <- unique(training_data$Class)
  predicted_classes <- class_names[predicted_classes]
  return(predicted_classes)
}

######################################################
#                 Phase de tests                    ##
######################################################
# Nouvelles données à classer
new_data <- data.frame(
  Sepal.Length = c(5.1, 6.0),
  Sepal.Width = c(3.5, 2.7),
  Petal.Length = c(1.4, 4.1),
  Petal.Width = c(0.2, 1.3)
)

# Appel de la fonction avec les nouvelles données
class_probabilities <- calculate_class_probabilities(new_data, iris)
print(class_probabilities)

# Appel de la fonction pour obtenir les classes prédites
predicted_classes <- predict_class(class_probabilities, iris)

# Affichage des classes prédites
print(predicted_classes)



