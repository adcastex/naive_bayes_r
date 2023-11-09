# Naive Bayes multinomial

#Cette variante est souvent utilisée pour la classification de documents ou de textes.
#Elle suppose que les caractéristiques (mots, termes, etc.) sont des variables aléatoires multinomiales, 
#c'est-à-dire qu'elles représentent des comptages discrets. 
#Cette approche est souvent utilisée pour la classification de texte et de documents, 
#notamment dans les tâches de catégorisation de textes et de détection de spam.


# Données d'exemple
data <- data.frame(
  Feature1 = c(1, 0, 1, 1, 0),
  Feature2 = c(0, 1, 1, 1, 0),
  Class = factor(c("Classe1", "Classe2", "Classe1", "Classe1", "Classe2"))
)

# Probabilités a priori
prior_prob <- table(data$Class) / nrow(data)

# Calcul des probabilités conditionnelles
conditional_prob <- lapply(unique(data$Class), function(class) {
  subset_data <- data[data$Class == class, ]
  sapply(names(subset_data)[1:(ncol(subset_data) - 1)], function(feature) {
    sum(subset_data[, feature]) / sum(subset_data[, feature], na.rm = TRUE)
  })
})

# Nouvelles données à classer
new_data <- data.frame(Feature1 = c(1, 0), Feature2 = c(0, 1))

# Calcul des probabilités de chaque classe pour les nouvelles données
class_probabilities <- sapply(unique(data$Class), function(class) {
  prob_class_given_data <- prior_prob[class]
  prod(sapply(names(new_data), function(feature) {
    conditional_prob_value <- conditional_prob[[class]][feature]
    if (is.na(conditional_prob_value) || conditional_prob_value == 0) {
      # Ajoute une petite valeur epsilon pour éviter une division par zéro
      epsilon <- 1e-10
      conditional_prob_value <- epsilon
    }
    new_data_value <- new_data[1, feature]  # Utilisez la première ligne de new_data ici
    prob_feature_given_class <- conditional_prob_value
    prob_class_given_data * prob_feature_given_class
  }))
})

# Normalisation des probabilités
class_probabilities <- class_probabilities / sum(class_probabilities)
print(class_probabilities)



#Naive Bayes gaussien :

#Dans cette variante, on suppose que les caractéristiques suivent une distribution gaussienne (normale) 
#dans chaque classe. 
#Cela signifie que les caractéristiques sont continues et suivent une distribution normale.
#Le Naive Bayes gaussien est adapté lorsque vos caractéristiques sont continues, 
#par exemple, dans la classification de données numériques, 
#telles que la classification des iris dans le jeu de données d'Iris.


set.seed(123)
data <- data.frame(
  Feature1 = rnorm(100, mean = 0, sd = 1),
  Feature2 = rnorm(100, mean = 2, sd = 1),
  Class = factor(sample(0:1, 100, replace = TRUE))
)

# Estimation des moyennes et des écarts-types pour chaque classe

#calcul des moyennes des caractéristiques (variables) en fonction de la variable de classe
means <- aggregate(. ~ Class, data = data, FUN = mean)

#calcul des écarts types des caractéristiques (variables) en fonction de la variable de classe
std_devs <- aggregate(. ~ Class, data = data, FUN = sd)

# Fonction pour calculer la densité de probabilité gaussienne
pdf_gaussian <- function(x, mean, std_dev) {
  exp(-0.5 * ((x - mean) / std_dev)^2) / (std_dev * sqrt(2 * pi))
}

# Nouvelles données à classer
new_data <- data.frame(
  Feature1 = c(1.5, 0.5),
  Feature2 = c(2.5, 3.0)
)

# Calcul des probabilités de chaque classe pour les nouvelles données
class_probabilities <- sapply(unique(data$Class), function(class) {
  prob_class_given_data <- sum(data$Class == class) / nrow(data)
  prob_feature1_given_class <- pdf_gaussian(new_data$Feature1, means$Feature1[means$Class == class], std_devs$Feature1[std_devs$Class == class])
  prob_feature2_given_class <- pdf_gaussian(new_data$Feature2, means$Feature2[means$Class == class], std_devs$Feature2[std_devs$Class == class])
  prob_class_given_data * prob_feature1_given_class * prob_feature2_given_class
})

# Normalisation des probabilités
class_probabilities <- class_probabilities / sum(class_probabilities)

print(class_probabilities)