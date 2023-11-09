data("iris")

colnames(iris)[colnames(iris) == "Species"] <- "Class"



library(R6)


MaClasse <- R6Class(
  "MaClasse",
  public = list(
    valeur = NULL,  # Propriété de la classe
    initialize = function(valeur_initiale) {
      self$valeur <- valeur_initiale  # Initialisation de la propriété
    },
    afficher_valeur = function() {
      cat("La valeur est :", self$valeur, "\n")
    }
  )
)

# Créer une instance de la classe
mon_objet <- MaClasse$new(valeur_initiale = 42)

# Appeler la méthode pour afficher la valeur
mon_objet$afficher_valeur()  # Cela affichera "La valeur est : 42"