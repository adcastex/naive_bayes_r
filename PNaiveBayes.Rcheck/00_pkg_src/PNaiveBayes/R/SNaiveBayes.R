library(R6)

NaiveBayes <- R6Class("NaiveBayes",
                      public = list(

                        ###Fonction -- Fit
                        fit = function(X, y, preproc = TRUE, nb_classe = 6, epsilon = NULL, g_na = TRUE) {
                          #Arreter le fit si il y a des na dans les y
                          if(any(is.na(y))){
                            print("Il y a des NA dans le y l'entainement n'a pas été lancé")
                            return(NULL)
                          }

                          private$etu_data(X,y)


                          # Gestion des NA dans le X
                          if(g_na && any(is.na(X))){
                            type_col = lapply(X, class)
                            X = private$rem_na(X,type_col)
                          }


                          #Si l'utilisateur n'indique pas que ses données sont déja pretraitées alors preproc
                          if (preproc==TRUE) {
                            private$preproc = preproc
                            private$nb_classe = nb_classe
                            X = private$gen_disc(X,"fit")

                          }

                          # A voir pour faire une petite Etude de données ici

                          private$compt_val(X)


                          # Probabilités de chaques classes des données d'entraînement
                          prior_prob <- table(y) / nrow(X)
                          cond_probs <- list()
                          features <-names(X)

                          # Ajout d'un epsilon = ne pas avoir de proba conditionnelle = 0 et ne pas biaiser les résultats des probas postérieures
                          if (!is.null(epsilon)) {
                            private$epsilon <- epsilon
                          }

                          # Probabilités conditionnelles
                          # On applique à chaque valeur de la classe des données d'entraînement une fonction
                          for (i in 1:ncol(X)) {
                            cond_probs[[i]] <- matrix(0, nrow = length(levels(X[, i])), ncol = length(unique(y)), dimnames = list(levels(X[, i]), levels(y)))

                            cross_table <- table(X[, i], y) + private$epsilon

                            # Calculer les probabilités conditionnelles
                            cond_probs[[i]] <- t(prop.table(cross_table, margin = 2))
                          }

                          # On donne les valeurs à nos attributs
                          private$prior_prob <- prior_prob
                          private$cond_probs <- cond_probs
                          private$features<-features
                          return(self)
                        },

                        ###Fonction -- Graphique Importance variable
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

                        ###Fonction -- Predict classe appartenance
                        predict = function(new_data) {
                          #Si l'utilisateur n'indique pas que ses données sont déja pretraitées alors preproc
                          if (private$preproc==TRUE) {
                            new_data = private$gen_disc(new_data,"pred")
                          }


                          # Extraire les probabilités a priori et conditionnelles du modèle
                          prior_prob <- private$prior_prob
                          cond_probs <- private$cond_probs
                          # Initialiser un vecteur pour stocker les prédictions
                          predictions <- vector("character", length = nrow(new_data))

                          # Pour chaque individu dans les nouvelles données
                          for (i in 1:nrow(new_data)) {
                            # Initialiser les probabilités postérieures pour chaque classe
                            posterior_probs <- rep(1, length(prior_prob))
                            for (j in 1:ncol(new_data)) {
                              # Mettre à jour les probabilités postérieures en fonction des probabilités conditionnelles
                              feature_values <- as.character(new_data[i, j])
                              if (!is.null(cond_probs[[j]]) && feature_values %in% colnames(cond_probs[[j]])) {
                                posterior_probs <- posterior_probs * cond_probs[[j]][, feature_values]
                              } else {
                                posterior_probs <- posterior_probs * private$epsilon
                              }

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

                        ###Fonction -- Predict Proba appartenance
                        predict_proba = function(new_data) {

                          if (private$preproc==TRUE) {
                            new_data = private$gen_disc(new_data,"pred")
                          }


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
                              feature_values <- as.character(new_data[i, j])

                              if (!is.null(cond_probs[[j]]) && feature_values %in% colnames(cond_probs[[j]])) {
                                posterior_probs <- posterior_probs * cond_probs[[j]][, as.character(new_data[i, j])]
                              } else {
                                posterior_probs <- posterior_probs * private$epsilon
                              }
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

                        ###Fonction -- Print
                        Print=function(){
                          sortie=paste("Le modèle sur lequel l'apprentissage a été réalisé est un Naive Bayes Catégorielle. l'apprentissage a été réalisé sur",
                                       private$nb_variables,
                                       "variables et sur un ensemble de",
                                       private$nb_data_train,
                                       "Données d'entrainement")
                          print(sortie)
                        },

                        ###Fonction -- Summary
                        Summary=function(){
                          print(paste("Votre variable à prédire possède ", length(private$nb_out_classe)," classes"))

                          df=rbind(private$min_parc_df, private$max_parc_df, private$nb_valu)
                          row.names(df) <- c("min","max", "Nombre de valeurs")
                          print(df)

                        }
                      ),

                      ###Constructeur
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
                        epsilon = 0.001,
                        list_remp_NA=list(),
                        med = NULL,
                        classe_maj = NULL,

                        matrice_param_preproc = NULL,

                        # remplacement des NA
                        rem_na = function(X,liste_class ){
                          for (i in 1:length(liste_class)){
                            if(liste_class[i]=="numeric"){


                              private$list_remp_NA = append(private$list_remp_NA,round(mean(X[,i], na.rm = TRUE),digits = 2))
                              liste=X[,i]


                              liste[is.na(liste)] = private$list_remp_NA[i]

                              liste=as.double(liste)

                              X[,i] = liste

                            }else{
                              liste=X[,i]
                              table_occurrences <- table(X[,i])
                              classe_majoritaire <- names(table_occurrences)[which.max(table_occurrences)]
                              private$list_remp_NA = append(private$list_remp_NA,classe_majoritaire)
                              liste[is.na(liste)] = private$list_remp_NA[i]
                              X[,i]=liste
                            }
                          }

                          return(X)
                        },

                        ###Fonction -- Discrétisation
                        dis = function(X, col, place){

                          if (place=="fit"){
                            if(class(X)=="numeric"){
                              mini=min(X, na.rm = TRUE)
                              private$matrice_param_preproc[1,col] = mini

                              maxi=max(X, na.rm = TRUE)
                              private$matrice_param_preproc[2,col] = maxi

                              inter=(maxi-mini)/private$nb_classe
                              private$matrice_param_preproc[3,col] = inter

                              points_de_coupure <- seq(from = mini, to = maxi, by = inter)

                              disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
                              return(disc)

                            }else{
                              return(X)
                            }

                          }else{
                            if(class(X)=="numeric"){
                              mini = private$matrice_param_preproc[1,col]
                              if(min(X, na.rm = TRUE) < mini){
                                mini = min(X, na.rm = TRUE)
                              }
                              maxi = private$matrice_param_preproc[2,col]
                              if(max(X, na.rm = TRUE) > maxi){
                                maxi = max(X, na.rm = TRUE)
                              }

                              inter = private$matrice_param_preproc[3,col]

                              points_de_coupure <- seq(from = mini, to = maxi, by = inter)
                              disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)

                              return(disc)
                            }else{
                              return(X)
                            }
                          }
                        },

                        #Fonction -- Application discrétisation
                        gen_disc = function(X, place){
                          if(place=="fit"){
                            private$matrice_param_preproc = matrix(0, nrow = 3, ncol = length(X))
                          }
                          for (i in 1:length(X)){
                            X[,i] <- private$dis(X[,i], i, place)
                          }

                          return(X)
                        },

                        ###Fonction -- Stats Desc
                        etu_data = function(X,y){
                          private$nb_variables = ncol(X)
                          private$nb_data_train = nrow(X)
                          private$nb_out_classe = unique(y)
                          private$min_parc_df = sapply(X, min)
                          private$max_parc_df = sapply(X, max)
                        },

                        ###Fonction -- Compte le nombre de variable
                        compt_val = function(X){
                          private$nb_valu <- sapply(X, function(col) length(unique(col)))
                        }
                      )
)
