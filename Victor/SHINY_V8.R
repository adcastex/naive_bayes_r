library(shiny)
library(e1071)
library(R6)
library(shinythemes)
### CLASSE NB ####
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
                            print(paste("les type des col", type_col, "je suis dans le gestion NA"))
                            X = private$rem_na(X,type_col)
                            
                          }
                          
                          
                          #Si l'utilisateur n'indique pas que ses données sont déja pretraitées alors preproc
                          if (preproc==TRUE) {
                            private$preproc = preproc
                            private$nb_classe = nb_classe
                            X = private$gen_disc(X)
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
                        predict = function(new_data,preproc=TRUE,nb_classe = 6) {
                          #Si l'utilisateur n'indique pas que ses données sont déja pretraitées alors preproc
                          if (preproc==TRUE) {
                            private$preproc = preproc
                            private$nb_classe = nb_classe
                            new_data = private$gen_disc(new_data)
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
                        predict_proba = function(new_data,preproc=TRUE,nb_classe = 6) {
                          
                          if (preproc==TRUE) {
                            private$preproc = preproc
                            private$nb_classe = nb_classe
                            new_data = private$gen_disc(new_data)
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
                        print=function(){
                          sortie=paste("Le modèle sur lequel l'apprentissage a été réalisé est un Naive Bayes Catégorielle. l'apprentissage a été réalisé sur",
                                       private$nb_variables,
                                       "variables et sur un ensemble de",
                                       private$nb_data_train,
                                       "Données d'entrainement")
                        },
                        
                        ###Fonction -- Summary
                        summary=function(){
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
                        dis = function(X){
                          if(class(X)=="numeric"){
                            mini=min(X)
                            maxi=max(X)
                            inter=(maxi-mini)/private$nb_classe
                            points_de_coupure <- seq(from = mini, to = maxi, by = inter)
                            
                            disc <- cut(X, breaks = points_de_coupure, labels = FALSE, include.lowest=TRUE)
                            return(disc)
                          }
                          
                        },
                        
                        #Fonction -- Application discrétisation
                        gen_disc = function(X){
                          X <- data.frame(apply(X, MARGIN = 2, FUN = function(i) private$dis(i)))
                          return(X)
                        },
                        
                        ###Fonction -- Stats Desc
                        etu_data = function(X,y){
                          print("Stats descriptives (à garder ?)")
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

############# PREPROCESSING FUNCTION ###################
################ UTILISER LA CLASSE  ##########


### SHIny #####
nb_model <- NaiveBayes$new()
set.seed(42)
# Définir l'interface utilisateur (UI)
ui <- navbarPage(
  theme = shinytheme("yeti"),
  title = "Application Naive Bayes Classifier",
  tabPanel("Entraîner le modèle",
           fluidRow(
             column(width = 3,
                    fileInput("file", "Sélectionnez le fichier CSV"),
                    selectInput("target_variable", "Sélectionnez la variable cible", choices = NULL),
                    selectInput("separator", "Séparateur", choices = c(",", ";"), selected = ","),
                    selectInput("decimal", "Décimal", choices = c(".", ","), selected = "."),
                    checkboxInput("header", "Le fichier CSV a un en-tête", value = TRUE),  
                    actionButton("process_data", "Traiter les données")
             ),
             column(width = 9,
                    HTML("<h3 style='text-align: left;'>Pré-Visualisation des données</h3>"),
                    tableOutput("raw_data_table"),
                    HTML("<h3 style='text-align: left;'>Résultats</h3>"),
                    h4("Accuracy"),
                    textOutput("accuracy_output"),  # Ajout de l'élément pour afficher l'accuracy
                    h4("Rappel"),
                    p("Le rappel du modèle est de n%"),
                    HTML("<h3 style='text-align: left;'>Sorties du modèle</h3>"),
                    verbatimTextOutput("model_output")
             )
           )
  ),
  
  tabPanel("Prediction avec un nouveau fichier",
           fluidRow(
             column(width = 3,
                    fileInput("new_file", "Sélectionnez le nouveau fichier CSV"),
                    selectInput("new_separator", "Séparateur", choices = c(",", ";"), selected = ","),
                    selectInput("new_decimal", "Décimal", choices = c(".", ","), selected = "."),
                    checkboxInput("new_header", "Le nouveau fichier CSV a un en-tête", value = TRUE),  
                    actionButton("predict_new_data", "Prédire les classes des nouvelles données")
             ),
             column(width = 4,
                    HTML("<h3 style='text-align: left;'>Nouvelles données</h3>"),
                    tableOutput("new_data_table"),
                    actionButton("export_button", "Exporter les données sous format Excel"),
             )
           )
  ),
  tabPanel("Prédictions classe",
           tableOutput("data_table")
  ),
  
  tabPanel("Probabilité d'appartenance",
           tableOutput("predictions_table")
  ),
  tabPanel("Graphique Importance variable",
           plotOutput("importance_plot")
  )
)



# Définir le serveur
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, sep = input$separator, dec = input$decimal, header = input$header)
  })
  
  observe({
    choices <- names(data())
    updateSelectInput(session, "target_variable", choices = choices)
  })
  
  #Affiche le df data train
  output$raw_data_table <- renderTable({
    df <- head(data(), n = 7)
    return(df)
  }, include.rownames = FALSE)
  
  predictions <- reactiveVal(NULL)
  test_data <- reactiveVal(NULL)
  
  observe({
    if (input$process_data > 0) {
      n_rows <- nrow(data())
      train_index <- sample(1:n_rows, 0.7 * n_rows)
      test_index <- setdiff(1:n_rows, train_index)
      train_data <- data()[train_index, ]
      test_data <- data()[test_index, ]
      X_train <- train_data[, setdiff(names(data()), input$target_variable)]
      y_train <- train_data[, input$target_variable]
      X_test <- test_data[, setdiff(names(data()), input$target_variable)]
      y_test <- test_data[, input$target_variable]
      nb_model$fit(X_train, y_train, preproc = TRUE)
    }
  })
  
  # Affiche l'accuracy
  output$accuracy_output <- renderText({
    if (input$process_data > 0) {
      #Données d'entrée
      n_rows <- nrow(data())
      train_index <- sample(1:n_rows, 0.7 * n_rows)
      test_index <- setdiff(1:n_rows, train_index)
      train_data <- data()[train_index, ]
      test_data <- data()[test_index, ]
      X_train <- train_data[, setdiff(names(data()), input$target_variable)]
      y_train <- train_data[, input$target_variable]
      X_test <- test_data[, setdiff(names(data()), input$target_variable)]
      y_test <- test_data[, input$target_variable]
      predictions <- nb_model$predict(X_test, preproc=T)
      comparison_result <- y_test == predictions
      count_identical <- sum(comparison_result)
      
      # Calcul de l'exactitude (accuracy)
      accuracy <- sum(count_identical) / length(y_test)
      return(accuracy)
    }
  })
  
  
  #Predict
  output$data_table <- renderTable({
    if (input$predict_new_data > 0) {
      new_data_predictions <- nb_model$predict(new_data(),preproc = T)
      combined_data <- cbind(new_data(), Prediction = new_data_predictions)
      return(combined_data)
    }
  })
  
  #Affichage la predictions des nouvelles données
  output$predictions_table <- renderTable({
    if (input$predict_new_data > 0) {
      probabilities <- nb_model$predict_proba(new_data())
      return(probabilities)
    }
  })
  
  # Gestion de l'export Excel
  observeEvent(input$export_button, {
    new_data_predictions <- nb_model$predict(new_data())
    combined_data <- cbind(new_data(), Prediction = new_data_predictions)
    current_directory <- normalizePath(getwd())
    filename <- "exported_data.xlsx"
    full_path <- paste0(current_directory, "\\", filename)
    print(full_path)
    write.xlsx(combined_data,full_path, row.names = FALSE)
    output$export_message <- renderText("Données exportées avec succès!")
  })
  
  
  #Plot importance variables
  output$importance_plot <- renderPlot({
    if (input$process_data > 0) {
      nb_model$compute_and_plot_importance()
    }
  })
  
  #Charge les nouvelles données
  new_data <- reactive({
    req(input$new_file)
    read.csv(input$new_file$datapath, sep = input$new_separator, dec = input$new_decimal, header = input$new_header)
  })
  
  observe({
    choices <- names(new_data())
    updateSelectInput(session, "new_data_target_variable", choices = choices)
  })
  
  #Affiche le df new_data
  output$new_data_table <- renderTable({
    if (input$predict_new_data > 0) {
      new_data_table <- head(new_data(), n = 7)
      return(new_data_table)
    }
  })
}



# Lancer l'application Shiny
shinyApp(ui, server)

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
