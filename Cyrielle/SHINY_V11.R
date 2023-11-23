library(shiny)
library(e1071)
library(R6)
library(shinythemes)
library(xlsx)

NaiveBayes <- R6Class("NaiveBayes",
                      public = list(
                        
                        ###Fonction -- Fit
                        fit = function(X, y, preproc = TRUE, nb_classe = 6, epsilon = NULL, g_na = TRUE) {
                          
                          #Interruption de la fonction si il y a des données manquantes dans la variable cible
                          
                          if(any(is.na(y))){
                            print("Function interrupted : there is missing data in the target variable ")
                            return(NULL)
                          }
                          
                          private$etu_data(X,y)
                          
                          
                          # Gestion des NA dans les variables prédictives
                          
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
                          
                          #Récupération du nombre de variable prédictives
                          private$compt_val(X)
                          
                          
                          ## Probabilités de chaques classes dans les données d'entraînement
                          prior_prob <- table(y) / nrow(X)
                          cond_probs <- list()
                          features <-names(X)
                          
                          # Ajout d'un epsilon pour ne pas avoir de proba conditionnelle à 0 et ne pas biaiser les résultats des probas postérieures
                          if (!is.null(epsilon)) {
                            private$epsilon <- epsilon
                          }
                          
                          ## Probabilités conditionnelles
                          
                          cl <- makeCluster(detectCores() - 1)  # On détecte le nombre de coeurs
                          registerDoParallel(cl)
                          
                          
                          # On applique à chaque valeur de la classe des données d'entraînement une fonction parallèle
                          
                          cond_probs <- foreach(i = 1:ncol(X)) %dopar% {
                            cross_table <- table(X[, i], y) + private$epsilon
                            t(prop.table(cross_table, margin = 2))
                          }
                          
                          stopCluster(cl)
                          
                          # On affecte les valeurs à nos attributs
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
                          
                          cl <- makeCluster(detectCores() - 1)  # On détecte le nombre de coeurs et on fait un cluster
                          registerDoParallel(cl)
                          
                          
                          # Parcourir les matrices de probabilités conditionnelles
                          deviations <- foreach(i = seq_along(cond_probs)) %dopar% {
                            # Récupérer la matrice de probabilités conditionnelles
                            cond_prob_matrix <- cond_probs[[i]]
                            # Calculer l'écart-type pour chaque colonne (modalités)
                            deviation_matrix <- apply(cond_prob_matrix, 2, sd)
                            deviation_matrix
                          }
                          
                          # Arrêter le cluster parallèle
                          stopCluster(cl)
                          
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
                        
                        ###Fonction -- Compter le nombre de variable prédictives
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
                    HTML("<h4 style='text-align: left;'>Chargement du CSV</h4>"),
                    fileInput("file", "Sélectionnez le fichier CSV"),
                    selectInput("target_variable", "Sélectionnez la variable cible", choices = NULL),
                    selectInput("explanatory_variables", "Sélectionnez les variables explicatives", choices = NULL, multiple = TRUE),
                    selectInput("separator", "Séparateur", choices = c(",", ";"), selected = ","),
                    selectInput("decimal", "Décimal", choices = c(".", ","), selected = "."),
                    checkboxInput("header", "Le fichier CSV a un en-tête", value = TRUE),
                    HTML("<h4 style='text-align: left;'>Paramètres du modèle</h4>"),
                    checkboxInput("preproc", "Prétraiter les données", value = TRUE),
                    numericInput("epsilon", "Epsilon", value = 0.001, step = 0.001),
                    actionButton("process_data", "Traiter les données")
             ),
             column(width = 9,
                    HTML("<h3 style='text-align: left;'>Pré-Visualisation des données</h3>"),
                    tableOutput("raw_data_table"),
                    HTML("<h3 style='text-align: left;'>Accuracy</h3>"),
                    textOutput("accuracy_output"),
                    HTML("<h3 style='text-align: left;'>Sorties du modèle</h3>"),
                    verbatimTextOutput("model_output")
             )
           )
  ),
  
  tabPanel("Graphique Importance variable",
           plotOutput("importance_plot")
  ),
  
  tabPanel("Prediction avec un nouveau fichier",
           fluidRow(
             column(width = 3,
                    fileInput("new_file", "Sélectionnez le nouveau fichier CSV"),
                    selectInput("new_separator", "Séparateur", choices = c(",", ";"), selected = ","),
                    selectInput("new_decimal", "Décimal", choices = c(".", ","), selected = "."),
                    checkboxInput("new_header", "Le nouveau fichier CSV a un en-tête", value = TRUE),  
                    actionButton("predict_new_data", "Prédire les classes"),
                    textOutput("predict_end_message"),
             ),
             column(width = 4,
                    HTML("<h3 style='text-align: left;'>Nouvelles données</h3>"),
                    tableOutput("new_data_table"),
                    actionButton("export_button", "Exporter les données sous format Excel"),
                    textOutput("export_message"),
             )
           )
  ),
  tabPanel("Prédictions classe",
           tableOutput("data_table")
  ),
  
  tabPanel("Probabilité d'appartenance",
           tableOutput("predictions_table")
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
  
  observe({
    choices <- names(data())
    updateSelectInput(session, "explanatory_variables", choices = choices)
  })
  
  #Affiche le df data train
  output$raw_data_table <- renderTable({
    df <- head(data(), n = 7)
    return(df)
  }, include.rownames = FALSE)
  
  predictions <- reactiveVal(NULL)
  test_data <- reactiveVal(NULL)
  
  #Met en place le fit
  observe({
    if (input$process_data > 0) {
      n_rows <- nrow(data())
      train_index <- sample(1:n_rows, 0.7 * n_rows)
      test_index <- setdiff(1:n_rows, train_index)
      train_data <- data()[train_index, ]
      test_data <- data()[test_index, ]
      X_train <- train_data[, input$explanatory_variables]
      y_train <- train_data[, input$target_variable]
      X_test <- test_data[, input$explanatory_variables]
      y_test <- test_data[, input$target_variable]
      nb_model$fit(X_train, y_train, preproc = input$preproc, epsilon = input$epsilon)
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
      X_train <- train_data[, input$explanatory_variables]
      y_train <- train_data[, input$target_variable]
      X_test <- test_data[, input$explanatory_variables]
      y_test <- test_data[, input$target_variable]
      predictions <- nb_model$predict(X_test)
      comparison_result <- y_test == predictions
      count_identical <- sum(comparison_result)
      accuracy <- sum(count_identical) / length(y_test)
      return(accuracy)
    }
  })
  
  # Affiche le résultat de la fonction print du modèle
  output$model_output <- renderPrint({
    if (input$process_data>0) {
      nb_model$Summary()
      nb_model$Print()
    }
  })
  
  #Predict
  output$data_table <- renderTable({
    if (input$predict_new_data > 0) {
      new_data_predictions <- nb_model$predict(new_data()[, input$explanatory_variables])
      combined_data <- cbind(new_data(), Prediction = new_data_predictions)
      return(combined_data)
    }
  })
  
  #Affichage la predictions des nouvelles données
  output$predictions_table <- renderTable({
    if (input$predict_new_data > 0) {
      probabilities <- nb_model$predict_proba(new_data()[, input$explanatory_variables])
      return(probabilities)
    }
  })
  
  # Gestion de l'export Excel
  observeEvent(input$export_button, {
    new_data_predictions <- nb_model$predict(new_data()[, input$explanatory_variables])
    combined_data <- cbind(new_data(), Prediction = new_data_predictions)
    current_directory <- normalizePath(getwd())
    filename <- "exported_data.xlsx"
    full_path <- paste0(current_directory, "\\", filename)
    print(full_path)
    write.xlsx(combined_data,full_path, row.names = FALSE)
    # Mettez à jour le message d'exportation
    output$export_message <- renderText({
      paste("Les données ont été exportées avec succès ici:", full_path)
    })
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
  
  
  
  
  #Affiche le df new_data
  output$new_data_table <- renderTable({
    new_data_table <- head(new_data(), n = 7)
    return(new_data_table)
  })
}



# Lancer l'application Shiny
shinyApp(ui, server)
