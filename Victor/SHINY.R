library(shiny)


# Définir l'interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Application Naive Bayes Classifier"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionnez le fichier CSV"),
      selectInput("target_variable", "Sélectionnez la variable cible", choices = NULL),
      actionButton("process_data", "Traiter les données")
    ),
    mainPanel(
      tableOutput("data_table")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    choices <- names(data())
    updateSelectInput(session, "target_variable", choices = choices)
  })
  
  output$data_table <- renderTable({
    if (input$process_data > 0) {
      # Utiliser votre fonction Naive Bayes Classifier ici
      # par exemple, si votre fonction s'appelle naive_bayes_classifier
      # result <- naive_bayes_classifier(data(), input$target_variable)
      # return(result)
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
