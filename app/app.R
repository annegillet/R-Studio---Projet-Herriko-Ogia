#install.packages("leaflet", dependencies = TRUE)
#install.packages("wesanderson")

library(shiny)
library(dplyr)
library(DT)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(wesanderson)

inventaire_boulangerie <- read.csv("inventaire_boulangerie.csv", sep = ",", header = TRUE)

#Préparation des données pour la visualisation graphique
data_graph <- read.csv("donnee_herriko_ogia.csv", sep = ",", header = TRUE)


#Formater la date
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# which fields get saved 
fieldsAll <- c("nom", "annee", "mois", "qte_farine_g")


# which fields are mandatory
fieldsMandatory <- c("nom", "annee", "mois", "qte_farine_g")

# add an asterisk to an input label
 labelMandatory <- function(label) {
     tagList(
         label,
         span("*", class = "mandatory_star")
     )
 }

# get current Epoch time
epochTime <- function() {
    return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
    format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
     fileName <- sprintf("%s_%s.csv",
                         humanTime(),
                         digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
    files <- list.files(file.path(responsesDir), full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    #data <- dplyr::rbind_all(data)
    data <- do.call(rbind, data)
    data
}

# directory where responses get stored
responsesDir <- file.path("donnees")

# CSS to use in the app
appCSS <-
    ".mandatory_star { color: red; }
    .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

shinyApp(
    ui = navbarPage(
        
        title = "HERRIKO OGIA",
        
        theme = shinytheme('flatly'),
        
        tabPanel("Données à saisir",
                 
                 shinyjs::useShinyjs(),
                 shinyjs::inlineCSS(appCSS),
                 
        div(id = "header",
            h3("Saisie des données"),
            
        fluidRow(
            column(4,
                   div(
                       id = "form",
                       
                       selectInput("nom", labelMandatory("Choisir sa boulangerie"), 
                                   c(as.character(inventaire_boulangerie$nom_boulangerie))),
                       selectInput("annee", labelMandatory("Année"), c("2019", "2020")),
                       selectInput("mois", labelMandatory("Mois"),
                                 c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
                       textInput("qte_farine_g", labelMandatory("Quantité de farine utilisée (g)"), ""),
                       actionButton("submit", "Valider", class = "btn-primary"),
                       actionButton("erase", "Effacer", class = "btn-primary"),
                       
                       shinyjs::hidden(
                           span(id = "submit_msg", "Chargement..."),
                           div(id = "error",
                               div(br(), tags$b("Error: "), span(id = "error_msg"))
                           )
                       )
                   ),
                   
                   shinyjs::hidden(
                       div(
                           id = "thankyou_msg",
                           h4("Merci, votre réponse a bien été validée !"),
                           actionLink("submit_another", "Entrer de nouvelles données")
                       )
                   )
            ),
            column(8,
                   uiOutput("adminPanelContainer")
            )
        )
        )),
        
        tabPanel("Empreinte blé",
                 h3("Suivi de la consommation"),
                 
                 sidebarPanel(
                   
                   helpText(h3("Choisissez vos variables")),
                   
                    selectInput("annee2",
                               "Année",
                               choices = as.numeric(unique(data_graph$annee))),
                              
                    selectInput("indicator", "Type d'indicateur",
                              choices = c("qte_farine_g", "qte_ble_m2", "cotis_var"))
                  ),
                 
                 mainPanel(
                   plotOutput("plot"))),
        
          tabPanel("Cartographie",
                   
                   h3("Identification de la production de blé"),
          
          #App mainPanel content and styles
          mainPanel(fluidRow(leafletOutput(outputId = "map")))
          )

    ),
    

    server = function(input, output, session) {
      
      # ---------------- Premier onglet : Saisie des données
        
        # Enable the Submit button when all mandatory fields are filled out
        observe({
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        
        # Gather all the form inputs
      
        formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, 
                      qte_ble_m2 = as.numeric(input$qte_farine_g) * 0.0018,
                      cotis_var = as.numeric(input$qte_farine_g) * 0.05,
                      ville = as.character(inventaire_boulangerie[inventaire_boulangerie$nom_boulangerie == input$nom, 4]),
                      latitude = as.character(inventaire_boulangerie[inventaire_boulangerie$nom_boulangerie == input$nom, 5]),
                      longitude = as.numeric(inventaire_boulangerie[inventaire_boulangerie$nom_boulangerie == input$nom, 6])) #timestamp = epochTime()
            data <- t(data)
            data
        })
        
        
        # When the Submit button is clicked, submit the response
        observeEvent(input$submit, {
            
            # User-experience stuff
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            # Save the data (show an error message in case of error)
            tryCatch({
                saveData(formData())
                shinyjs::reset("form")
                shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
        })
        
        # submit another response
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        })
        
        # render the admin panel
        output$adminPanelContainer <- renderUI({
            
            div(
                id = "adminPanel",
                downloadButton("downloadBtn", "Télécharger les données"), br(), br(),
                DT::dataTableOutput("responsesTable"), br()
            )
        })
      
        
        # Show the responses in the admin table
        output$responsesTable <- DT::renderDataTable({
            data <- loadData() 
            DT::datatable(
                data,
                rownames = FALSE,
                options = list(searching = FALSE, lengthChange = FALSE)
            )
        })
        
        # Allow user to download responses
        output$downloadBtn <- downloadHandler(
            filename = function() { 
                sprintf("donnee_herriko_ogia.csv", humanTime())
            },
            content = function(file) {
                write.csv(loadData(), file, row.names = FALSE)
            }
        )
        
        
        # -------- Deuxième onglet : Visualisation
        
        
         indicator_reactive <- reactive({
           switch(input$indicator,
                  "qte_farine_g" = data_graph$qte_farine_g,
                  "qte_ble_m2" = data_graph$qte_ble_m2,
                  "cotis_var" = data_graph$cotis_var)
         })
        
        output$plot <- renderPlot({
          data_plot <- data_graph %>%
             group_by(annee == input$annee2)
          
           ggplot(data = data_plot) +
             geom_bar(stat = 'identity', aes(x = as.character(mois), y = indicator_reactive(), fill = nom)) +
             scale_fill_brewer(palette = "BrBG") +
             geom_text(aes(x = as.character(mois), y = indicator_reactive(), fill = nom,
                           label = indicator_reactive()), position = position_stack(vjust = 0.5), color = "black", size = 4.5) +
             labs(x = "Mois", y = input$indicator, fill = "Boulangerie") + 
             theme_minimal()
          
        })
        
        # -------- Troisième onglet : Carte
        
        data_carte <- data_graph %>% 
          group_by(longitude, latitude, ville) %>% 
          summarise(total_ble_m2 = sum(qte_ble_m2))
        
        map <- leaflet(data_carte) %>% 
          addTiles() %>%
          addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                     radius = ~total_ble_m2, popup = ~paste(ville, ":", total_ble_m2),
                     color = "BrBG", fillOpacity = 0.5)

         output$map <- renderLeaflet(map)
        
    }
)

# Run the application 
#shinyApp(ui = ui, server = server)
