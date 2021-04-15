my_packages <- c("shinyWidgets","utils","tidyverse","reshape2","matlab","Hmisc")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])] 
if(length(not_installed)) install.packages(not_installed) 

source("functions.R", encoding = "UTF-8", local=TRUE)
source("fct_BA_App.R", encoding = "UTF-8", local=TRUE)
source("fct_Organe_App.R", encoding = "UTF-8", local=TRUE)
source("fct_ARS_App.R", encoding = "UTF-8", local=TRUE)

library(shiny)
library(shinyWidgets)

######################################### APP #######################################
## User Interface

ui <- fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    div(class = "input",
      h1("Données d'entrée"),
      
      div(class="row_1",
        h2("Téléchargement des fichiers"),
        fluidRow(
          column(3,h4("> CHMS")),
          column(1,
            dropdown(
              width = 900,
              fluidRow(
                column(3,
                       div(class="fInput",fileInput("CHMS_Tho","Thoracique",multiple = FALSE, accept = c(".csv"))),
                       div(class="fInput",fileInput("CHMS_Gen","Générale",multiple = FALSE, accept = c(".csv")))
                ),
                column(3,
                       div(class="fInput",fileInput("CHMS_Uro","Urologie",multiple = FALSE, accept = c(".csv"))),
                       div(class="fInput",fileInput("CHMS_Dig","Digestif",multiple = FALSE, accept = c(".csv")))
                ),
                column(3,
                       div(class="fInput",fileInput("CHMS_Gyn","Gynécologie",multiple = FALSE, accept = c(".csv"))),
                       div(class="fInput",fileInput("CHMS_End","Endocrines",multiple = FALSE, accept = c(".csv")))
                ),
                column(3,
                       div(class="fInput",fileInput("CHMS_ORL","ORL",multiple = FALSE, accept = c(".csv"))),
                       div(class="fInput",fileInput("CHMS_Hem","Hématologie",multiple = FALSE, accept = c(".csv")))
                )
              )
            )
          )
        ),
        fluidRow(
          column(3,h4("> MEDIPOLE")),
          column(1,
            dropdown(
              width = 900,
              fluidRow(
                column(3,
                       div(class="fInput",fileInput("Med_Dig","Digestif",multiple = FALSE, accept = c(".csv")))   
                 ),
                column(3,
                       div(class="fInput",fileInput("Med_Gyn","Gynécologie",multiple = FALSE, accept = c(".csv")))
                ),
                column(3,
                       div(class="fInput",fileInput("Med_Uro","Urologie",multiple = FALSE, accept = c(".csv")))
                )
              )
            )
          )
        )
      ),
      div(class="row_1",
        h2("Données supplémentaires"),
        fluidRow(
          column(4,
            div(class="input_1",numericInput("Annee","Année",2020,2017,2050,width='50%'))
          ),
          column(4,
            div(class="input_1",selectInput("Choix","Rapport cible",
              choices = list("Bilan Annuel 3C"=1, "Bilan Organes 3C"=2, "Bilan Annuel ARS"=3),selected=1)) 
          )
        )
      )
    ),
    div(class="output",
      fluidRow(
        column(4),
        div(class="titleSortie",column(4,h1("Données de sortie"))),
        column(4,downloadButton("TelechargementBilan","Télécharger le bilan"))
      ),
      div(class="tOutput",
        tableOutput("header"),
        div(class="dOutput",
          tableOutput("TdB"),
          tableOutput("Bilan")
        )
      )
    )
)

## Server

server <- function(input, output){
  
  showModal(modalDialog(title = "OUTIL RCP-3C","MESSAGES IMPORTANTS : (1) L'outil accepte uniquement des fichiers '.csv' (2) veuillez indiquer l'année EXACTE dont les fichiers sont issus afin d'éviter les bugs de l'outil et les erreurs sur les indicateurs",footer = modalButton("OK")))
  
  dataTdB <- reactive({
    req(input$CHMS_Tho, input$CHMS_Uro, input$CHMS_Gyn, input$CHMS_ORL, input$CHMS_Gen, input$CHMS_Dig, input$CHMS_End, input$CHMS_Hem, input$Med_Dig, input$Med_Gyn, input$Med_Uro)
    tryCatch(
      {
        CHMS_Tho <- open_CSVfile(input$CHMS_Tho$datapath)
        CHMS_Uro <- open_CSVfile(input$CHMS_Uro$datapath)
        CHMS_Gyn <- open_CSVfile(input$CHMS_Gyn$datapath)
        CHMS_ORL <- open_CSVfile(input$CHMS_ORL$datapath)
        CHMS_Gen <- open_CSVfile(input$CHMS_Gen$datapath)
        CHMS_Dig <- open_CSVfile(input$CHMS_Dig$datapath)
        CHMS_End <- open_CSVfile(input$CHMS_End$datapath)
        CHMS_Hem <- open_CSVfile(input$CHMS_Hem$datapath)
        Med_Dig <- open_CSVfile(input$Med_Dig$datapath)
        Med_Gyn <- open_CSVfile(input$Med_Gyn$datapath)
        Med_Uro <- open_CSVfile(input$Med_Uro$datapath)

        liste_fichier <- list(CHMS_Tho,CHMS_Uro,CHMS_Gyn,CHMS_ORL,CHMS_Gen,CHMS_Dig,CHMS_End,CHMS_Hem,Med_Dig,Med_Gyn,Med_Uro)
        names(liste_fichier) <- c('CHMS_Tho','CHMS_Uro','CHMS_Gyn','CHMS_ORL','CHMS_Gen','CHMS_Dig','CHMS_End','CHMS_Hem','Med_Dig','Med_Gyn','Med_Uro')

        tIndic <- switch(input$Choix,
            "1" = data_BA(liste_fichier,input),
            "2" = data_Organe(liste_fichier),
            "3" = data_ARS(liste_fichier,input)
        )

      },
      error = function(e){
        stop(showModal(modalDialog("Une erreur empêche l'affichage des données, vérifiez l'extension des fichiers (csv accepté), l'année indiquée et le fichier chargé sous la spécialité Hématologie",footer = modalButton("OK"))))
      },
      warning = function(w){
        stop(showModal(modalDialog("Une erreur empêche l'affichage des données, vérifiez l'extension des fichiers (csv accepté), l'année indiquée et le fichier chargé sous la spécialité Hématologie",footer = modalButton("OK"))))
      }
    )
    tIndic
  })

  output$Bilan <- renderTable({
    if(input$Choix != "1"){
      if(input$Choix != "2"){
        dataTdB()
      }
      else{
        data.frame(t(gsub("\n","<br>",unlist(dataTdB()))),row.names = "Détail par organe")
      }
    }
    else{
      NULL
    }
  },
  rownames = TRUE,
  sanitize.text.function=identity
  )
  
  output$TdB <- renderTable({
    if(input$Choix == "1"){
      dataTdB()$data
    }
    else{
      NULL
    }
  },
  rownames = TRUE,
  colnames = FALSE,
  sanitize.text.function=identity
  )
  
  output$header <- renderTable({
    if(input$Choix == "1"){
      data.frame(t(dataTdB()$colnames))
    }
    else{
      NULL
    }
  },
  rownames = FALSE,
  colnames = FALSE
  )

  output$TelechargementBilan <- downloadHandler(
    filename = function(){"TelechargementBilan.csv"},
    content = function(file){
      if(input$Choix == "1"){
        write.csv2(dataTdB()$data,file,row.names=TRUE)
      }
      else{
        write.csv2(dataTdB(),file,row.names=TRUE)
      }
    }
  )
}

shinyApp(ui, server)
