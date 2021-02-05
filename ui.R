library(shiny)
library(PTXQC)
library(dplyr)
library(magrittr)
library(shinythemes)
library(shinyjs)
library(yaml)
library(waiter)
library(shinyBS)
library(htmlwidgets)


shinyUI(tagList(fluidPage(
  
  use_waiter(), 
  useShinyjs(),
  tags$script(src = "text.js"),
  
  tags$head(tags$script('var dimension = 0;
                        $(document).on("shiny:connected", function(e) {
                        dimension = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '),                                                                                                        
            tags$style(HTML("#shiny-notification-panel {
                            top: 110px;
                            bottom: unset;
                            right: 0;
                            left: 0;
                            margin-left: 34%;
                            margin-right: auto;
                            width: 100%;
                            max-width: 450px;
                            }")
                       )
            ),
  
  
  navbarPage(paste0("Proteomics Quality Control (Version ", packageVersion("PTXQC"), ")"), theme = shinytheme("flatly"),
    
    tabPanel("Report", fluid = TRUE, icon =icon("file-alt"),
      sidebarLayout(
      
        sidebarPanel(
          
          titlePanel(textOutput("title")),
          
          selectInput("dtype", "Data type",
                      c("MaxQuant directory", "MaxQuant files", "Mztab file")),
          
          conditionalPanel(condition = "input.dtype == 'MaxQuant directory'",
                           tags$div(class="form-group shiny-input-container", 
                                    tags$div(tags$label("Choose folder")),
                                    fluidRow(
                                      column(4, 
                                        tags$div(tags$label("Browse...", class="btn btn-primary", tags$input(id = "fileIn", webkitdirectory = TRUE, type = "file", style="display: none;", onchange="pressed()")))
                                        ),
                                      column(8,
                                        verbatimTextOutput("mqdir")
                                        )
                                    ),
                                    tags$div(id="fileIn_progress", class="progress progress-striped active shiny-file-input-progress",
                                             tags$div(class="progress-bar")
                                    )
                           )
                           
          ),
          conditionalPanel(condition = "input.dtype == 'MaxQuant files'",
                          fileInput("mqfiles", "Choose files", multiple = TRUE)
          ),
          conditionalPanel(condition = "input.dtype == 'Mztab file'",
                           fileInput("file", "Choose file", accept = ".mzTab")
          ),
          
          checkboxInput("showsets", "Adjust advanced settings"),
          
          conditionalPanel(condition = "input.showsets",
                           selectInput("settings", label = "How?", choices = c( "Change settings manually", "Upload yaml file")), 
                           conditionalPanel(condition = "input.showsets && input.settings == 'Upload yaml file'", 
                                            uiOutput("yaml.load")),
                           conditionalPanel(condition = "input.settings == 'Change settings manually'",
                                            
                                            tags$style(".popover{
                                                          max-width: 100%;
                                                       }"),
                                            uiOutput("adv.set1"),

                                            fluidRow(
                                              column(6, 
                                                     uiOutput("adv.set2")
                                                     ),
                                              column(6, 
                                                     uiOutput("adv.set3")
                                                     )
                                              
                                            ),
                                              
                                            checkboxGroupInput("metrics", "Compute metrics", choices = lst_qcMetrics_ord, selected = lst_qcMetrics_ord)
                           )
          ),
          
          

          
          br(),


          actionButton("creport", "Create report"),

          conditionalPanel("output.created", 

                br(),
                fluidRow(
                         column(6, align = "center", 
                                uiOutput("pdfd")
                                ), 
                         column(6, align = "center",
                                uiOutput("yamld")
                                )
                        ),
                
                br(),
                
                fluidRow(
                        column(6, align = "center",
                                uiOutput("htmld")
                        ),
                        column(6, align = "center", 
                               uiOutput("logs")
                        )
                ),
                
                br(),
                fluidRow(align = "center",
                         uiOutput("newreport")
                )
             
          ),
          
          
          br()
          
        ),
        mainPanel(
          
          conditionalPanel(condition = "input.creport == 1",
                           htmlOutput("htmlpage") 
                           )
        )
      )
    ),
    
      
    tabPanel("Help", fluid = TRUE, icon = icon("question-circle"),
             uiOutput("infoptxqc")),
    
    tabPanel("About", fluid = TRUE, icon = icon("info-circle"), 
             uiOutput("impressum"))
  
  ), 
  HTML("<script type='text/javascript' src='getFolders.js'></script>")
)))

