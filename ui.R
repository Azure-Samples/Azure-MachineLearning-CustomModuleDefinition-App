library(shiny)
library(htmltools)
source("helper.R")

# Define the UI elements of the Shiny app
shinyUI(fluidPage(style="width: 800px",
  
  # Add js script tags to the <HEAD> of the document.
  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("disableButton",
                             function(message) {
                             console.log(message)
                             eval(message.code);
                             });'
                             )),
            tags$script(HTML('Shiny.addCustomMessageHandler("enableButton",
                             function(message) {
                             console.log(message)
                             eval(message.code);
                             });')),
            tags$script(HTML('Shiny.addCustomMessageHandler("incompleteData",
                              function(msg) {
                              window.alert("The "+msg+" cannot be empty.")
                              });')),
            tags$script(HTML('Shiny.addCustomMessageHandler("invalidScript",
                              function(msg) {
                              window.alert(msg)
                              });')),
            syntaxColorTags()
            ),
  
  # Add style options to tags in the document
  tags$style(type="text/css", ".recalculating{ opacity: 1.0; } 
             #chooseMethod.recalculating{ opacity: 1.0; }"),
  tags$style(type='text/css', ".selectize-dropdown-content {max-height: 100px; }"), 
  tags$style(type="text/css", ".requiredStar { color: red; }"),
  tags$head(tags$style("p {font-size: 15px;} li{font-size: 15px}")),
  
  # Page title and description of the app
  tags$h2("XML Definition for Custom R Modules in Azure ML"),
  p("Key words: Azure, custom modules, machine learning, Microsoft, R."),
  hr(style = "border-top: 10px solid #eee;"),
  
  tags$p("This Shiny application allows users to create a .zip package containing 
         the files necessary for delploying a custom R module in Azure Machine Learning. 
         To learn more about custom R modules for Azure Machine Learning", 
         tags$a("click here.", 
                href="https://azure.microsoft.com/en-us/documentation/articles/machine-learning-custom-r-modules/")),
  tags$p("To get started, upload the file that implements the R method exposed by the module,
         as well as any additional files with functionality that can be accessed from the custom module.",
         tags$b("Note:"), "This version does not support folder upload. You can, however, select multiple files during upload."),
  tags$p("Once the files have been uploaded and the source file and method are selected, fill out the fields in the 
         dynamic panel that will appear below. The following are required to successfully generate an XML definition for the custom module:"),
  tags$ul(tags$li("a valid R script with at least one method"), 
          tags$li("a module name and owner"),
          tags$li("types for the input arguments (by default, all method parameters are input ports)")),
  tags$p("Click the 'generateXML' button to display the XML definition of the custom module. A download button will appear
         at the bottom of the generated XML."),
  hr(style = "border-top: 10px solid #eee;"),
  
  # Input fields for all files, source file
  # and the method exposed by the module
  fluidRow(
    column(
      4, style="height: 300px;",
      wellPanel(
        fileInput("srcFile", labelRequired("Choose files:"),  width="170px",
                  accept=c("txt", "text/plain", ".R"), multiple = TRUE),
        uiOutput("chooseFile"),
        uiOutput("chooseMethod")
      )
    ),
    column(
      8, style = "overflow-y:scroll;  max-height: 280px;", 
      uiOutput("script")
    )
  ),
  hr(style = "border-top: 10px solid #eee;"),
  
  # Input fields for name, owner and description of the module.
  wellPanel(
    fluidRow(
      column(
        4, style="padding-left:30px",
        textInput("moduleName", labelRequired("Module name:"), 
                  value = "", width="200px")
      ),
      column( style="padding-left:30px; padding-right:30px",
        4,
        textInput("moduleOwner", labelRequired("Module owner:"), 
                  value = "", width="200px")
      ),
      column(
        4, style="padding-right:30px",
        textInput("moduleDesc", "Module description:", 
                  value = "", width="200px")
      )
    )
  ), 
  
  # Tabs with input fields for input ports/args and output ports.
  fluidRow(
    column(
      12,
      tabsetPanel(
        tabPanel(
          "Inputs",
          htmlOutput("inputs")
        ),
        tabPanel(
          "Outputs",
          uiOutput("outputs")
        )
      )
    )
  ),
  hr(style = "border-top: 10px solid #eee;"),
  
  # Button that displays the XML definition.
  fluidRow(
    column( style="padding-top:20px; padding-bottom:20px",
      2, 
      actionButton("generateXML", "Generate XML")
    )
  ),
  
  # XML definition and download button
  fluidRow(
    column(
      12,
      uiOutput("xml")
    )
  )
    
  ))
