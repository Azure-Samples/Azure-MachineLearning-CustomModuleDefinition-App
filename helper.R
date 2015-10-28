library(shiny)
library(XML)

# Predefined lists of port/arg properties.
# 
# nameList: Maps arg properties to their internal id for each arg type 
# cpAllowedTypes: Allowed types for a 'ColumnPicker' argument.
#
#
nameList <<- list(bool=list(default = "_boolopt"), 
                  int=list(default = "_intopt", min = "_intmin", max = "_intmax"),
                  double=list(default = "_intopt", min = "_intmin", max = "_intmax"),
                  string=list(default = "_stringopt"),
                  ColumnPicker=list(default = "_cpopt", allowedTypes = "_cptypes", portId = "_cpPortID"))

cpAllowedTypes <- c("None")
for(valueType in c("Numeric", "Boolean", "Categorical", "String", "All"))
  for(obj in c("Feature", "Label", "Score", "All"))
    cpAllowedTypes <- c(cpAllowedTypes, paste0(valueType, obj))
cpAllowedTypes <- replace(cpAllowedTypes, length(cpAllowedTypes), "All")


# Labels required fields with a red star.
#
# Returns: 
#     Tags to be added to the field label.
#
labelRequired <- function(label) {
  tagList(
    label,
    span("*", class = "requiredStar")
  )
}

# Given a label of an input field, dysplay an alert message.
#
#
# Returns: 
#     Tags for a js script that produces and alert window.
#
# Use:
#     In server.R to alert user that one of the fields
#     required to generate the XML is not filled.
#     
alertMsg <- function(label){
  tags$script(
    paste0('window.alert("The ', 
           label, ' cannot be empty.")') 
  )
}

# Disables action button.
#
# Returns:
#     A message from server to client.
#
# Use:
#     In server.R, to disable 'addOutput' when the number
#     of outputs is 8 or 'removeOutput' when the number 
#     of outputs is 0.
#
disableActionButton <- function(id, session) {
  session$sendCustomMessage(type="disableButton",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

# Enables action button 
#
# Returns:
#     A message from server to client
#
# Use:
#     In server.R, to enable 'addOutput' when the number
#     of outputs is less than 8 or 'removeOutput' when  
#     the number of outputs is more than 0.
#
enableActionButton <- function(id, session) {
  session$sendCustomMessage(type="enableButton",
                            list(code= paste("$('#",id,"').prop('disabled',false)"
                                             ,sep="")))
}

# Adds dependencies for syntax coloring.
#
# Returns:
#     Tags intended for the <HEAD> of the document. 
#
# Use: 
#     To display the uploaded script and the XML document
#     using color-coded syntax.
#
syntaxColorTags <- function() {
  html <- tags$link(rel="stylesheet", type="text/css", 
                    href="shared/highlight/rstudio.css")
  dep  <- htmlDependency("highlight.js", "6.2", c(href="shared/highlight"),
                         script = "highlight.pack.js")
  return(attachDependencies(html, dep))
}

# Colors code syntax.
#
# Returns:
#     HTML tags to surround the R script/XML text. 
#
codeDisplayTags <- function(rFile){
  if(!is.null(rFile)){
    suppressWarnings(fileText <- paste(readLines(rFile),  collapse="\n"))
  }
  else{
    fileText <- ""
  }
  list(pre(class="shiny-code",
           HTML(format(tags$code(
             class="language-r",
             fileText
           ), indent = FALSE))),
       tags$script('hljs.initHighlighting(); hljs.initHighlighting.called=false;'))
}

# Checks if expressions in the parsed script are functions.
#
# Returns:
#     Names of the functions in the script.
#
# Use:
#     In getRFunctions.
validExpression <- function(parsedScript){
  func <- c()
  for(i in 1:length(parsedScript))
    func <- c(func, grepl("*<- *function*(([^)]+))", as.character(parsedScript[i]))[1])
  return(func)
}

# Gets the names and arguments of R methods from uploaded file.
# 
# Params:
#   srcFile: the path to the uploaded file.
# 
# Returns:
#   The functions from srcFile and their arguments.
#
getRFunctions <- function(srcFile){
  
  if(is.null(srcFile))
    return()
  
  sandbox <- new.env()
  
  # Parse script into expressions to be evaluated
  # Return NULL if script is invalid.
  err <- tryCatch(
    { parsedScript <- parse(srcFile)},
    error = function(e){
      return(NULL)
    }
  )
  if(is.null(err)) return()
  
  # Evaluate variables and functions in a temp environment
  obj <- parsedScript[validExpression(parsedScript)]
  sapply(obj, function(f, envir = sandbox) eval(f, envir = envir))
  
  # Get all objects in the temp environment
  fobj <- ls(env=sandbox)
  if(length(fobj)==0) return()
  
  # Select only the functions
  fobj <- fobj[sapply(fobj, function(f) exists(f, envir=sandbox, mode="function", inherits=FALSE))]
  if(length(fobj)==0) return()
  
 # Return list of functions and their arguments
  return(lapply(mget(fobj, envir = sandbox), function(f) formals(f)))
}

# Adds row of input fields for each input port
#
# Params:
#     argNames: names of all input ports
#     id: internal id of the row
#
# Returns:
#     HTML tags for the display of input fields.
#
addInputRow <- function(argNames, id){
  conditionalPanel(
    condition = paste0("input.inputPorts.indexOf('",argNames[id],"') >= 0"),
    
    list(
      fluidRow(
        column(
          2, style="padding-top:8px",
          argNames[id]
        ),
        column(
          2,
          selectizeInput(paste0("input",id,"_type"),label=NULL,
                         choices=c("data.frame"="DataTable", "Zip"), selected="DataTable"
          )
        ),
        column(
          2,
          textInput(paste0("input",id,"_name"), label=NULL, value=argNames[id])
        ),
        column(
          3,
          textInput(paste0("input",id,"_desc"), label=NULL)
        ),
        column(
          3,
          checkboxInput(paste0("input",id,"_opt"), label = "Optional Port")
        )
      ),
      hr()
    )
    
  )
}

# Makes a table of input fields for all input ports
# Adds a row for each id in (1, length(argNames))
#
# Params:
#     argNames: names of all port inputs
#     input: input objects passed in by Shiny ui
getInputPorts <- function(argNames, input){
  
  pageHeader <- list(
    fluidRow(
      column( 
        6, style="padding-top:15px",
        tags$b("Select input ports:")
      )
    ),
    fluidRow(
      column( 
        6, style="padding-top:10px",
        checkboxGroupInput("inputPorts", label=NULL, 
                           choices=argNames, selected=argNames, inline=FALSE)
      )
    ),
    fluidRow(
      column(
        2,  style="padding-top:20px",
        tags$b("ID")
      ),
      column(
        2,  style="padding-top:20px",
        tags$b("Type")
      ),
      column(
        2,  style="padding-top:20px",
        tags$b("Name")
      ),
      column(
        3,  style="padding-top:20px",
        tags$b("Description")
      ),
      column(
        3,  style="padding-top:20px",
        tags$b("Options")
      )
    ),
    hr(),
    fluidRow(
      column(
        6,
        tags$b("Input Ports:")
      )
    ),
    hr())
  
  inputTable <- list()
  
  if(length(argNames)==0)
    return(list(pageHeader))
  
  for(j in 1:length(argNames)){
    
    inputTableRow <- addInputRow(argNames,j)
    inputTable <- c(inputTable, list(inputTableRow))
  }
  
  return(list(pageHeader, inputTable))
}

# Adds row of input fields for each input arg
# For each arg type (bool, int,etc.), creates 
# a conditional panel for the corresponding options
#
# Params:
#     argNames: names of all input args
#     id: internal id of the row
#
# Returns:
#     HTML tags for the display of input fields.
#
addArgRow <- function(argNames, id){
  conditionalPanel(
    condition = paste0("(!!input.inputPorts) && input.inputPorts.indexOf('",
                       argNames[id],"') < 0"),
    
    list(hr(),
      fluidRow(
        column(
          2, style="padding-top:8px",
          argNames[id]
        ),
        column(
          2,
          selectizeInput(paste0("input",id,"_vartype"),label=NULL,
                         choices=c("bool","int","double","string","ColumnPicker"), selected=NULL,
                         options = list(
                           placeholder = 'Select',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          )
        ),
        column(
          2,
          textInput(paste0("input",id,"_varname"), label=NULL, value=argNames[id])
        ),
        column(
          3,
          textInput(paste0("input",id,"_vardesc"), label=NULL)
        ),
        column(#style = "overflow-y:scroll;  max-height: 130px;",  
          # Make a conditional panel for every argument type
          3,
          # Default values
          conditionalPanel(
            condition = paste0("input.","input",id,"_vartype"," == ''"),
            helpText("No Options")
          ),
          # Bool Arg
          conditionalPanel(
            condition = paste0("input.","input",id,"_vartype"," == 'bool'"),
            fluidRow(
              column(
                4, "Default Value:"
              ),
              column(
                8,
                selectInput(paste0("input",id,"_boolopt"),label=NULL, 
                            choices = c("FALSE","TRUE"))
              )
            )
          ),
          # Int or double Arg
          conditionalPanel(
            condition = paste0("input.","input",id,"_vartype"," == 'int' || input.",
                               "input",id,"_vartype", " == 'double'"),
            fluidRow(
              column(
                4, "Default Value:"
              ),
              column(
                8, textInput(paste0("input",id,"_intopt"), label=NULL)
              )
            ),
            
            fluidRow(
              column(
                6,
                textInput(paste0("input",id,"_intmin"), label=h5("Min:"))
              ),
              column(
                6,
                textInput(paste0("input",id,"_intmax"), label=h5("Max:"))
              )
            )
            
          ),
          # String Arg
          conditionalPanel(
            condition = paste0("input.","input",id,"_vartype"," == 'string' "),
            fluidRow(
              column(
                4, 
                "Default Value:"
              ),
              column(
                8,
                textInput(paste0("input",id,"_stringopt"),label=NULL)
              )
            )
          ),
          # ColumnPicker Arg
          conditionalPanel(
            condition = paste0("input.","input",id,"_vartype"," == 'ColumnPicker' "),
            fluidRow(
              column(
                12,
                "Port ID:",
                textInput(paste0("input",id,"_cpPortID"), label=NULL)
              )
            ),
            fluidRow(
              column(
                12, "Default Value:",
                selectizeInput(paste0("input",id,"_cptopt"), label=NULL, 
                               choices=cpAllowedTypes, 
                               selected=NULL,
                               options = list(
                                 placeholder = 'Select',
                                 onInitialize = I('function() { this.setValue(""); }'))
                )
              )
            ),
            fluidRow(
              column(
                12,
                "Allowed Types:"
              )
            ),
            fluidRow(
              column(
                12,
                selectizeInput(paste0("input",id,"_cptypes"), label=NULL, 
                               choices=c("Numeric", "Boolean", "Categorical","String",
                                         "Label", "Feature", "Score", "All"), 
                               selected=NULL,
                               options = list(
                                 placeholder = 'Select',
                                 onInitialize = I('function() { this.setValue(""); }'))
                )
              )
            )
            
          )
        )
      )
    )
  )
}

# Makes a table of input fields for all input args.
# Adds a row for each id in (1, length(argNames)).
#
# Params:
#     argNames: names of all input args.
#     input: input object passed in by Shiny ui.
#
getArgTable <- function(argNames, input){
  
  pageHeader <- list(
    fluidRow(
      column(
        6,
        tags$b("Arguments:")
      )
    )
  )
  
  argTable <- list()
  
  if(length(argNames)==0)
    return(list(pageHeader))
  
  for(j in 1:length(argNames)){
    
    argTableRow <- addArgRow(argNames, j)
    argTable <- c(argTable, list(argTableRow))
  }
  return(c(pageHeader, argTable))
}

# Adds row of input fields for each output port.
# For each output, it creates a conditional panel
# that appears only when the number of outputs
# is greater than or equal to the row id.
#
# Params:
#     id: internal id of the row.
#
addOutputRow <- function(id){
  conditionalPanel(
    condition = paste0("(input.addOutput - input.removeOutput) >= ",id),
    
    list(fluidRow(
      column(
        2,
        style="padding-top:8px",
        paste0("output",id)
      ),
      column(
        2, style="padding-top:8px",
        "data.frame"
      ),
      column(
        2,
        textInput(paste0("output",id,"_name"), label=NULL, value=paste0("output",id))
      ),
      column(
        3,
        textInput(paste0("output",id,"_desc"), label=NULL)
      ),
      column(
        3,
        helpText("No Options")
      )
    ), hr())
    
  )
}

# Makes table for output ports.
# Adds an output row for each id
# in (1, no. of outputs).
#
getOutputPorts <- function(input){
  tableHeader <-  list(
    fluidRow(
      column(
        12, style="padding-top:20px;",
        tags$p("You can add up to 8 output ports and the 
             number of outputs has to match the number of outputs of the module. For example,
               if the chosen R method returns a list of 2 dataframes, the number of outputs should be 2.")
      )
    ),
    fluidRow(
      column(
        2, style="padding-top:20px",
        actionButton("addOutput", "Add Output", style='background-color:#337ab7;color:white')
      ),
      column(
        2, style="padding-top:20px",
        actionButton("removeOutput", "Remove Output", style='background-color:#337ab7;color:white')
      )
    ),
    fluidRow(
      column(
        2,  style="padding-top:20px",
        tags$b("ID")
      ),
      column(
        2,  style="padding-top:20px",
        tags$b("Type")
      ),
      column(
        2,  style="padding-top:20px",
        tags$b("Name")
      ),
      column(
        3,  style="padding-top:20px",
        tags$b("Description")
      ),
      column(
        3,  style="padding-top:20px",
        tags$b("Options")
      )
    ),
    hr())
  
  outputTable <- list()
  for(j in 1:8){
    outputTableRow <- addOutputRow(j)
    outputTable <-c(outputTable, list(outputTableRow))
  }
  
  visPort <- list(
    fluidRow(
      column(
        4, 
        checkboxInput("visPort", label=tags$b("Add Visualization Output"))
      ),
      column(
        8,  style="padding-top:10px",
        conditionalPanel(
          condition = paste0("input.visPort"),
          textInput("visPortDesc", "Description:")
        )
      )
    )
  )
  
  return(list(tableHeader, outputTable, visPort))
}

# Checks if the fields necessary for the XML definition
# have been correctly filled out.
#
# Params:
#     varNames: names of all arguments of the R method
#     input: input object passed in by Shiny ui.
#
# Returns:
#     NULL if the fields were correctly filled out,
#     the label of the empty field otherwise.    
#
validateXML <- function(varNames, input){
  
  if(input$generateXML == 0)
    return("")
  if(input$inputFile == "")
    return("selected file")
  
  if(input$inputMethod == "")
    return("R method")
  if(input$moduleName == "")
    return("module name")
  if(input$moduleOwner=="")
    return("module owner")
  
  if(length(varNames)==0)
    return()
  
  for(j in 1:length(varNames)){
    if(!(varNames[j] %in% input$inputPorts) &&
       input[[paste0("input",j,"_vartype")]] == ""){
      
      return(paste0("type of the argument ","'",varNames[j], "'"))
      
    }
  }
  
  return()
}

# Create XML definition for module
# 
# Returns:
#     NULL if one of necessary fields was not filled out,
#     the XML definition otherwise.     
#
getXML <- function(varNames, input){
  
  if(!is.null(validateXML(varNames, input)))
    return()
  
  # Begin xml document
  options(warn = -1)
  xmlDoc <- xmlTree()
  
  # Begin module definition
  # Add module name
  xmlDoc$addNode("Module", attrs=c("name"=input$moduleName), close=FALSE)
  # Add module owner
  xmlDoc$addNode("Owner", input$moduleOwner)
  # Add module description
  xmlDoc$addNode("Description", input$moduleDesc)
  # Add language
  xmlDoc$addNode("Language", attrs=c("name"="R", 
                                     "entryPoint"=input$inputMethod,
                                     "sourceFile"=input$inputFile))
  ### Add ports
  childList <- c()
  # Add input children
  for(varName in input$inputPorts){
    idx <- match(varName, varNames)
    child <- xmlDoc$addNode("Input", attrs=c("id"=varName, 
                                             "name"=input[[paste0("input",idx,"_name")]],
                                             "type"=input[[paste0("input",idx,"_type")]],
                                             "isOptional"=tolower(input[[paste0("input",idx,"_opt")]])
    ),
    .children=c(xmlDoc$addNode("Description", input[[paste0("input",idx,"_desc")]]))
    )
    childList <- c(childList, child)
  }
  # Add output children
  if((input$addOutput-input$removeOutput)>0){
    for(j in 1:(input$addOutput-input$removeOutput)){
      varName <- paste0("output",j)
      child <- xmlDoc$addNode("Output", attrs=c("id"=varName, 
                                                "name"=input[[paste0(varName, "_name")]],
                                                "type"="DataTable"),
                              .children=c(xmlDoc$addNode("Description", input[[paste0(varName, "_desc")]]))
      )
      childList <- c(childList, child)
    }
  }
  # Add visualization port
  if(input$visPort){
    child <- xmlDoc$addNode("Output", 
                            attrs=c("id"="deviceOutput", "name"="View Port", "type"="Visualization"),
                            .children=c(xmlDoc$addNode("Description",  input[['visPortDesc']])))
    childList <- c(childList, child)
  } 
  ### Attach children to "Ports" parent
  xmlDoc$addNode("Ports", .children = childList)
  
  ### Add arguments
  childList <- c()
  # Add arg children
  for(varName in setdiff(varNames, input$inputPorts)){
    idx <- match(varName, varNames)
    attrs <- c()
    
    type <- input[[paste0("input",idx,"_vartype")]]
    propList <- nameList[[type]]
    if(type=="bool"){
      attrs <- c("default"=tolower(input[[paste0("input",idx,"_boolopt")]]))
    }
    else{
      for(prop in names(propList))
        attrs[[prop]] <- input[[paste0("input",idx,propList[[prop]])]]
    }
    child <- xmlDoc$addNode("Arg", attrs=c("id"=varName,
                                           "name"=input[[paste0("input",idx,"_varname")]],
                                           "type"=input[[paste0("input",idx,"_vartype")]]),
                            .children=c(xmlDoc$addNode("Properties", attrs=attrs), 
                                        xmlDoc$addNode("Description", input[[paste0("input",idx,"_vardesc")]]))
    )
    childList <- c(childList, child)
  }
  ### Attach children to "Arguments" parent           
  xmlDoc$addNode("Arguments", .children = childList)
  # Close module definition
  xmlDoc$closeNode()
  
  return(saveXML(xmlDoc))
}
