library(shiny)
source("helper.R")
#Sys.setenv(R_ZIPCMD="C:/Rtools/bin/zip")

# Define server logic required to build app
shinyServer(function(input, output, session) {
  
  # Reactive value that stores the path of 
  # the source file.
  #
  chosenFilePath <- reactive({
    if(is.null(input$inputFile) 
       || input$inputFile == "")
      return()
   isolate({
     idx <- match(input$inputFile, input$srcFile$name)
     input$srcFile$datapath[idx]
   })
  })
  
  # Reactive value that stores the R methods 
  # (and their arguments) in the source file
  #
  rMethodsNames <- reactive({
    getRFunctions(chosenFilePath())
  })
  
  # Renders dropdown menu for choosing source file
  # from among the uploaded files.
  #
  output$chooseFile <- renderUI({
    if(length(input$srcFile$name)==1)
      options <- NULL
    else{
      options <- list(
        placeholder = 'Select',
        onInitialize = I('function() { this.setValue(""); }')
      )
    }
    selectizeInput("inputFile", labelRequired("Choose source file:"), 
                    choices = input$srcFile$name, width="170px",
                    options = options)
    
  })
  
  # Renders dropdown menu for choosing method
  # from among the methods in the source file.
  #
  output$chooseMethod <- renderUI({
    if(length(names(rMethodsNames()))==1)
      options <- NULL
    else{
      options <- list(
        placeholder = 'Select',
        onInitialize = I('function() { this.setValue(""); }')
      ) 
    }
    selectizeInput("inputMethod", labelRequired("Choose method:"), 
                   choices = names(rMethodsNames()),  width="170px",
                   options = options)
  })
  
  # Displays an alert message if the source file contains an invalid script
  # or it does not contain any methods.
  #
  observe({
    if(!is.null(input$inputFile) &&
       input$inputFile != "" && 
       is.null(rMethodsNames()))
      session$sendCustomMessage(type = "invalidScript", "Invalid script or no R methods found.")
  })
  
  # Displays script text with colored syntax
  output$script <- renderUI({
    codeDisplayTags(chosenFilePath())
  })
  
  # Creates (reactive) input entries
  inputData <- reactive({
    
    if(input$inputMethod == "" || is.null(rMethodsNames()))
      return()
    
    varNames <- names(rMethodsNames()[[input$inputMethod]])
    isolate({
      list(
        getInputPorts(varNames, input),
        getArgTable(varNames, input) 
      )
    })
    
  }) 
  
  # Creates (reactive) output entries
  outputData <- reactive({
    
    if( input$inputMethod == "" || is.null(rMethodsNames()))
      return()
    getOutputPorts(isolate(input))
    
  })
  
  # Renders input table
  output$inputs <- renderUI({
    inputData()
  })
  
  # Renders output table
  output$outputs <- renderUI({
    outputData()
  })
  
  # Evaluates expression in the tabs even when hidden.
  outputOptions(output, 'outputs', suspendWhenHidden=FALSE)
  outputOptions(output, 'inputs', suspendWhenHidden=FALSE)
  
  # Generates XML definition
  moduleXML <- reactive({
    input$generateXML
    isolate(getXML(names(rMethodsNames()[[input$inputMethod]]),input))
  })
  
  # Displasy XML definition and download button if all the fields 
  # have been filled out correctly.
  # Otherwise, return NULL.
  #
  output$xml <- renderUI({
    
    # Take dependency on these input values.
    input$generateXML
    input$srcFile
    input$inputFile
    input$inputMethod
    
    isolate({
      varNames <- names(rMethodsNames()[[input$inputMethod]])
      incompleteField <- validateXML(varNames, input)
      
      if(!is.null(incompleteField))
        return()
    })
    
    isolate(
      list(
        list(pre(class="shiny-code",
                 HTML(format(tags$code(
                   class="language-r",
                   moduleXML()
                 ), indent = FALSE))),
             tags$script('hljs.initHighlighting(); hljs.initHighlighting.called=false;')),
        # Postpone download until an XML file
        # has been generated
        downloadButton("downloadData", "Download Zip"),
        hr()
      )
    )
    
  })
  
  # Displays an alert message if the 'generateXML' button was clicked
  # and not all the required fields were filled out.
  # 
  observe({
    input$generateXML
    
    isolate({
      varNames <- names(rMethodsNames()[[input$inputMethod]])
      incompleteField <- validateXML(varNames, input)
      
      if(is.null(incompleteField))
        return()
      else{
        session$sendCustomMessage(type = "incompleteData", 
                                  incompleteField)
      }
      
    })
  })
  
  
  # Downloads zip file of all source files and the XML definition of the module
  output$downloadData <- downloadHandler(
    filename = function() { paste0(input$inputMethod, ".zip")},
    content = function(fname) {
  
      tmpdir <- tempdir()
      setwd(tempdir())
      
      filePaths <- input$srcFile$datapath
      fileNames <- input$srcFile$name

      fs <- c(paste0(input$inputMethod,".xml"), fileNames)
      
      sapply(1:length(filePaths), 
             function(x) file.copy(filePaths[x], fileNames[x]))
      write(moduleXML(), file = fs[1])
    
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  # Disables 'addOutput' button if 8 outputs were already added
  # Disables 'removeOutput' button if no outputs were added
  observe({
    if(is.null(input$addOutput)) return()
    
    if(input$addOutput - input$removeOutput >= 8){
      disableActionButton("addOutput",session)
      return()
    }
    if(input$addOutput - input$removeOutput <= 0){
      disableActionButton("removeOutput",session)
      return()
    }
    enableActionButton("addOutput", session)
    enableActionButton("removeOutput", session)
      
  })
  
  # Remove user input from recently hidden (previously visible)
  # output ports
  observe({
    input$removeOutput
    isolate({
      id <- as.integer(input$addOutput-input$removeOutput+1)
    })
    updateTextInput(session, paste0("output",id,"_name"), value=paste0("output",id))
    updateTextInput(session, paste0("output",id,"_desc"), value="")
  })
  
  # Remove user input from recently hidden (previously visible)
  # input ports
  observe({
    if(is.null(input$inputPorts) || length(input$inputPorts)==0)
      return()
    isolate({
      varNames <- names(rMethodsNames()[[input$inputMethod]])
      for(j in 1:length(varNames)){
        if(!(varNames[j] %in% input$inputPorts)){
          updateSelectizeInput(session, paste0("input",j,"_type"), selected = "DataTable")
          updateTextInput(session, paste0("input",j,"_name"), value=varNames[j])
          updateTextInput(session, paste0("input",j,"_desc"), value="")
          updateCheckboxInput(session, paste0("input",j,"_opt"), value=FALSE)
        }
      }
    })
  })
  
})



