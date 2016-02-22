
require(MASS)
require(stats)
require(car)
require(formula.tools)

source("helper.R")


shinyServer(function(input, output) {
  
  dataInput <- reactive({input })
  
  # reactive({ifelse(is.null(input$addVar), "", updateCheckboxInput(session, "addVar", value = FALSE))})
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  output$plots <- renderPlot({
    mapfunc(input)
    
  })
  
  
  
  output$contents <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return("Upload Data set")
    outCont(input)
    
  })
  
  
  
  output$summary = renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return("Model Summary")
    outSumm(input)
    
  })
  
  
  output$var1UI = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    varChoices = createVarList(input)
    if(input$plotType == "Scatterplot" | input$plotType == "Boxplot" | input$plotType == "Mosaic Plot")
    selectInput("var1", label = "Select Response Variable", choices = varChoices)
  })
  
  
  output$var2UI = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    varChoices = createVarList(input)
    if(input$plotType == "Scatterplot" | input$plotType == "Boxplot" | input$plotType == "Mosaic Plot")
    selectInput("var2", label = "Select Explanatory Variable", choices = varChoices)
  })
  
  output$selectedVarUI = renderUI({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    varChoices = createVarList(input)
    if(input$addVar == TRUE)
      selectInput("selectedVar", label = "Color by what variable", choices = varChoices)
  })
  
  output$modelUI = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$plotType == "Response Transformation Diagnosis" | input$plotType == "Added-Variable Plots")
      textInput("model", label = "Enter model", value = "y~x1+x2+...")
  
  })
  
  
  output$numUI = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$plotType == "Response Transformation Diagnosis")
      numericInput("num", label = "Response variable transformation by exponent n", value = 1)
  
  })
  
  output$factIntUI = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$plotType == "Factor Analysis")
      numericInput("factInt", label = "Number of Factors (Integer)", value = 1)
    
  })
  
  output$scatMatrixUI = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$plotType == "Scatterplot")
      checkboxInput("scatMatrix", label = "Scatterplot Matrix", value = FALSE)
    
  })
  
  
  
  output$logBoxUI = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$plotType == "Response Transformation Diagnosis")
      checkboxInput("logBox", label = "Log Transform Response", value = FALSE)
    
  })
  output$selectedVarOption = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$plotType == "Scatterplot" | input$plotType == "Classical MDS")
      checkboxInput("addVar", label = "Color by group", value = FALSE)
  })
  
  
})

