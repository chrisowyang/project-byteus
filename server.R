require(MASS)
require(stats)
require(car)
require(formula.tools)

source("helper.R")

shinyServer(function(input,output) {

    dataInput <- reactive({input})
    
    output$plots <- renderPlot({
        mapfunc(input)
    })
    
    output$contents<- renderPrint({outCont(input)
})
            
        dataInput <- read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
        
        #DT::renderDataTable(dataInput,options=list(pageLength=5),escape=TRUE)
        head(dataInput)
    })
    
    output$summary = renderPrint({
        outSumm(input)
    })
    
    output$var1UI=renderUI({
        inFile<- input$file1
        if (is.null(inFile))
            return(NULL)
        varChoices = createVarList(input)
        
        selectInput("var1",label="Select Predictor Variable", choices = varChoices)
    })
    
    output$selectedVarUI = renderUI({
        inFile<-input$file1
        if (is.null(inFile))
            return(NULL)
        varChoices = createVarList(input)
        selectInput("selectedVar",label="Color by what variable",choices=varChoices)
    })
})