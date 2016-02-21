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
    
    output$contents<- reactive({
        inFile<- input$file1
        if(is.null(inFile))
            return(NULL)
            
        dataInput <- read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
        
        #DT::renderDataTable(dataInput,options=list(pageLength=5),escape=TRUE)
        head(dataInput)
    })
    
    output$click_info <- renderPrint({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        nearPoints(dataInput,inpust$plot1_click,"X","Y")
    })
    
    output$summary = renderPrint({
        outSumm(input)
    })
})