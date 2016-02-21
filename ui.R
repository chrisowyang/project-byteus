

shinyUI(fluidPage(
    titlePanel("Linear Model Response Variable Transformation Explorer"),

    sidebarLayout(
        sidebarPanel(
            helpText("Create a model and see the effects of transforming the response variable"),
            fileInput('file1','Choose CSV File',accept=c('text/csv','text/comma-separated-values','text/plain','.csv')),
            tags$hr(),
            checkboxInput('header','Header',TRUE),
            radioButtons('sep','Separator',c(Comma=',',Semicolon';',Tab='\t'),','),
            radioButtons('quote','Quote',c(None='','Double Quote'='"','Single Quote'="'"),'"'),
            selectInput("type",label="Model Type", choices=list('Linear Model'='Linear Model'),selected='Linear Model'),
            textInput("model",label="Enter model",value="y~x1+x2+..."),
            numericInput("num",label="Reponse variable transformation by exponent n",value=1),
            checkboxInput("logBox",label="Log Transformation Response",value=FALSE),
            selectInput("plotType",label="Plot Type",choices=list('Response Transformation'='Response Transformation','Correlation Heat Map'='Correlation Heat Map','Classical MDS'='Classical MDS'),selected='Response Transformation')),
            mainPanel(plotOutput("plots",click="plot1_click",brush=brush0pts (id="plot1_brush"),width="650px",height="650px"))
        ),
            #fluidRow(column(width=6,h3("Points near click"),
            verbatimTextOutput("click_info")),
                #column(width=6,h3("Brushed points"),
                verbatimTextOutput("brush_info")),
            #hr(),
        fluidRow(column(4,verbatimTextOutput('contents')),column(4,verbatimTextOutput('summary')))
))
            