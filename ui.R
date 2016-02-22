
shinyUI(fluidPage(
  titlePanel("QuickViz - Quick Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Upload a data set and start exploring"),
      
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      selectInput("plotType", label = "Plot Type", choices = list('Scatterplot' = 'Scatterplot', 'Mosaic Plot' = 'Mosaic Plot', 'Boxplot' = 'Boxplot','Correlation Heat Map' = 'Correlation Heat Map',  'Classical MDS' = 'Classical MDS','Response Transformation Diagnosis' = 'Response Transformation Diagnosis','Added-Variable Plots' = 'Added-Variable Plots', 'PCA' = 'PCA','Factor Analysis' = 'Factor Analysis')),
      
      htmlOutput("var1UI"),
      
      htmlOutput("var2UI"),
      
      htmlOutput("modelUI"),
      
      htmlOutput("numUI"),
      
      htmlOutput("factIntUI"),
      
      htmlOutput("scatMatrixUI"),
      
      htmlOutput("logBoxUI"),
      
      htmlOutput("selectedVarUI")),
    
      mainPanel(plotOutput("plots" , width = "650px", height = "650px"))
  ), 
  
 
  fluidRow(column(4,verbatimTextOutput('contents')),column(4,verbatimTextOutput('summary')))
  
))