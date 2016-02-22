
shinyUI(fluidPage(
  titlePanel("Visualize.It - Quick Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Upload a data set (CSV) and start exploring"),
      
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
                   choices = c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),''),
      
      selectInput("plotType", label = "Plot Type", choices = list('Scatterplot' = 'Scatterplot', 'Mosaic Plot' = 'Mosaic Plot', 'Boxplot' = 'Boxplot','Correlation Heat Map' = 'Correlation Heat Map',  'Classical MDS' = 'Classical MDS','Response Transformation Diagnosis' = 'Response Transformation Diagnosis','Added-Variable Plots' = 'Added-Variable Plots', 'PCA' = 'PCA','Factor Analysis' = 'Factor Analysis'), selected = 'Classical MDS'),
      
      htmlOutput("var1UI"),
      
      htmlOutput("var2UI"),
      
      htmlOutput("modelUI"),
      
      htmlOutput("numUI"),
      
      htmlOutput("factIntUI"),
      
      htmlOutput("scatMatrixUI"),
      
      htmlOutput("logBoxUI"),
      
      htmlOutput("selectedVarOption"),
    
      htmlOutput("selectedVarUI")),
    
      mainPanel(plotOutput("plots" , width = "750px", height = "750px"))
  ), 
  
 
  fluidRow(column(4,verbatimTextOutput('contents')),column(4,verbatimTextOutput('summary')))
  
))
