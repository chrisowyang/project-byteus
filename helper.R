
mapTrans = function(input){
  
  inFile <- input$file1
  inMod = input$model
  if(is.null(inFile)|(inMod == "y~x1+x2+...")|(is.null(inMod)))return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  modelType <- input$type
  
  formula = as.formula(inMod)
  
  model = lm(formula, data=dataInput)
  
  par(mfrow = c(2,2))
  
  response = lhs.vars(formula)
  
  predictor = rhs.vars(formula)[1]
  
  exponent = input$num
  
  ifelse((input$logBox),(updatedResponse = paste("log(",response,"^",exponent,")",sep = "", collapse =",")),(updatedResponse = paste(response,"^", exponent,sep = "", collapse =",")))
  
  updatedFormula = eval(parse(text=paste("as.formula(", updatedResponse,"~",predictor,")",sep = "", collapse =",")))
  
  model = update(model,updatedFormula)
  
  newResponse = lhs.vars(updatedFormula)
  
  newPredictor = rhs.vars(updatedFormula)[1]
  
  x = eval(parse(text=paste("dataInput$", newPredictor,sep = "", collapse =",")))
  
  ifelse((input$logBox),(y = eval(parse(text=paste("log(dataInput$", response,"^", exponent,")",sep = "", collapse =",")))),(y = eval(parse(text=paste("dataInput$", response,"^", exponent,sep = "", collapse =",")))))
  
  plot( x, y, ylab= response, xlab = predictor  )
  abline(model)
  
  qqPlot(model)
  spreadLevelPlot(model)
  
  sresid <- studres(model) 
  hist(sresid, freq=FALSE, 
       main="Distribution of Studentized Residuals")
  xfit<-seq(min(sresid),max(sresid),length=40) 
  yfit<-dnorm(xfit) 
  lines(xfit, yfit)
  
}


mapHeat = function(input){
  inFile <- input$file1
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  nums = sapply(dataInput, is.numeric)
  
  varcor = cor(dataInput[,nums]) 
  regular_cor_melt = melt(varcor)
  p= qplot(x=Var1, y=Var2, data=regular_cor_melt, main="Correlation Heat Map", fill=value,geom="tile")+ scale_fill_gradient2(limits=c(-1, 1))
  p+theme(axis.text.x=element_text(size=5, face="bold"))
  
}


mapfunc <- function(input){
  inFile <- input$file1
  if(is.null(inFile))return(NULL)
  types = input$plotType
  if(types =="Response Transformation"){
    mapTrans(input)
  }
  
  if(types == "Correlation Heat Map"){
    
    mapHeat(input)
  
  }
  if(types == "Mosaic Plot"){
    
    mapMosaic(input)
  }
  if(types == "Classical MDS"){
    
    mapMDS(input)
  }
  if(types == "Scatterplot"){
    
    mapScat(input)
  }
  if(types == "Boxplot"){
    
    mapBox(input)
  }
  if(types == "Added-Variable Plots"){
    
    mapAV(input)
    
  }
  if(types == "PCA"){
     
    mapPCA(input)
  }
  if(types == 'Factor Analysis'){
    
    mapFactor(input)
  }
}
outSumm = function(input){
  
  if(!input$plotType == "Response Transformation Diagnosis" & !input$plotType == "Added-Variable Plots")return("Build model in Response Transformation or AV plot")
    
  inMod = input$model
  if(is.null(inMod)|(inMod == "y~x1+x2+..."))return("Change model format: y~x1+x2+...")
  
  inFile <- input$file1
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  modelType <- input$type
  
  formula = as.formula(inMod)
  
  model = lm(formula, data=dataInput)
  
  par(mfrow = c(2,2))
  
  response = lhs.vars(formula)
  
  predictor = rhs.vars(formula)[1]
  
  exponent = input$num
  
  ifelse((input$logBox),(updatedResponse = paste("log(",response,"^",exponent,")",sep = "", collapse =",")),(updatedResponse = paste(response,"^", exponent,sep = "", collapse =",")))
  
  updatedFormula = eval(parse(text=paste("as.formula(", updatedResponse,"~",predictor,")",sep = "", collapse =",")))
  
  model = update(model,updatedFormula)
  
  return(summary(model))
}

mapMDS= function(input){
  
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  nums = sapply(dataInput, is.numeric)
  
  loc = cmdscale(dist(dataInput[, nums]), k = 2, eig = TRUE)
  
  #create input to select what variable to color (selectedVar)
   #col = input$selectedVar,
  plot(loc$points, pch = 19)
  
}

outCont = function(input){
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    names(dataInput)
}



mapScat = function (input){
  
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  if(input$scatMatrix == TRUE)return(pairs(dataInput))
 
  y = eval(parse(text = paste("dataInput$", input$var1, sep="")))
  
  x= eval(parse(text = paste("dataInput$", input$var2, sep="")))
  
  return(plot(x,y))
}


  
mapBox = function(input){
  
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  y = eval(parse(text = paste("dataInput$", input$var1, sep="")))
  
  x= eval(parse(text = paste("dataInput$", input$var2, sep="")))
  
  #ylab needs fix
  boxplot(y ~ x, data = dataInput, names = levels(x))
  
  
}

mapAV = function(input){
  
  
  inFile <- input$file1
  inMod = input$model
  if(is.null(inFile)|(inMod == "y~x1+x2+..."))return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  modelType <- input$type
  
  formula = as.formula(inMod)
  
  model = lm(formula, data=dataInput)
  
  par(mfrow = c(2,2))
  
  response = lhs.vars(formula)
  
  predictor = rhs.vars(formula)[1]
  
  exponent = input$num
  
  ifelse((input$logBox),(updatedResponse = paste("log(",response,"^",exponent,")",sep = "", collapse =",")),(updatedResponse = paste(response,"^", exponent,sep = "", collapse =",")))
  
  updatedFormula = eval(parse(text=paste("as.formula(", updatedResponse,"~",predictor,")",sep = "", collapse =",")))
  
  model = update(model,updatedFormula)
  
  avPlots(model)
  
  
}

mapMosaic = function(input){
  
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  y = eval(parse(text = paste("dataInput$", input$var1, sep="")))
  
  x= eval(parse(text = paste("dataInput$", input$var2, sep="")))
  
  tab <- table(x, y)
  
  row.names(tab) <- levels(x)
  
  #fix labels
  mosaicplot(tab, main = " ", color = TRUE)
  
}
  

mapPCA = function(input){
  
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  nums = sapply(dataInput, is.numeric)
  
  pc = prcomp(dataInput[,nums], scale = TRUE)
  
  plot(pc$x, col = input$selectedVar, pch = 19)
}

mapFactor = function(input){
  
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  factanal(dataInput[,-1] , factors = input$factInt) 
  
  
}

createVarList = function(input){
  
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
  dataInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  
  choices = paste(names(dataInput),sep='',collapse=',')
  
  listChoices = as.list(strsplit(choices, ",")[[1]])
  return(listChoices)
}



