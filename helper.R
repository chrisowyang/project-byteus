require(reshape2)
require(ggplot2)
require(d3heatmap)
require(DT)

mapTrans = function(input){
    inFile <- input$file1
    inMod = input$model
    if(is.null(inFile)|(inMod=="y~x1+x2+..."))return(NULL)
    dataInput <- read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
    modelType<- input$type
    formula=as.formula(inMod)
    model=lm(formula,data=dataInput)
    par(mfrow=c(2,2))
    response = lhs.vars(formula)
    predictor = rhs.vars(formula)[1]
    exponent=input$num
    ifelse((input$logBox),(updatedResponse=paste("log(",response"^",exponent,")",sep="",collapse=",")),(updatedResponse=paste(response,"^",exponent=sep="",collapse=",")))
    updatedFormula=eval(parse(text=paste("as.formula(",updatedResponse,"~",predictor,")",sep="",collapse=",")))
    model=update(model,updatedFormula)
    newResponse=lhs.vars(updatedFormula)
    newPredictor=rhs.vars(updatedFormula)[1]
    x=eval(parse(text=paste("dataInput$",newPredictor,sep="",collapse=",")))
    
    ifelse((input$logBox),(y=eval(parse(text=paste("log(dataInput$",response,"^",exponent,")",sep="",collapse=",")))),(y=eval(parse(text=paste("dataInput$",response,"^",exponent,sep="",collapse=",")))))
    
    plot(x,y,ylab=response,xlab=predictor)
    abline(model)
    
    qqPlot(model)
    spreadLevelPlot(model)
    sresid<-studres(model)
    hist(sresid,freq=FALSE,main="Distribution of Studentized Residuals")
    xfit<-seq(min(sresid),max(sresid),length=40)
    yfit<-dnorm(xfit)
    lines(xfit,yfit)
}

mapHeat = function(input){
    inFile <- input$file1
    dataInput<-read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
    nums=sapply(dataInput,is.numeric)
    
    #d3heatmap(dataInput[,nums],scale="column",colors="Blues")
    varcor=cor(dataInput[,nums])
    regular_cor_melt = melt(varcor)
    p=qplot(x=Var1,y=Var2,data=regular_cor_melt,main="Correlation Heat Map",fill=value,geom="title")+scale_fill_gradient2(limits=c(-1,1))
    p+theme(axis.text.x=element_text(size=5,face="bold"))
}

mapfunc <- function(input){
    inFile <- input$file1
    if(is.null(inFile))return(NULL)
    types=input$plotType
    if(types=="Response Transformation"){
        mapTrans(input)
    }
    
    if(types=="Correlation Heat Map"){
        mapHeat(input)
    }
    if(types=="Classical MDS"){
        mdsMap(input)
}}

outSumm=function(input){
    inFile<-input$file1
    inMod=input$model
    if(is.null(inFile)|(inMod=="y~x1+x2+..."))return(NULL)
    
    dataInput<-read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
    
    modelType<- input$type
    formula=as.formula(inMod)
    model=lm(formula,data=dataInput)
    par(mfrow=c(2,2))
    response=lhs.vars(formula)
    predictor=rhs.vars(formula)[1]
    exponent=input$num
    
    ifelse((input$logBox),(updateResponse=paste("log(",response,"^",exponent,")",sep="",collapse=",")))
    
    updatedFormula = eval(parse(text=paste("as.formula(",updatedResponse,"~",predictor,")",sep="",collapse=",")))
    
    model=update(model,updatedFormula)
    
    return(summary(model))
}

mdsMap=function(input){


}
    