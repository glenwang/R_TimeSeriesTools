# functions

TrainTestAssignPrint=function(name, Train, Test){
  assign(paste(name, "TrainSet", sep="."),Train, envir = .GlobalEnv)
  assign(paste(name, "TestSet", sep="."),Test, envir = .GlobalEnv)
  print(paste(name, "TrainSet", sep="."))
  print(head(Train,1))
  print(tail(Train,1))
  print(paste(name, "TestSet", sep="."))
  print(head(Test,1))
  print(tail(Test,1))
}

SplitTS=function(dataTS, trainRatio=0.8){
  name=deparse(substitute(dataTS))
  splitPoint<- floor(trainRatio*length(dataTS)) #select the first 80% of the data
  Train = window(dataTS, end = time(dataTS)[splitPoint])
  Test <- window(dataTS, start=time(dataTS)[splitPoint+1]) #assign the most recent 20% to the test set
  TrainTestAssignPrint(name, Train, Test)
}

trainSet=function(dataTS, trainRatio=0.8){
  splitPoint<- floor(trainRatio*length(dataTS)) #select the first 80% of the data
  Train = window(dataTS, end = time(dataTS)[splitPoint])
  return(Train)
}

testSet=function(dataTS, trainRatio=0.8){
  splitPoint<- floor(trainRatio*length(dataTS)) #select the first 80% of the data
  Test <- window(dataTS, start=time(dataTS)[splitPoint+1])
  return(Test)
}

SplitTSMatrix=function(dataTS, trainRatio=0.8){
  name=deparse(substitute(dataTS))
  splitPoint<- floor(trainRatio*nrow(dataTS)) #select the first 80% of the data
  Train = window(dataTS, end = time(dataTS)[splitPoint])
  Test <- window(dataTS, start=time(dataTS)[splitPoint+1]) #assign the most recent 20% to the test set
  TrainTestAssignPrint(name, Train, Test)
}

SplitSequence=function(sequence, trainRatio=0.8){
  splitPoint<- floor(trainRatio*length(sequence)) #select the first 80% of the data
  Train = sequence[1:splitPoint]
  Test <- sequence[(splitPoint+1):length(sequence)]
  name=deparse(substitute(sequence))
  TrainTestAssignPrint(name, Train, Test)
}

SplitMatrix=function(matrix, trainRatio=0.8){
  splitPoint<- floor(trainRatio*nrow(matrix)) #select the first 80% of the data
  Train = matrix[1:splitPoint,]
  Test <- matrix[(splitPoint+1):nrow(matrix),]
  name=deparse(substitute(matrix))
  TrainTestAssignPrint(name, Train, Test)
}

AICSIC=function(model){
  aic=AIC(model)
  sic = BIC(model)
  cat("AIC=", aic, "SIC=", sic)
}

PlotSeries=function(dataSeries, ylab=NULL,xlab=NULL,title=NULL, subtitle=NULL, caption=NULL){
  defaultTitle=deparse(substitute(dataSeries))
  if (is.null(title)){
    title=defaultTitle
  }
  autoplot(dataSeries)+
    labs(y=ylab,
         x=xlab,
         title=title,
         subtitle=subtitle,
         caption=caption)+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size=12),
          plot.caption = element_text(hjust = 0))
}

PlotAcfPacf=function(dataSeries, aTitle=NULL, pTitle=NULL){
  aPlot=ggAcf(dataSeries)+
    labs(title=aTitle)
  pPlot=ggPacf(dataSeries)+
    labs(title=pTitle)
  plot_grid(aPlot, pPlot, ncol=2)
}

PlotTsAcf=function(dataSeries, ylab=NULL,xlab=NULL,title=NULL, subtitle=NULL, caption=NULL, relativeHeight=c(1,.6), aTitle=NULL, pTitle=NULL){
  tRow=PlotSeries(dataSeries, ylab,xlab,title, subtitle, caption)
  bRow=PlotAcfPacf(dataSeries, aTitle, pTitle)
  plot_grid(tRow, bRow, ncol = 1, rel_heights = relativeHeight)
}

PlotAcfs.Title=function(dataSeries,title=NULL, subtitle=NULL, aTitle=NULL, pTitle=NULL){
  defaultTitle=deparse(substitute(dataSeries))
  if (is.null(title)){
    title=defaultTitle
  }
  bRow=PlotAcfPacf(dataSeries)
  tRow <- ggdraw() + draw_label(title, fontface='bold')
  plot_grid(tRow, bRow, ncol = 1, rel_heights = c(0.18,1))
}


SaveFigure=function(saveTag, type="_typeUndefined.png"){
  if (is.null(saveTag)==FALSE){
    ggsave(paste(saveTag, type, sep=""), width = 8, height=6, units = 'in')
    cat("\nsaving figure ", paste(saveTag, type, sep=""))
  }
}


getBoxLagList=function(series, boxLag=24, type='Ljung', fitdf=0){
  matrix=as.data.frame(matrix(nrow=2))
  for (blg in seq((1+fitdf),boxLag)){
    box=Box.test(series, lag = blg, type=type, fitdf = fitdf)
    #df=box$parameter[[1]]
    qstat=box$statistic[[1]]
    pvalue=box$p.value
    col=data.frame(v= c(round(qstat, 2), round(pvalue, 4)))
    names(col)=paste("Lag", blg, sep="")
    matrix=cbind( matrix, col)
  }
  rownames(matrix)=c('Ljung-Box', 'p-value')
  matrix[,1]=NULL
  print(matrix)
}

AdfBoxTest=function(series, boxLag=24, fitdf=0, boxList=FALSE){
  paste("for ", deparse(substitute(series)))
  print(tseries::adf.test(x=series, k=boxLag))
  if(boxList==TRUE){
    getBoxLagList(series, boxLag=boxLag, fitdf=fitdf)
  } else{
    print(Box.test(series, lag = boxLag, type="Ljung", fitdf = fitdf))
  }
  #lag 10 for non-seasonal. lag 2m for seasonal, m=units in a period
}

AnalyzeSeries=function(series,boxLag=24, ylab=NULL,xlab="Year",title=NULL, subtitle=NULL, caption=NULL, saveTag=NULL, seriesPlot=TRUE, aTitle=NULL, pTitle=NULL, relativeHeight=c(1,0.6)){
  if(is.null(title)){
    title=deparse(substitute(series))
  }
  plot(PlotTsAcf(series,title=title, ylab=ylab,xlab=xlab, subtitle=subtitle, caption=caption, aTitle = aTitle, pTitle = pTitle, relativeHeight=relativeHeight ))
  SaveFigure(saveTag, type="_Series.png")
  AdfBoxTest(series, boxLag = boxLag, boxList = TRUE)
}

EstimateModel=function(model){
  print(summary(model))
  print(coeftest(model))
  AICSIC(model)
}

AnalyzeModel=function(model, saveTag=NULL, boxLag=24, resTitle="Model Residuals", ylab=NULL){
  EstimateModel(model)
  cat("\n")
  assign('modelResiduals', model$residuals, envir = .GlobalEnv)
  plot(PlotTsAcf(model$residuals, title = resTitle, xlab = "Year", ylab))
  SaveFigure(saveTag, type="_ModelResiduals.png")
  cat("\nfor model's residuals")
  AdfBoxTest(model$residuals, boxLag=boxLag, fitdf = (length(model$coef)-1), boxList = TRUE)
}

GraphForecast=function(fc, testData, layerPlot=TRUE, model){
  graph=Plot1Layers(fc, testData)
  assign(paste(deparse(substitute(model)), "Plot1Layer", sep = "."), graph, envir = .GlobalEnv)
  cat("generating Layer1 Plot for", paste(deparse(substitute(model)), "Plot1Layer", sep = "."))
  if(layerPlot==TRUE){
    plot(graph)
  }
}

AnalyzeForecastRes=function(fc, model, testData, saveTag, title="Forecast Error", 
                            ylab=NULL, xlab="Year",relativeHeight=c(1,0.8), boxLag=24){
  testRes=testData-fc$mean
  cat("\nfor forecast error")
  AdfBoxTest(testRes, boxLag=boxLag, fitdf = 0, boxList = TRUE)
  print(forecast::accuracy(fc, testData))
  plot(PlotTsAcf(testRes, title = title, xlab=xlab, ylab=ylab,relativeHeight=relativeHeight))
  SaveFigure(saveTag, type="_ForecastError.png")
}

ForecastModel=function(model, testData, xregFuture=NULL, saveTag=NULL, layerPlot=TRUE, title="Forecast Error", 
                       ylab=NULL, xlab="Year", relativeHeight=c(1,.8), boxLag=24){
  fc= forecast(model, h=length(testData), xreg = xregFuture, level=c(80,95))
  GraphForecast(fc, testData, layerPlot, model)
  AnalyzeForecastRes(fc, model, testData, saveTag, title, ylab, xlab,relativeHeight=relativeHeight, boxLag=boxLag)
}



###ARIMA modeling with forecast and plots
OutputARIMAwithForecast = function(TrainData, order=c(0,0,0), xreg=NULL, testData, xregFuture=NULL, layerPlot=TRUE){
  cat(paste("for series", deparse(substitute(TrainData))))
  print(adf.test(TrainData))
  print(Box.test(TrainData, type="Ljung-Box", lag=24))
  plot(PlotTsAcf(TrainData))
  model=Arima(TrainData, order=order, xreg=xreg)
  #print(deparse(substitute(TrainData)))
  #cat("arima(x=",deparse(substitute(TrainData)), 'order=', order, "xreg=", deparse(substitute(xreg)), ")")
  print(summary(model))
  print(coeftest(model))
  #AICSIC(model)
  cat("\n")
  plot(PlotTsAcf(model$residuals, title = "Training Set Residuals", xlab = "Year"))
  cat("\nfor training set residuals")
  print(adf.test(model$residuals))
  print(Box.test(model$residuals, type="Ljung-Box", lag=24))
}

###Plot actual and predictions
Plot1Layers=function(forecastResults, TestSet, colorSet=c("blue","tan4","black",NULL)){
  TestSetResiduals=TestSet-forecastResults$mean
    autoplot(ts(c(forecastResults$x, TestSet), start=start(forecastResults$x), frequency = frequency(forecastResults$x)))+
    autolayer(forecastResults$fitted)+
    autolayer(forecastResults$residuals)+
    annotate("rect", fill = "grey", alpha = 0.5, 
               xmin = time(TestSet)[1], xmax = time(TestSet)[length(TestSet)],
               ymin = -Inf, ymax = Inf)+
    autolayer(forecastResults)+
    autolayer(TestSet)+
    #autolayer(TestSetResiduals)+
    scale_color_manual(values=colorSet)+
    theme(legend.position='none')+
    labs(title = forecastResults$method)
}



Plot2Labels = function(layerplot, ylab ="Units", xlab = "Year", subtitle = "Data Series", caption='P.I. (Prediction Interval)',title=layerplot$labels[1], yscale = 1){
  layerplot+
    labs(y=ylab, 
         x=xlab,
         subtitle=subtitle,
         title=title,
         caption=caption)+
    scale_y_continuous(labels=function(x)x/yscale)+
    theme(panel.background  = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.justification = c("right", "top"),
          axis.line = element_line(colour = "grey40"),
          plot.caption = element_text(hjust=0))
}

Plot3Annotate =function(labeledplot, xA=NULL, yA=NULL, xP=NULL, yP=NULL, xR=NULL, yR=NULL, xCI=NULL, yCI=NULL, xTT=NULL, yTT=NULL,
                        colorSet=c('black', 'blue', 'tan4', 'black', 'black'),
                        inverseCI=FALSE){
  CI='80%\n95%\nP.I.'
  if(inverseCI==FALSE){
    CI='P.I.\n95%\n80%'
  }
  labeledplot+
    annotate("text", 
             x =c(decimal_date(ymd(xA)), decimal_date(ymd(xP)), decimal_date(ymd(xR)), decimal_date(ymd(xCI)),decimal_date(ymd(xTT))), 
             #decimal_date(ymd("2000-03-01")) for annotate x axix position. 
             y =c(yA, yP, yR, yCI,yTT), 
             label = c('Actual','Model', "Residuals", CI, "Train  Forecast"), 
             color=colorSet,
             hjust=0.5)
}


getAICSICMatrix=function(data,ARMAOrder=c(0,1,2,3,4), diff=0, xreg=NULL, iterations=1000, trace=FALSE){
  AICmatrix=data.frame()
  SICmatrix=data.frame()
  List=data.frame()
  for (ar in ARMAOrder){
    AICrow=c()
    SICrow=c()
    for (ma in ARMAOrder){
      model=try(forecast::Arima(data, order = c(ar,diff,ma), xreg=xreg, method = 'ML',optim.control = list(maxit =iterations)))
      if(inherits(model, "try-error")){
        cat("Error in ARMA(",ar, diff, ma,") \n")
        AICrow[ma+1]="Error"
        SICrow[ma+1]="Error"
        Listrow=list("ARMA",ar, diff, ma, Inf, Inf)
        next
      }
      AICrow[ma+1]=AIC(model)
      SICrow[ma+1]=BIC(model)
      Listrow=list("ARMA",ar, diff, ma, AIC(model), BIC(model))
      List=rbind(List, Listrow)
      if (trace){
        cat("ARMA(",ar,",", diff, ",",ma,") ", "AIC:", AIC(model), "SIC:", BIC(model), "\n")
      }
    }
    AICmatrix=rbind(AICmatrix,AICrow)
    SICmatrix=rbind(SICmatrix,SICrow) 
  }
  names(AICmatrix)=c("AIC MA0",ARMAOrder[-1])
  names(SICmatrix)=c("SIC MA0",ARMAOrder[-1])
  rownames(AICmatrix)=c("AR0",ARMAOrder[-1])
  rownames(SICmatrix)=c("AR0",ARMAOrder[-1])
  assign(paste(deparse(substitute(data)), "AICmatrix", sep = "."), AICmatrix, envir = .GlobalEnv)
  assign(paste(deparse(substitute(data)), "SICmatrix", sep = "."), SICmatrix, envir = .GlobalEnv)
  print(paste(deparse(substitute(data)), "AICmatrix", sep = "."))
  print(AICmatrix)
  names(List)=c("ARMA","ar","i", "ma", "AIC", "SIC")
  print("Minimum AIC happens at")
  print(List[which.min(List$AIC),])
  print(paste(deparse(substitute(data)), "SICmatrix", sep = "."))
  print(SICmatrix)
  print("Minimum SIC happens at")
  print(List[which.min(List$SIC),])
}


RecursiveCrossValidation=function(dataTS, trainRatio=0.8, ar, ma, diff=0, lambda=NULL, xreg=NULL, xregFuture = NULL){
  splitPoint<- floor(trainRatio*length(dataTS)) #select the first 80% of the data
  initialTestData <- window(dataTS, start=time(dataTS)[splitPoint+1])
  predictList=c()
  for (timeUnits in 1:length(initialTestData)){
    TrainData=window(dataTS, end = time(dataTS)[splitPoint-1+timeUnits])
    model=Arima(TrainData, order=c(ar,diff,ma), lambda = lambda, xreg=xreg, method = "ML",optim.control = list(maxit =1000))
    actualData=window(dataTS, start=time(dataTS)[splitPoint+timeUnits],end = time(dataTS)[splitPoint+timeUnits])
    fc=forecast(model, h=1, xreg=xregFuture)
    predictList=c(predictList, fc$mean)
  }
  rmse=Metrics::rmse(initialTestData,predictList)
  return(rmse)
}


SimpleRMSE=function(dataTS, trainRatio=0.8, ar, ma, diff=0, lambda=NULL, xreg=NULL, xregFuture=NULL){
  splitPoint<- floor(trainRatio*length(dataTS))
  TrainData = window(dataTS, end = time(dataTS)[splitPoint])
  TestData <- window(dataTS, start=time(dataTS)[splitPoint+1])
  model=Arima(TrainData, order=c(ar,diff,ma), lambda = lambda, xreg=xreg, method = "ML",optim.control = list(maxit =1000))
  fc=forecast(model,h=length(TestData), xreg = xregFuture)
  rmse=Metrics::rmse(TestData,fc$mean)
  #print(forecast::accuracy(fc, TestData))
  return(rmse)
}

getTestSetRMSEMatrix=function(dataTS, trainRatio=0.8, ARMAOrder=c(0,1,2,3,4), diff=0, lambda=NULL, valuation="Simple", xreg=NULL, xregFuture=NULL){
  RMSEMatrix=data.frame()
  List=data.frame()
  for (ar in ARMAOrder){
    row=c()
    for (ma in ARMAOrder){
      if (valuation=="Simple"){
        rmse=try(SimpleRMSE(dataTS, trainRatio, ar, ma, diff, lambda, xreg=xreg, xregFuture = xregFuture))
      }
      if (valuation=="Recursive"){
        rmse=try(RecursiveCrossValidation(dataTS, trainRatio, ar, ma, diff, lambda, xreg=xreg, xregFuture = xregFuture))
      }
      if(inherits(rmse, "try-error")){
        cat("Error in ARMA(",ar,",",diff,",",ma,") \n")
        row[ma+1]="Error"
        Listrow=list("ARIMA",ar, diff, ma, Inf, Inf)
        next
      }
      cat("\nARIMA(",ar,",",diff,",",ma,")", "RMSE=",rmse)
      row[ma+1]=rmse
      Listrow=list("ARIMA",ar, diff, ma, rmse)
      List=rbind(List, Listrow)
    }
    RMSEMatrix=rbind(RMSEMatrix, row)
  }
  names(RMSEMatrix)=c("RMSE MA0",ARMAOrder[-1])
  rownames(RMSEMatrix)=c("AR0",ARMAOrder[-1])
  assign(paste(deparse(substitute(dataTS)), "Diff", diff, "RMSEMatrix", valuation, sep = "."), RMSEMatrix, envir = .GlobalEnv)
  print(paste(deparse(substitute(dataTS)), "Diff", diff, "RMSEMatrix", valuation, sep = "."))
  print(RMSEMatrix)
  names(List)=c("ARIMA","ar","i", "ma", "RMSE")
  print("Minimum RMSE happens at")
  print(List[which.min(List$RMSE),])
}

