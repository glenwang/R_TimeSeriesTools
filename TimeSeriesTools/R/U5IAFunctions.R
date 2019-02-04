#depends on U3IAFunctions.R

SumNameACol=function(rmseCol, lag){
  rmseCol=c(rmseCol, sum(rmseCol))
  col=as.data.frame(rmseCol)
  names(col)=paste("Lag", lag, sep="")
  return(col)
}

getVARfcRMSE=function(fc, testDataTSMatrix, endoNamesVector, lag){
  i=0
  rmseCol=c()
  for (s in endoNamesVector){
    i=i+1
    testd=testDataTSMatrix[,c(endoNamesVector[i])]
    txt=paste('fc$forecast$', endoNamesVector[i], '$mean', sep = "")
    fcmean=eval(parse(text=txt))
    rmseCol[i]=round(Metrics::rmse(testd, fcmean),2)
  }
  rmseCol=SumNameACol(rmseCol, lag)
  return(rmseCol)
}

getVARfcInvDiffRMSE=function(fc, unDiffedTestDataTSMatrix, unDiffedTraintDataTSMatrix, endoNamesVector, endoNamesVectorUnDiff, lag){
  i=0
  rmseCol=c()
  for (s in endoNamesVector){
    i=i+1
    testd=unDiffedTestDataTSMatrix[,c(endoNamesVectorUnDiff[i])]
    txt=paste('fc$forecast$', endoNamesVector[i], '$mean', sep = "")
    fcmean=eval(parse(text=txt))
    #print(endoNamesVectorUnDiff[i])
    xi=unDiffedTraintDataTSMatrix[,c(endoNamesVectorUnDiff[i])][nrow(unDiffedTraintDataTSMatrix)]
    fcmeanUnDiff=window(diffinv(fcmean, xi=xi), start = time(fcmean)[1])
    rmseCol[i]=round(Metrics::rmse(testd, fcmeanUnDiff),2)
  }
  rmseCol=SumNameACol(rmseCol, lag)
  return(rmseCol)
}

renameRMSEMatrix=function(rmseMatrix, seriesNames){
  rmseMatrix[,1]=NULL
  rownames(rmseMatrix)=c(seriesNames, 'Sum')
  cat(" GlobalEnv: rmseMatrix \n")
  print(rmseMatrix)
  assign("rmseMatrix", rmseMatrix, envir = .GlobalEnv)
}

getEndo=function(endoNamesVector, trainData){
  endoVar=trainData[,endoNamesVector]
  return(endoVar)
}

getExoDum=function(NamesVector, TSMatrix){
  EDVar=NULL
  if(is.null(NamesVector)==FALSE){
    EDVar=data.frame(TSMatrix[,NamesVector])
    colnames(EDVar)=NamesVector
  }
  return(EDVar)
}

printNotice=function(endoNamesVector, exogenNamesVector){
  cat("\n", length(endoNamesVector), 'endogenous variables: ')
  print(endoNamesVector)
  cat("", length(exogenNamesVector), "exogenous variables: ")
  print(exogenNamesVector)
}

printSeasonType=function(season, type){
  if (is.null(season)){
    cat(' season= NULL. ')
  } else{
    cat(' season=',season, " ")
  }
  cat(" type=", type, "\n")
}

printfcLagInspection=function(inspectLag, l, md, fc){
  if ( is.null(inspectLag)==FALSE) {
    if(l==inspectLag){ 
      mdname=paste('inspectmodel.lag',l,sep="")
      fcname=paste('inspectforecast.lag',l,sep="")
      assign(mdname, md, envir = .GlobalEnv)
      assign(fcname, fc, envir = .GlobalEnv)
      cat(" GlobalEnv:", mdname, fcname)
    }
  }
}

checkunDiffreturnrmseCol=function(unDiff, fc,testDataTSMatrix,endoNamesVector,endoNamesVectorUnDiff,l, unDiffedTestDataTSMatrix,unDiffedTraintDataTSMatrix ){
  if (unDiff==FALSE){
    rmseCol=getVARfcRMSE(fc, testDataTSMatrix, endoNamesVector, l)      
  }else{
    rmseCol=getVARfcInvDiffRMSE(fc, unDiffedTestDataTSMatrix, 
                                unDiffedTraintDataTSMatrix, endoNamesVector, 
                                endoNamesVectorUnDiff, lag=l)
  }
}

checkIfNULLElseFC=function(endoVariable, l, type,season, testDataTSMatrix, exoVar, dumVariable, inspectLag){
  if(is.null(exoVar)){
    md=vars::VAR(endoVariable, p=l, type = type, season=season, exogen = NULL)
    fc=forecast::forecast(md, h =nrow(testDataTSMatrix), dumvar=NULL)
  } else{
    internalExoVar=exoVar
    assign("internalExoVar", internalExoVar, envir = .GlobalEnv)
    md=vars::VAR(endoVariable, p=l, type = type, exogen=internalExoVar, season=season)
    fc=forecast::forecast(md, h =nrow(testDataTSMatrix), dumvar=dumVariable)
  }
  printfcLagInspection(inspectLag, l, md, fc)
  return(fc)
}

getVARRMSEMatrix=function(endoVariable, exoVar, dumVariable, season=NULL, testDataTSMatrix, uptoLag=10, type='none', inspectLag=NULL, unDiff=FALSE, endoNamesVectorUnDiff,unDiffedTestDataTSMatrix,unDiffedTraintDataTSMatrix){
  endoNamesVector=colnames(endoVariable)
  rmseMatrix=as.data.frame(matrix(nrow = (length(endoNamesVector)+1)))
  printSeasonType(season, type)
  for (l in seq(1:uptoLag)){
    fc=checkIfNULLElseFC(endoVariable, l, type,season, testDataTSMatrix, exoVar, dumVariable=dumVariable, inspectLag)
    rmseCol=checkunDiffreturnrmseCol(unDiff, fc,testDataTSMatrix,endoNamesVector,endoNamesVectorUnDiff,l, unDiffedTestDataTSMatrix,unDiffedTraintDataTSMatrix )
    rmseMatrix=cbind(rmseMatrix,rmseCol)
  }
  return(rmseMatrix)
}

AnalyzeVARForecast=function(endoNamesVector, exogenNamesVector, trainData, testDataTSMatrix, season=NULL, uptoLag=10, type='none', inspectLag=NULL,
                            unDiff=FALSE,endoNamesVectorUnDiff=NULL,unDiffedTestDataTSMatrix=NULL,unDiffedTraintDataTSMatrix=NULL){
  printNotice(endoNamesVector, exogenNamesVector)
  endoVariables=getEndo(endoNamesVector, trainData)
  exogenVariables=getExoDum(exogenNamesVector, trainData)
  dumVariables=getExoDum(exogenNamesVector, testDataTSMatrix)
  rmseMatrix=getVARRMSEMatrix(endoVariable=endoVariables, 
                              exoVar=exogenVariables, dumVariable=dumVariables, 
                              season=season, testDataTSMatrix=testDataTSMatrix, 
                              uptoLag=uptoLag, type=type, inspectLag=inspectLag, 
                              unDiff=unDiff, endoNamesVectorUnDiff=endoNamesVectorUnDiff,
                              unDiffedTestDataTSMatrix=unDiffedTestDataTSMatrix,
                              unDiffedTraintDataTSMatrix=unDiffedTraintDataTSMatrix)
  if(unDiff){
    renameRMSEMatrix(rmseMatrix, endoNamesVectorUnDiff)
  }else{
    renameRMSEMatrix(rmseMatrix, endoNamesVector)
  }
}

loopAnalyzeVARfc=function(inputList,TrainSet, TestSet, season=NULL, type='trend', inspectLag=NULL, uptoLag=12){
  i=0
  for (C in inputList){
    i=i+1; cat("\nNo.", i, "in the parameter list")
    endoNV=C[[1]]
    exogenNV=C[[2]]
    AnalyzeVARForecast(endoNV, exogenNV, TrainSet, TestSet, season=season, type=type, inspectLag=inspectLag, uptoLag = uptoLag)
  }
}


invDiffFCError=function(inspectfc, endoNamesVector, endoNamesVectorUnDiff, unDiffedTestDataTSMatrix, unDiffedTraintDataTSMatrix){
  i=0
  fc=inspectfc
  fcUnDiffList=list()
  for (s in endoNamesVector){
    i=i+1
    testd=unDiffedTestDataTSMatrix[,c(endoNamesVectorUnDiff[i])]
    txt=paste('fc$forecast$', endoNamesVector[i], '$mean', sep = "")
    fcmean=eval(parse(text=txt))
    xi=unDiffedTraintDataTSMatrix[,c(endoNamesVectorUnDiff[i])][nrow(unDiffedTraintDataTSMatrix)]
    fcmeanUnDiff=window(diffinv(fcmean, xi=xi), start=time(fcmean)[1])
    fcUnDiffList[[endoNamesVectorUnDiff[i]]]=fcmeanUnDiff
    fcUnDiffList[[paste(endoNamesVectorUnDiff[i],'.FCerror', sep = "") ]]=testd-fcmeanUnDiff
    
  }
  return(fcUnDiffList)
}

plotFC=function(TSMatrix, fcIDifList, name, xA=NULL, yA=NULL, xF=NULL, yF=NULL, ylab="", xlab='Year', subtitle=NULL, title=NULL, caption=NULL, colorSet=('blue')){
  autoplot(TSMatrix[,c(name)])+
    autolayer(fcIDifList[[name]])+
    labs(y=ylab, 
         x=xlab,
         subtitle=subtitle,
         title=title,
         caption=caption)+
    scale_color_manual(values=colorSet)+
    theme(panel.background  = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = 'none',
          axis.line = element_line(colour = "grey40"),
          plot.caption = element_text(hjust=0))+
    annotate('text', x =c(decimal_date(ymd(xA)), decimal_date(ymd(xF))),
             y=c(yA, yF),
             label=c('Acutal', "Forecast"),
             color=c('black', colorSet))
}
