plotTimeseries<-function( timeseriesToPlotListXTS, chartTitle=NULL, chartTitleSize=16, 
                          showLegend=TRUE, legendPosition="bottom", 
                          xAxisLabel=NULL, yAxisLabel=NULL, legendLabel="", showPoints=FALSE, normalize=FALSE, 
                          normalizeDate=NULL, datesToHighlightDF=NULL ) {
  # if passing in datesToHighlightDF, column1 should be named "start" and column2 named "end"
  # EXAMPLE:  
  #   startEndDF=data.frame("start"=c("2007-01-03","2009-1-1"),end=c("2007-02-14","2009-12-12"), stringsAsFactors=FALSE)
  
  timeseriesListXTS<-timeseriesToPlotListXTS
  if( normalize ) {
    yAxisLabelStr<-paste(yAxisLabel,"(Normalized)")
  } else {
    yAxisLabelStr<-yAxisLabel
  }
  # first determine whether we need to plat charts normalized to a certain date. 
  # ie: plot timeseries that all start at 100 on the same start date
  if( normalize ) {
    if( is.null(normalizeDate) ) {
      # if no normalizeDate is passed, then just start each new timeseries at 100 on earliest date available in each timeseries
      for( i in 1:length(timeseriesToPlotListXTS) ) {
        timeseriesListXTS[[i]]<-cumprod( 1 + dailyReturn(timeseriesToPlotListXTS[[i]]) ) - 1
        colnames(timeseriesListXTS[[i]])<-colnames(timeseriesToPlotListXTS[[i]])
      }
    } else {
      
    }
  } else {
    timeseriesListXTS<-timeseriesToPlotListXTS
  }
  
  allTimeseriesDF<-data.frame()
  for( i in 1:length(timeseriesListXTS) ) {
    tempXTS<-timeseriesListXTS[[i]]
    tempDF<-as.data.frame(tempXTS)
    tempDF$Date<-as.Date(rownames(tempDF))
    #     if( is.null(colnames(tempXTS)) ) {
    #       colnames(tempDF[,1])<-paste("Timeseries", i, sep="_")
    #     }
    if( is.null(names(tempXTS)) ) {
      names(tempDF[,1])<-paste("Timeseries", i, sep="_")
    }
    tempMelt <- melt(tempDF, id="Date")
    if( i == 1 ) { 
      allTimeseriesDF <- tempMelt
    } else {
      allTimeseriesDF <- rbind(allTimeseriesDF,tempMelt)  
    }
  }
  localEnv <- environment()
  #below uses pallette from library(RColorBrewer)
  ggPlotFormula<-ggplot(data=allTimeseriesDF, 
                        aes(x=Date,y=value,colour=variable,group=variable)) + 
    geom_line() + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(legend.position=legendPosition) +
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11)) + 
    #    scale_color_brewer(palette = "Dark2") + 
    #    scale_color_brewer(palette = "Set1") +
    theme(plot.title=element_text(size=chartTitleSize)) 
  
  if( length( timeseriesToPlotListXTS ) > 1 & length( timeseriesToPlotListXTS ) < 10 ){
    ggPlotFormula<-ggPlotFormula + 
      scale_color_brewer(palette = "Set1")
  }
  
  if( !is.null(datesToHighlightDF) ){
    ggPlotFormula<-ggPlotFormula +
      geom_rect(data=datesToHighlightDF, aes(xmin=as.Date(start), xmax=as.Date(end), ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.30, inherit.aes=FALSE, show_guide=FALSE)
  }
  
  if( showPoints ) {
    ggPlotFormula<-ggPlotFormula + 
      geom_point(aes(size=4))
  }
  
  if( showLegend ) {
    ggPlotFormula<-ggPlotFormula +
      labs(colour=legendLabel) + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) 
  } else {
    ggPlotFormula<-ggPlotFormula + 
      theme(legend.position="none")
  }
  
  return(ggPlotFormula)
}

plotSingleTimeseries<-function(timeseriesXTS, startDate=NULL, endDate=NULL, 
                               xAxisLabel="Dates", yAxisLabel="Values"){
  tempDF<-as.data.frame(timeseriesXTS)
  
  startIndex<-NULL
  endIndex<-NULL
  
  # to plot by specifying startdate and enddate
  if( is.null(startDate) ) {
    startIndex<-1  
  } else {
    startIndex<-which(index(timeseriesXTS)==startDate)
    if( is.na(startIndex[1]) ) {
      print(paste("the startDate you specified,", startDate, ", is not in the timeseries"))
      return()
    }
  }
  if( is.null(endDate) ) {
    endIndex<-length(timeseriesXTS)
  } else {
    endIndex<-which(index(timeseriesXTS)==endDate)
    if( is.na(endIndex[1]) ) {
      print(paste("the endDate you specified,", endDate, " is not in the timeseries"))
      return()
    }
  }
  
  rowsToPlot<-seq(startIndex,endIndex)
  qplot(index(timeseriesXTS)[rowsToPlot], tempDF[,1][rowsToPlot], geom="line", xlab=xAxisLabel, ylab=yAxisLabel)
  
  # basic line plot with no dates
  #   qplot(index(tempDF),tempDF[,4], geom="line")
  # with dates (automatically chooses the "scale" which to show dates --ie: if timeseries is long, will only show yearly dates on x-axis)
  #   qplot(index(AAPL),AAPLdf[,4], geom="line")
  # another way to choose how much to show in chart
  #   qplot(index(AAPL)[1:5],AAPLdf[,4][1:5], geom="line")
  
}
plotHistogram<-function( values, barColor="Blues", barColorDarkness=3, chartTitle=NULL, chartTitleSize=16, 
                         showMeanLine=FALSE, showStDevLines=FALSE, 
                         xAxisLabel=NULL, yAxisLabel="Count", 
                         showHorizontalBars=FALSE,
                         useDefaultName=FALSE) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-as.data.frame(values)
  colnames(DF)<-"x"
  localEnv <- environment()
  xAxisLabelStr <- NULL
  yAxisLabelStr <- yAxisLabel
  
  if( useDefaultName ){
    xAxisLabelStr <-  names(values)
  } else {
    if( !is.null(xAxisLabel) ) {
      xAxisLabelStr <- xAxisLabel
    }  
  }
  
  
  ggPlotFormula<-ggplot(data=DF, aes(x=x)) + 
    scale_color_brewer(type="seq") +
    geom_histogram(fill=paste(mypalette[barColorDarkness]),colour="grey30") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  if( showMeanLine ){
    tempMean=mean(values)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.5)
    if( showHorizontalBars ){
      ggPlotFormula<-ggPlotFormula + 
        annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=1.05, size=3.5, color="grey40" )        
    } else {
      ggPlotFormula<-ggPlotFormula + 
        annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=-0.1, size=3.5, color="grey40" )        
    } 
  }
  if( showStDevLines ){
    tempMean<-mean(values)
    if( class(values)[1] == "xts" || class(values)[1] == "zoo" ){
      tempStdDev<-apply(values,2,sd)  
    } else {
      tempStdDev<-sd(values)
    }
    if( showHorizontalBars ){
      ggPlotFormula<-ggPlotFormula + 
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" )
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" )
    }
  }
  return(ggPlotFormula)
}

plotBarChart<-function( values, categoryNames, barColor="Blues", barColorDarkness=4, 
                        chartTitle=NULL, chartTitleSize=18, xAxisLabel=NULL, yAxisLabel="Values", 
                        showMeanLine=FALSE, showStDevLines=FALSE, showHorizontalBars=FALSE, sortAsc=FALSE, sortDesc=FALSE ) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-data.frame(xVal=values, categoryNames=categoryNames)
  localEnv <- environment()
  yAxisLabelStr<-yAxisLabel
  if( !sortAsc && !sortDesc ){
    ggPlotFormula<-ggplot(data=DF, aes(x=categoryNames, y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
  } else {
    if( sortAsc ){
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) 
    } else {
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
    } 
  }
  #  ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) + 
  ggPlotFormula<-ggPlotFormula +
    geom_bar(fill=paste(mypalette[barColorDarkness]),colour="grey30",stat="identity") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  
  if( showMeanLine ){
    tempMean<-mean(values)
    minXcoord<-min(values)
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = tempMean, colour=paste(mypalette[barColorDarkness+2]), linetype = "dashed", size=1.0, alpha=1.0) +
      annotate(geom="text", x=minXcoord, y=tempMean, label="avg", vjust=-0.5, hjust=-0.1, size=5, color="grey20" )  
  }
  
  if( showStDevLines) {
    
    tempMean<-mean(values)
    minXcoord<-min(values)
    if( class(values)[1] == "xts" || class(values)[1] == "zoo" ){
      tempStdDev=apply(values,2,sd)  
    } else {
      tempStdDev=sd(values)
    }
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
      annotate(geom="text", x=minXcoord, y=tempMean-tempStdDev, label="-1sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean+tempStdDev, label="+1sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean-2*tempStdDev, label="-2sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean+2*tempStdDev, label="+2sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" )
  }
  
  return(ggPlotFormula)
} 

plotScatterplot<-function( xValues, yValues, pointLabelNames=NULL, showPointLabels=FALSE, 
                           barColor="Blues", barColorDarkness=5, pointSize=4, 
                           chartTitle=NULL, chartTitleSize=16,  
                           xAxisLabel=NULL, yAxisLabel=NULL, 
                           showRegressionLine=FALSE, showConfidenceInterval=FALSE, 
                           showRegressionAdjRSQ=FALSE, labelPositionRSQ="upper-left", labelRSQfontSize=6, 
                           showXmeanLine=FALSE, showYmeanLine=FALSE, 
                           showXstDevLines=FALSE, showYstDevLines=FALSE, 
                           labelPositionRSQoverrideX=0, labelPositionRSQoverrideY=0,
                           pointColorValues=NULL,
                           lowShadingColor=NULL,
                           highShadingColor=NULL,
                           legendLabel=""
) {
  # parameter "barColor" can be 'Blues', 'Greens', 'Reds'
  
  # inputs "xValues", "yValues", and "pointLabelNames" should be vectors all of the same length
  if( is.null(pointLabelNames) && showPointLabels==TRUE ){
    print("You specified showPointLabels=TRUE but did not pass in a vector of point label names to the pointLabelNames parameter.")
    return()
  }
  
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(8, barColor)
  
  #DF<-as.data.frame(values)
  if( showPointLabels ) {
    DF<-data.frame(x=xValues, y=yValues, pointLabels=pointLabelNames)
    colnames(DF)<- c("x","y","pointLabels")
  } else {
    DF<-data.frame(x=xValues, y=yValues)
    colnames(DF)<- c("x","y")
  }
  
  if( ! is.null(pointColorValues) ){
    DF<-cbind( DF, pointColors=pointColorValues )
  } else {
    DF<-cbind( DF, pointColors=barColorDarkness )
  }
  
  localEnv <- environment()
  
  yAxisLabelStr<-yAxisLabel
  ggPlotFormula <- NULL
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y ) )
  } else {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y, color=pointColors))
  }
  ggPlotFormula <- ggPlotFormula + 
    # scale_color_brewer(type="seq") +
    # geom_point(show_guide=FALSE, shape=19, size=pointSize) + 
    #geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize) + 
    # geom_point(color=pointColorValues, show_guide=FALSE, shape=19, size=pointSize) + 
    # scale_color_gradient(colours=rainbow(7))
    ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) + 
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    #    geom_text(data=DF, aes(label=pointLabels), vjust=-0.8) +
    theme(plot.title=element_text(size=chartTitleSize)) 
  
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggPlotFormula +
      geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize)
  } else {
    ggPlotFormula <- ggPlotFormula +
      geom_point(show_guide=FALSE, shape=19, size=pointSize) +
      labs(colour=legendLabel) + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="bottom")
    
    if( !is.null(lowShadingColor) & !is.null(highShadingColor) ) {
      ggPlotFormula <- ggPlotFormula + 
        scale_color_gradient(low=lowShadingColor, high=highShadingColor)
    }
  }
  
  if( showPointLabels ){
    ggPlotFormula<-ggPlotFormula +
      geom_text(data=DF, aes(label=pointLabels), vjust=-0.8)
  }
  if( showRegressionLine ){
    if( showConfidenceInterval ){
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", fill="pink", alpha=0.25, method=lm, show_guide=FALSE)
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", method=lm, se=FALSE, show_guide=FALSE)
    }
  }
  if( showRegressionAdjRSQ ) {
    tempLM<-lm(DF[,2] ~ DF[,1])
    tempAdjRSQ<-summary(tempLM)$adj.r.squared
    tempAdjRSQ<-round(tempAdjRSQ,2)
    
    if( labelPositionRSQ=="lower-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =", tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=0.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="lower-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=-0.5+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    
  }
  
  if( showXmeanLine ){
    tempMean=mean(xValues)
    minYcoord<-min(DF[,2])
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=tempMean, y=minYcoord, label="avg(X)", angle=90, vjust=-1.0, hjust=0.2, size=4, color="grey40" )  
  }
  if( showYmeanLine ){
    tempMean=mean(yValues)
    minXcoord<-min(DF[,1])
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=minXcoord, y=tempMean, label="avg(Y)", vjust=-1.0, hjust=0.1, size=4, color="grey40" )  
  }
  
  if( showXstDevLines ){
    tempMean=mean(xValues)
    tempStdDev=sd(xValues)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  if( showYstDevLines ){
    #     tempMean=mean(yValues)
    tempStdDev=sd(yValues)
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  return(ggPlotFormula)
}


