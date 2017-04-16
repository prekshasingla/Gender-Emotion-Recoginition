library(shiny)
library(tuneR)
library(seewave)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(warbleR)
library(xgboost)
library(e1071)

source('example.R')


function(input, output) {
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$file1, {
    content <- ''
    inFile <- input$file1
    
    if (grepl('.wav', tolower(inFile$name)) != TRUE) {
      content <- '<div class="shiny-output-error-validation">Please select a .WAV file to upload.</div>'
    }
    else if (!is.null(inFile)) {
      
      withProgress(message='Please wait ..', style='old', value=0, {
        inFile <- input$file1
        
        if (is.null(inFile))
          return(NULL)
        
        id <- sample(1:100000, 1)
        filePath <- paste0('./temp', sample(1:100000, 1), '/temp', id, '.wav')
        
        
        currentPath <- getwd()
        fileName <- basename(filePath)
        path <- dirname(filePath)
        
        dir.create(path)
        
        file.copy(inFile$datapath, filePath)
      
        content <- process(filePath)
        
        if (!is.null(content$graph1)) {
          output$graph1 <- content$graph1
          output$graph2 <- content$graph2
        }
        
        unlink(path, recursive = T)
        
        
        
        #list(content=formatResult(result), graph1=result$graph1, graph2=result$graph2)
        
      })
    }
    
    v$data <- formatResult(content)
    
  })
  
  
  output$content <- eventReactive(v$data, {
    HTML(v$data)
  })
  
  #output$contents <- renderTable({
    
   #})
  
  
  process <- function(path) {
    content1 <- list(label = 'Sorry, an error occurred.', prob = 0, data = NULL)
   # content2 <- list(label = '', prob = 0, data = NULL)
   #  content3 <- list(label = '', prob = 0, data = NULL)
    #content4 <- list(label = '', prob = 0, data = NULL)
    #content5 <- list(label = '', prob = 0, data = NULL)
    graph1 <- NULL
    graph2 <- NULL
    freq <- list(minf = NULL, meanf = NULL, maxf = NULL)
    
    
      incProgress(0.3, message = 'Processing voice ..')
      content1 <- gender(path)
     # incProgress(0.4, message = 'Analyzing voice 1/4 ..')
      #content2 <- gender(path, 2, content1)
      #incProgress(0.5, message = 'Analyzing voice 2/4 ..')
      #content3 <- gender(path, 3, content1)
      #incProgress(0.6, message = 'Analyzing voice 3/4 ..')
      #content4 <- gender(path, 4, content1)
      #incProgress(0.7, message = 'Analyzing voice 4/4 ..')
      #content5 <- gender(path, 5, content1)
      
      incProgress(0.8, message = 'Building graph 2/2 ..')
      
      wl <- 2048
      ylim <- 280
      thresh <- 5
      
      # Calculate fundamental frequencies.
      freqs <- fund(content1$wave, fmax=ylim, ylim=c(0, ylim/1000), threshold=thresh, plot=F, wl=wl)
      freq$minf <- round(min(freqs[,2], na.rm = T)*1000, 0)
      freq$meanf <- round(mean(freqs[,2], na.rm = T)*1000, 0)
      freq$maxf <- round(max(freqs[,2], na.rm = T)*1000, 0)
      
      graph1 <- renderPlot({
        fund(content1$wave, fmax=ylim, ylim=c(0, ylim/1000), type='l', threshold=thresh, col='red', wl=wl)
        x <- freqs[,1]
        y <- freqs[,2] + 0.01
        labels <- freqs[,2]
        
        subx <- x[seq(1, length(x), 4)]
        suby <- y[seq(1, length(y), 4)]
        sublabels <- paste(round(labels[seq(1, length(labels), 4)] * 1000, 0), 'hz')
        text(subx, suby, labels = sublabels)
        
        legend(0.5, 0.05, legend=c(paste('Min frequency', freq$minf, 'hz'), paste('Average frequency', freq$meanf, 'hz'), paste('Max frequency', freq$maxf, 'hz')), text.col=c('black', 'darkgreen', 'black'), pch=c(19, 19, 19))
        
      })
      
      incProgress(0.9, message = 'Building graph 2/2 ..')
      
      #graph2
      graph2 <- renderPlot({
        spectro(content1$wave, ovlp=40, zp=8, scale=FALSE, flim=c(0,ylim/1000), wl=wl)
      })
      
    
    #print(content1$label)
    
    
    #try catch end
    list(label=content1$label,cart=content1$cart,svm=content1$svm,emotion=content1$emotion, graph1=graph1, graph2=graph2)
    
    #list(content1=content1, content2=content2, content3=content3, content4=content4, content5=content5, graph1=graph1, graph2=graph2, freq=freq)
  }
  
  formatResult <- function(result) {
    html<-""
    html <- paste0(html, '<div class="detail-header">Details</div>')
    html <- paste0(html, 'Gender: ', result$label, '<i class="fa fa-info" aria-hidden="true" title="Gender"></i>,  ')
    html <- paste0(html, 'Emotion: ',result$emotion, '<i class="fa fa-info" aria-hidden="true" title="Emotion"></i> <br> ')
    html <- paste0(html, 'Model 1: ',result$cart, '<i class="fa fa-info" aria-hidden="true" title="Model1"></i> , ')
    html <- paste0(html, 'Model 2: ',result$label, '<i class="fa fa-info" aria-hidden="true" title="Model2"></i> , ')
    html <- paste0(html, 'Model 3: ',result$svm, '<i class="fa fa-info" aria-hidden="true" title="Model3"></i>  ')
    html <- paste0(html, '</div>')
    html
  }
  
  
}