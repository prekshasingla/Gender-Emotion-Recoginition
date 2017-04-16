library(shiny)

fluidPage(
  titlePanel("Gender and Emotion Recognition System"),
    mainPanel(
      fileInput('file1', 'Choose WAV File', accept = c('audio/wav'), width = '100%'),
      tags$hr(),
      div(id='result', style='font-size: 22px;', htmlOutput('content')),
      
      conditionalPanel(condition='output.content != null',
                       tabsetPanel(id='graphs',
                                   tabPanel('Frequency Graph', plotOutput("graph1", width=900, height=450)),
                                   tabPanel('Spectrogram', plotOutput("graph2", width=900, height=450))
                               ),
      
                     div(style='margin: 20px 0 0 0;')
      )
    )
  )
