library(shiny)
library(shinydashboard)
library(tsoutliers)
library(forecast)
library(dygraphs)
library(xts)
library(anomalous)
library(e1071)



ui <- dashboardPage(
  dashboardHeader(title = "TS Outlier Detection"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Single Time Series", tabName = "sts"),
      menuItem("Multiple Time Series", tabName = "mts")

    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "sts",
              fluidPage(
                
                # Title of the page
                titlePanel("Single Time Series Outlier Detection"),
                
                hr(),
                
                # Input data from file
                fileInput("file", label = "File input"),
                
                
                splitLayout(
                  checkboxGroupInput("oltype", label = "Choose outlier types", 
                                     choices = list("AO (Additive outlier)" = "AO", "LS (Level shift)" = "LS", "TC (Temporary change)" = "TC")),
                  
                  sliderInput("cval", "Outlier threshold (critical value)",
                              min=4, max=20, value=10, step=1),
                  
                  cellWidths = c("220px", "230px")
                ),
                
                hr(),
                br(),
                
                # Show the dygraph of the time series
                dygraphOutput("dygraph"),
                
                hr(),
                
                img(src="bigorb.png", height = 30, width = 30),
                "Shiny is a product of ", 
                span("RStudio", style = "color:blue")
              )
            
      ),
      
      # Second tab content
      tabItem(tabName = "mts",
              fluidPage(
                
                # Title of the page
                titlePanel("Abnormal Series Detection among Multiple Time Series "),
                
                hr(),
                
                # Input data from file
                fileInput("file2", label = "File input:"),
                
                hr(),
                
                # Show the dygraph of the time series
                dygraphOutput("dygraph2"),
                hr(),
                
                conditionalPanel(
                  condition = "output.fileUploaded",
                  
                  sliderInput("num_ots", "Moving the slider to decrease the number of outlier series to be displayed:",
                              min=1, max=31, value=31, step=1, width = "500px", ticks = FALSE),
                  
                  br(),
                  # Press the button to detect outliers
                  actionButton("go", "Detect outliers")
                ),
                
                #fluidRow(column(3, verbatimTextOutput("value"))),
                #fluidRow( verbatimTextOutput("value2"))
                
                hr(),
                
                img(src="bigorb.png", height = 30, width = 30),
                "Shiny is a product of ", 
                span("RStudio", style = "color:blue")
                
                
              )
      )
    )
  )
)










server <- function(input, output, session) { 
  
  InitSeries <- read.csv("./test6.csv",header = FALSE)
  
  
  D <- 8
  N <- 1200
  
  
  n <- 6   # event duration
  E <- rep(NA, n)   # initialization of event
  
  mx <- vector()
  for (i in 1:(D+1)){
    mx <- cbind(mx, InitSeries$V1[i:(i+391)])
  }
  

  ### Tab1 ###
  
  data <- reactive({
    return(read.csv(file = input$file[,4]))
  })
  
  ts <- reactive({
    #data <- read.csv(file = input$file[,4])
    return(xts(data()$value, order.by = as.POSIXct(data()$time, tz = "GMT")))
  })
  
  ol <- reactive({
    
    fit <- forecast::auto.arima(ts(), max.p=2, max.q=2, max.d=2)
    otypes <- c("AO","LS","TC")
    stage1 <- locate.outliers.oloop(y = ts(), fit = fit, types = otypes, cval = input$cval, maxit.iloop = 1)
    return(stage1$outliers)
  })
  
  output$dygraph <- renderDygraph({
    if (is.null(input$file)) return(NULL)
    #data <- read.csv(file = input$file[,4])
    
    olselected <- ol()[ol()$type %in% input$oltype,]
    
    # create dygraph of the series
    dyG <- dygraph(ts()) %>%
      dyRangeSelector()
    
    
    for (i in 1:length(olselected$ind)){
      dyG <- dyG %>% dyAnnotation(x = data()$time[olselected$ind[i]], text = olselected$type[i], width = 30, height = 20)
    }
    
    dyG
    
  })
  
  
  ### Tab2 ###
  
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$go, {
    v$doPlot <- input$go
  }) 
  
  observeEvent(input$file2, {
    v$doPlot <- FALSE
  }) 
  
  # Read muliple series data from file
  mts <- reactive({
    if (is.null(input$file2)) return(NULL)
    return(read.zoo(file = input$file2[,4], head = TRUE, sep = ",", FUN = as.POSIXct))
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(mts()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  # Find anomalies
  anom <- reactive({
    
    # Compute feature matrix including mean and variance
    feature_mx <- tsmeasures(mts())
    avg <-  colMeans(mts())
    variance <- apply(mts(), 2, var)
    feature_mx <- cbind(feature_mx, avg, variance)
    class(feature_mx) <- c("features", "matrix")
    
    return(anomaly(feature_mx, n = nrow(feature_mx), plot = FALSE))
  })
  
  
  
  
  
  output$dygraph2 <- renderDygraph({
    if (is.null(input$file2)) return(NULL)
    
    if (v$doPlot == FALSE) {
      dyG2 <- dygraph(mts()) %>%
        dyHighlight(highlightCircleSize = 0, 
                    highlightSeriesBackgroundAlpha = 0.3,
                    hideOnMouseOut = TRUE,
                    highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyCSS("./highlight_legend.css")
    } else {
      
      index <- anom()$index[1:input$num_ots]
      ots <- mts()[, index, drop = FALSE]
      
      # Display the dygraph
      dyG2 <- dygraph(ots) %>%
        dyHighlight(highlightCircleSize = 0, 
                    highlightSeriesBackgroundAlpha = 0.3,
                    hideOnMouseOut = TRUE,
                    highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyCSS("./highlight_legend.css")
    }
    
    dyG2
  
  })
  
  
  ### Tab3 ###
  
  #global varible on x-axis
  x <- 400
  df <- as.data.frame(mx)
  
  
  autoUpdate <- reactiveTimer(500,session)
  
  observe({
    autoUpdate()
    
    
    x <<- x + 1
    
    if (x > 500 && x < 520){
      y <<- sin(40*pi*x/N) + rnorm(1,0,0.1) + rnorm(1,0,0.5)
    } else {  y <<- sin(40*pi*x/N) + rnorm(1,0,0.1) }
    
    
    model <<- svm(V9 ~ ., data = df)
    new.data <<- append(as.numeric(df[392,][-1]),y)
    df <<- rbind(df[-1,], new.data)
    pred <<- predict(model, df[392,])
    
    # matching value
    V <<- df[392,D+1]-pred
    
    # occurence
    O = as.numeric(abs(V) > 0.4)
    
    # event
    E <<- append(E[-1],O)
    
    # number of surprises within event
    S <<- sum(E)
    
    
    
    
    #pass data to client as object - x & y are passed to client from server on every second
    variableToPassClient = sprintf('{"X":"%s", 
                                   "Y": "%s", "Z": "%s"                          
  }', x, y, O)

    session$sendCustomMessage(type="SendObjectToClientDynamicCallbackHandler",variableToPassClient)
})
  
  
}

shinyApp(ui, server)