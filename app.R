#Preparation

# List of packages for session
.packages = c("ggplot2", "plyr", "dplyr", "ggpubr","shiny","DT","simstudy","R2OpenBUGS")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# Define environments to save outputs
dpm <- new.env()
fitenvir <- new.env()
predictenvir <- new.env()

# Call functions
source("functions/simulate.R")
source("functions/fit_model.R")
source("functions/update_model.R")
source("functions/openbugs_predict.R")
source("functions/run_predict.R")
source("functions/summary.R")
source("functions/plot.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Dynamic prediction model - continuous outcome"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the example data set or create own data set ----
      selectInput("dataset", "Choose a data set to work with.",
                  list("Example data set"="example",
                       "Create a new data set"="create")),
      helpText("Input parameters for data simulation"),
      conditionalPanel(
        condition = "input.dataset == 'example'",
        verticalLayout(p(strong("Number of individuals = 50")),
                       p(strong("Fixed intercept = 15")),
                       p(strong("Fixed slope = 0.5")),
                       p(strong("Standard deviation of residuals = 10")),
                       p(strong("Expected value of random intercept = 0")),
                       p(strong("Standard deviation of random intercept = 10")),
                       p(strong("Expected value of random slope = 0")),
                       p(strong("Standard deviation of random slope = 5")),
                       p(strong("Correlation between 2 random effects = 0.70")),
                       p(strong("Average number of timepoints per individual = 10")),
                       p(strong("Minimal time distance between 2 timepoints (time units) = 0.5")),
                       p(strong("Maximal time distance between 2 timepoints (time units) = 3")),
                       p(strong("Random seed = 11")),
                       fluid=T)               ),
      conditionalPanel(
        condition = "input.dataset == 'create'",
        numericInput("nid","Number of individuals",50),
        
        numericInput("beta0","Fixed intercept",15),
        numericInput("beta1","Fixed slope",0.5),
        numericInput("sd.y","Standard deviation of residuals",10),
        numericInput("V0","Expected value of random intercept",0),
        numericInput("sigma0","Standard deviation of random intercept",10),
        numericInput("V1","Expected value of random slope",0),
        
        numericInput("sigma1","Standard deviation of random slope",5),
        sliderInput("rho", "Correlation between 2 random effects", 
                    min = -1, max = 1, value = 0.7,step=0.01),
        numericInput("ncount","Average number of timepoints per individual",10),
        numericInput("min.interval","Minimal time distance between 2 timepoints (time units)",0.5),
        numericInput("max.interval","Maximal time distance between 2 timepoints (time units)",3),
        numericInput("seed", "Random seed", 11, min = 1, max = .Machine$integer.max)
      )),
    
    # Main panel for displaying outputs ----
    mainPanel( tabsetPanel(type = "tabs",
                           tabPanel("Simulated data set",  
                                    helpText("Outcome over time"),
                                    
                                    conditionalPanel(
                                      condition= "input.dataset == 'create'",
                                      plotOutput("simplot"),
                                      DTOutput("simtab"),
                                      actionButton("create.data", "Save data")),
                                    
                                    conditionalPanel(
                                      condition= "input.dataset == 'example'",
                                      plotOutput("exampleplot"),
                                      DTOutput("example"),
                                      actionButton("example.data", "Save data"))),
                           
                           tabPanel("Model input", 
                                    helpText("Input parameters for prediction"),
                                    uiOutput("max.id.fit"),
                                    uiOutput("id.update"),
                                    
                                    
                                    
                                    helpText("MCMC setting parameters"),
                                    numericInput("ni","Number of total iterations per chain (including burn in)",1000),
                                    numericInput("nb","Length of burn in, i.e. number of iterations to discard at the beginning. ",500),
                                    numericInput("nt","Thinning rate. Must be a positive integer.",1),
                                    numericInput("nc","Number of Markov chains",3),
                                    actionButton("go","Submit"),
                                    textOutput("predict")
                           ),
                           
                           tabPanel("Output", 
                                    helpText("Prediction of future outcomes w.r.t. provided information. Red points display simulated data, grey points are available data in each data set. Predicted outcomes are shown as median and 95% prediction interval."),
                                    actionButton("goplot2","Show plot"),
                                    plotOutput("plot"),
                                    helpText("Prediction of future outcome at chosen timepoint in datasets with increasing amount of provided information (y axis). Predicted outcomes are shown as median and 95% prediction interval. Dotted line shows the value of the simulated outcome at the same timepoint."),
                                    uiOutput("timeID"),
                                    actionButton("goplot","Show plot"),
                                    plotOutput("summary")
                                    
                           )
    ))
    
  )
)




server <- function(input, output) {
  
  # Call simulated data, if "create" chosen
  simtab <- reactive({simulate(nid=input$nid,beta0=input$beta0,beta1=input$beta1,sd.y=input$sd.y,V0=input$V0,sigma0=input$sigma0,V1=input$V1,sigma1=input$sigma1,rho=input$rho,ncount=input$ncount,min.interval=input$min.interval,max.interval=input$max.interval,seed=input$seed)})
  
  observeEvent(input$create.data, {
    write.csv(simtab()[[2]],"output/simdata.csv",row.names = F)
  })
  
  
  
  output$simtab <- renderDT({simtab()[[2]]
  })
  
  
  simplot <- reactive({
    
    ggplot(aes(x=time,y=Ytime,group=id),data=simtab()[[2]])+
      geom_line()+
      geom_point()+
      theme_pubr()})
  output$simplot <- renderPlot({simplot()})
  
  # Call example data, if "example" chosen    
  example <- reactive({dpm$example[[2]]})
  
  observeEvent(input$example.data, {
    write.csv(example(),"output/simdata.csv",row.names = F)
  })
  
  
  
  output$example <- renderDT({example()})
  
  exampleplot <- reactive({
    
    ggplot(aes(x=time,y=Ytime,group=id),data=example())+
      geom_line()+
      geom_point()+
      theme_pubr()})
  output$exampleplot <- renderPlot({exampleplot()})
  
  
  output$max.id.fit <- renderUI({numericInput("max.id.fit","Number of individuals included in the first part of the data set. (This part will be used to estimate the fixed effects, residuals SD, correlation, and SD of the random effects)",input$nid-1,min=10,max=input$nid-1)})
  
  output$id.update<- renderUI({numericInput("id.update","Enter the individual ID, of whom the random effects and the future outcome will be predicted.",input$nid,min=11,max=input$nid)})
  
  #read <- reactive({dattab <-ifelse(input$dataset=="create",read.csv("output/create.csv"),read.csv("output/example.csv"))})
  #dattab <- renderTable(read())
  
  output$timeID<- renderUI({
    #dattab <-ifelse(input$dataset=="create",read.csv("output/create.csv"),read.csv("output/example.csv"))
    #write.csv(dattab,"output/simdata.csv",row.names = F)
    selectInput("time.plot","Enter the time point, from which the future outcome will be predicted.",choices=dattab$time[dattab$id==input$id.update] )})
  
  dattab <- read.csv("output/simdata.csv")
  
  # Run predict function
  predict <- eventReactive(input$go, {
    # reactive({
    withProgress(message = 'Running MCMC ...', value = 0, {
      for (i in dattab$time[dattab$id==input$id.update]) {run_predict(dattab=dattab,max.id.fit=input$max.id.fit,id.update=input$id.update,start.time.update=i,seed=input$seed,nt=input$nt,nc=input$nc,nb=input$nb,ni=input$ni)
        cat(i)
        # Incremental Progress Bar (add some more info if neccessary)
        incProgress(1/max(dattab$time[dattab$id==input$id.update]),detail  = paste0("First timepoint with outcome to be predicted: ",i))
        
        # Pause
        Sys.sleep(0.1)
        
      }
      
    })
  })
  output$predict <- renderText({predict()})
  
  summary.func <- eventReactive(input$goplot, {
    summaryfunc(dattab=dattab,re="y",re0="Ytime",max.id.fit=input$max.id.fit,id.update=input$id.update,time.plot=as.numeric(input$time.plot))})
  
  output$summary <- renderPlot({summary.func()})
  
  plot.func <- eventReactive(input$goplot2, {plotfunc(dattab=dattab,re="y",re0="Ytime",max.id.fit=input$max.id.fit,id.update=input$id.update)})
  output$plot <- renderPlot({plot.func()})
}

# Run app
shiny::shinyApp(ui,server)
