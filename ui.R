# Define UI for application that draws a histogram
fluidPage(
  
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
                                      downloadButton("create.data", "Save data")),
                                    
                                    conditionalPanel(
                                      condition= "input.dataset == 'example'",
                                      plotOutput("exampleplot"),
                                      DTOutput("example"),
                                      downloadButton("example.data", "Save data"))),
                           
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

