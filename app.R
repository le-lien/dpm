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
# Run app
runApp("/home/lienle/Desktop/DPM/app", display.mode = "showcase")
