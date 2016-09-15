# global.R script for PrototypeAPAP3
# Objects that are not reactive are written here
# This also a safe place for functions that are then used in server.R
# ------------------------------------------------------------------------------
# Load package libraries
  library(shiny)
  library(shinydashboard)  # Package for making cooler user-interface for Shiny applications
  library(shinyjs)  # Package for perform common JavaScript operations
  library(ggplot2)  # Plotting
  library(grid)   #Plotting
  library(plyr)  # Split and rearrange data, ddply function
  library(dplyr)  # New plyr
  library(rmarkdown)  # Generate report to a Word, pdf or HTML document
  library(mrgsolve) # Metrum differential equation solver for pharmacometrics
# Define a custom ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 16))
# Source model code
  source("model.R")

# ------------------------------------------------------------------------------
# Define time sequence
  TIME.base <- c(seq(from = 0,to = 3,by = 0.5),
                seq(from = 4,to = 12,by = 2),
                seq(from = 16,to = 32,by = 8))
# Set the number of individuals that make up the 95% confidence intervals
  n <- 1000
# 95% confidence interval functions
  CI95lo <- function(x) quantile(x,probs = 0.025)
  CI95hi <- function(x) quantile(x,probs = 0.975)
# Set seed for reproducible numbers
  set.seed(123456)

# Update model code for Bayesian forecasting (now called "mod2")
  # Update the omega values in model code - omega here are NOT between-subject variability but the precision of the parameters
  # For the individual line - there is no error
    omega.list <- list(ETA_CL = 0,ETA_V = 0,ETA_KA = 0,ETA_F = 0)	# Simulating only the individual based on the Bayes estimates - no need for variability
  # Formally update the model parameters
    mod2 <- mod %>% carry.out(amt,PAC) %>% omat(dmat(omega.list))

# ------------------------------------------------------------------------------
# Fit individual parameters given the observed concentrations, estimated doses and covariate values
  bayesian.function <- function(input.data) {
    # Initial parameter estimates
      initial.par <- c(exp(0),exp(0),exp(0),exp(0)) # Population values as initial estimates
      par <- initial.par
    # Observation - for the posterior
      Yobs <- input.data$PAC[is.na(input.data$PAC) == F]  # Most of this will be NA except for the samples
      input.conc.data <- input.data[input.data$time == 0 | is.na(input.data$PAC) == F,]

    # Function for estimating individual parameters by minimising the Bayesian objective function value
      bayesian.ofv <- function(par) {
        ETA1fit <- log(par[1])  # Bayesian estimated ETA for clearance
        ETA2fit <- log(par[2])  # Bayesian estimated ETA for volume
        ETA3fit <- log(par[3])  # Bayesian estimated ETA for absorption rate constant
        ETA4fit <- log(par[4])  # Bayesian estimated ETA for bioavailability
        # List of ETA values that will be optimised - these will updated and connected to ERR_X terms in the mrgsolve model code
          input.conc.data$ERR_CL <- ETA1fit
          input.conc.data$ERR_V <- ETA2fit
          input.conc.data$ERR_KA <- ETA3fit
          input.conc.data$ERR_F <- ETA4fit
        # Simulate concentration-time profile with nth iteration of ETA values
          conc.data <- mod2 %>% data_set(input.conc.data) %>% mrgsim()
          conc.data <- as.data.frame(conc.data) # Convert to a data frame

        # If Yobsx was NA, then Yhat needs to be NA too (for calculating the log-likelihood)
          Yhat <- conc.data$IPRE[is.na(conc.data$PAC) == F]  # Make a Yhat vector based on IPRE in conc.data
        # Posterior component (from the data)
        # Log densities of residuals
        # Residual error model, Y = IPRE*(1+ERR), Y = IPRE + IPRE*ERR
          ERRPRO <- sqrt(as.matrix(smat(mod)))  # Pull out SIGMA from "mod" - original model code
          loglikpost <- dnorm(Yobs,mean = Yhat,sd = Yhat*ERRPRO,log = T)
        # Prior component (from the model)
          ETA <- c(ETA1fit,ETA2fit,ETA3fit,ETA4fit) # List of Bayesian estimated ETAs
          ETABSV <- as.matrix(omat(mod)) # PPV for model parameters in "mod" - original model code
          ETABSV <- sqrt(c(ETABSV[1,1],ETABSV[2,2],ETABSV[3,3],ETABSV[4,4]))  # Pull out the OMEGAs and convert to SDs
          loglikprior <- dnorm(ETA,mean = 0,sd = ETABSV,log = T)
        # Calculate the combined likelihood
          OFVBayes <- -1*sum(loglikpost,loglikprior)
          OFVBayes
      }
    # Optimise the ETA parameters to minimise the OFVBayes
      resultfit <- optim(par,bayesian.ofv,hessian = TRUE,method = "L-BFGS-B",lower = c(0.001,0.001,0.001,0.001),upper = c(Inf,Inf,Inf,Inf),control = list(parscale = par,factr = 1e7))
  }

# ------------------------------------------------------------------------------
# Functions for applying various decision rules to Bayes estimated concentration profiles
  TIME <- 4:24  # Times that the Rumack-Matthew nomogram can only be applied to
  TIME.rm <- seq(from = 0,to = max(TIME.base),by = 0.25)
# Rumack-Matthew Nomogram
  CONCrm <- 300*exp(-log(2)/4*TIME.rm)
  rule.data <- data.frame(TIME = TIME.rm,CONCrm)

# Function for flagging if an individual should receive NAC or not based on Rumack-Matthew Nomogram
# Function for BAYESIAN FORECASTED PAC
  rm.function <- function(input.data) {
    PAC_TIME <- input.data$time[1]
    input.data$NAC_DEC <- 0
    input.data$NAC_DEC[input.data$IPRE[input.data$time == PAC_TIME] > rule.data$CONCrm[rule.data$TIME == PAC_TIME]] <- 1
    if (PAC_TIME < 4) {
      input.data$NAC_DEC[input.data$TIME == PAC_TIME] <- NA
    }
    input.data
  }
