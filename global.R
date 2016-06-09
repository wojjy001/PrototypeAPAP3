#global.R script for PrototypeAPAP3
#Objects that are not reactive are written here
#This also a safe place for functions that are then used in server.R
#------------------------------------------------------------------------------------------
#Load package libraries
  library(shiny)
  library(shinydashboard)  #Package for making cooler user-interface for Shiny applications
  library(shinyjs)  #Package for perform common JavaScript operations
  library(ggplot2)  #Plotting
  library(grid)  #Plotting
  library(plyr)  #Split and rearrange data, ddply function
  library(dplyr)  #New plyr
  library(rmarkdown)  #Generate report to a Word, pdf or HTML document
  library(mrgsolve) #Metrum differential equation solver for pharmacometrics
#Directories on Windows
  # dir <- "//psf/Home/Desktop/PipPrototypeApp3/"	#Directory where application files are saved
  # pandocdir <- "C:/Program Files/RStudio/bin/pandoc"	#Directory for pancdoc (writing to word document)
#Directories on Mac
	dir <- "/Volumes/Prosecutor/PhD/APAP/PrototypeAPAP3/"  #Application's directory
  pandocdir <- "/Applications/RStudio.app/Contents/MacOS/pandoc"  #Directory for pancdoc (writing to word document)
#Define a custom ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 16))

#------------------------------------------------------------------------------------------
#Define time sequence
  TIME.base <- c(seq(from = 0,to = 3,by = 0.5),
                seq(from = 4,to = 12,by = 2),
                seq(from = 16,to = 32,by = 8))
  TIME.tgrid <- c(tgrid(0,3,0.5),tgrid(4,12,2),tgrid(16,32,8))
#Set the number of individuals that make up the 95% prediction intervals
  n <- 1000
#95% prediction interval functions
  CI95lo <- function(x) quantile(x,probs = 0.025)
  CI95hi <- function(x) quantile(x,probs = 0.975)
#Set seed for reproducible numbers
  set.seed(123456)
#One per ID function
  oneperID <- function(x) head(x,1)

#------------------------------------------------------------------------------------------
#Calculate concentrations at each time-point for the individual
  #Using mrgsolve - analytical solutions
  #This compiled model is used for simulating the individual's estimated profile and 95% prediction intervals
    code <- '
    $PARAM    POPCL = 14.6076,  //Population parameter values - fixed effects
              POPV = 76.1352,
              POPKA = 0.66668,
              POPF = 1,
              ERR_CL = 0, //Place holders for Bayesian estimated ETAs
              ERR_V = 0,
              ERR_KA = 0,
              ERR_F = 0,
              WT_CL = 0.75, //Covariate effects
              WT_V = 1,
              SDAC_F = -0.179735,
              PROD0_KA = 0,
              PROD1_KA = 1.41279,
              PROD2_KA = -0.488444,
              PROD3_KA = 0.0222383,
              PROD4_KA = -0.348731,
              WT = 70,  //Place holders for patient covariate values
              SDAC = 0,
              PROD = 0

    $INIT     GUT = 0, CENT = 0

    $PKMODEL  ncmt = 1,
              depot = TRUE,
              trans = 2

    $OMEGA    block = TRUE
              labels = s(ETA_CL,ETA_V,ETA_KA,ETA_F)
              0.035022858
              0.0044077284  0.0054543827
              0.10313184 -0.0034085583 0.45608978
              0.016587014 0.002349424 -0.15735995 0.52338442

    $SIGMA    block = FALSE
              labels = s(ERR_PRO)
              0.101285

    $MAIN     double CL = POPCL*pow(WT/70,WT_CL)*exp(ETA_CL+ERR_CL);
              double V = POPV*pow(WT/70,WT_V)*exp(ETA_V+ERR_V);
              double KA = POPKA;
              if (PROD == 0) KA = POPKA*(1+PROD0_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 1) KA = POPKA*(1+PROD1_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 2) KA = POPKA*(1+PROD2_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 3) KA = POPKA*(1+PROD3_KA)*exp(ETA_KA+ERR_KA);
              if (PROD == 4) KA = POPKA*(1+PROD4_KA)*exp(ETA_KA+ERR_KA);
              double F = POPF*(1+SDAC_F*SDAC)*exp(ETA_F+ERR_F);
              F_GUT = F;

    $TABLE    table(IPRE) = CENT/V;
              table(DV) = table(IPRE)*(1 + ERR_PRO);

    $CAPTURE  CL V KA F
    '
    mod <- mcode("popAPAP",code)  #Compile the model code on application initiation
    #There is opportunity to simply update model parameters after the model code has been compiled

#------------------------------------------------------------------------------------------
#Fit individual parameters given the observed concentrations, estimated doses and covariate values
  bayesian.function <- function(input.data) {
    #Initial parameter estimates
      initial.par <- c(exp(0),exp(0),exp(0),exp(0)) #Population values
      par <- initial.par
    #Observation - for the posterior
      Yobs <- input.data$PAC  #Most of this will be NA except for the samples
    #Update "mod" (model code for mrgsolve) parameters for Bayesian estimation
      covariate.list <- list(PROD = input.data$PROD[1],WT = input.data$WT[1],SDAC = input.data$SDAC[1])
      omega.list <- list(ETA_CL = 0,ETA_V = 0,ETA_KA = 0,ETA_F = 0)
      update.parameters <- mod %>% param(covariate.list) %>% omat(dmat(omega.list))
    #Input dataset for mrgsolve
  		input.conc.data <- expand.ev(ID = 1,amt = input.data$AMT[1])
    #Time sequence for mrgsolve
      time.bayes <- c(input.data$TIME)

    #Function for estimating individual parameters by minimising the Bayesian objective function value
      bayesian.ofv <- function(par) {
        ETA1fit <- log(par[1])  #Bayesian estimated ETA for clearance
        ETA2fit <- log(par[2]) #Bayesian estimated ETA for volume
        ETA3fit <- log(par[3])  #Bayesian estimated ETA for absorption rate constant
        ETA4fit <- log(par[4])  #Bayesian estimated ETA for bioavailability

        ETAfit.list <- list(ERR_CL = ETA1fit,ERR_V = ETA2fit,ERR_KA = ETA3fit,ERR_F = ETA4fit)  #List of ETA values that will be optimised - these will updated and connected to ERR_X terms in the mrgsolve model code
        conc.data <- update.parameters %>% param(ETAfit.list) %>% data_set(input.conc.data,evid = 1,cmt = 1) %>% mrgsim(start = 0,end = 0,add = time.bayes)  #Simulate concentration-time profile with nth iteration of ETA values
        conc.data <- as.data.frame(conc.data) #Convert to a data frame
        conc.data <- conc.data[-1,] #Remove the first row (don't need 2 x time = 0)

        Yhat <- conc.data$IPRE  #Make a Yhat vector based on IPRE in conc.data
        #If Yobsx was NA, then Yhat needs to be NA too (for calculating the log-likelihood)
        Yhat[is.na(Yobs) == T] <- NA
        #Posterior component (from the data)
        #Log densities of residuals
        #Residual error model, Y = IPRE*(1+ERR), Y = IPRE + IPRE*ERR
        ERRPRO <- sqrt(as.matrix(smat(mod)))  #Pull out SIGMA from "mod" - original model code
        loglikpost <- dnorm(na.omit(Yobs),mean = na.omit(Yhat),sd = na.omit(Yhat)*ERRPRO,log = T)
        #Prior component (from the model)
        ETA <- c(ETA1fit,ETA2fit,ETA3fit,ETA4fit) #List of Bayesian estimated ETAs
        ETABSV <- as.matrix(omat(mod)) #PPV for model parameters in "mod" - original model code
        ETABSV <- sqrt(c(ETABSV[1,1],ETABSV[2,2],ETABSV[3,3],ETABSV[4,4]))  #Pull out the OMEGAs and convert to SDs
        loglikprior <- dnorm(ETA,mean = 0,sd = ETABSV,log = T)
        #Calculate the combined likelihood
        OFVBayes <- -1*sum(loglikpost,loglikprior)
        OFVBayes
      }
    #Optimise the ETA parameters to minimise the OFVBayes
      resultfit <- optim(par,bayesian.ofv,hessian = TRUE,method = "L-BFGS-B",lower = c(0.001,0.001,0.001,0.001),upper = c(Inf,Inf,Inf,Inf),control = list(parscale = par,factr = 1e7))
    #Put results in a data frame
      #Split up the elements of the Hessian matrix to calculate standard errors for parameter estimates later on
      resultfit.data <- data.frame(ETA1 = log(resultfit$par[1]),
                                   ETA2 = log(resultfit$par[2]),
                                   ETA3 = log(resultfit$par[3]),
                                   ETA4 = log(resultfit$par[4]),
                                   HESS11 = resultfit$hessian[1,1],
                                   HESS12 = resultfit$hessian[1,2],
                                   HESS13 = resultfit$hessian[1,3],
                                   HESS14 = resultfit$hessian[1,4],
                                   HESS21 = resultfit$hessian[2,1],
                                   HESS22 = resultfit$hessian[2,2],
                                   HESS23 = resultfit$hessian[2,3],
                                   HESS24 = resultfit$hessian[2,4],
                                   HESS31 = resultfit$hessian[3,1],
                                   HESS32 = resultfit$hessian[3,2],
                                   HESS33 = resultfit$hessian[3,3],
                                   HESS34 = resultfit$hessian[3,4],
                                   HESS41 = resultfit$hessian[4,1],
                                   HESS42 = resultfit$hessian[4,2],
                                   HESS43 = resultfit$hessian[4,3],
                                   HESS44 = resultfit$hessian[4,4])
      resultfit.data
  }

#------------------------------------------------------------------------------------------
#Functions for applying various decision rules to Bayes estimated concentration profiles
  TIME <- 4:24  #Times that the Rumack-Matthew nomogram can only be applied to
  TIME.rm <- seq(from = 0,to = max(TIME.base),by = 0.25)
#Rumack-Matthew Nomogram
  CONCrm <- 300*exp(-log(2)/4*TIME.rm)
  rule.data <- data.frame(TIME = TIME.rm,CONCrm)

#Function for flagging if an individual should receive NAC or not based on Rumack-Matthew Nomogram
#Function for BAYESIAN FORECASTED PAC
  rm.function <- function(input.data) {
    PAC_TIME <- input.data$time[1]
    input.data$NAC_DEC <- 0
    input.data$NAC_DEC[input.data$IPRE[input.data$time == PAC_TIME] > rule.data$CONCrm[rule.data$TIME == PAC_TIME]] <- 1
    if (PAC_TIME < 4) {
      input.data$NAC_DEC[input.data$TIME == PAC_TIME] <- NA
    }
    input.data
  }
