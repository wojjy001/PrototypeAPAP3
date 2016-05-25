#global.R script for PrototypeAPAP2
#Objects that are not reactive are written here
#This also a safe place for functions that are then used in server.R
#------------------------------------------------------------------------------------------
#Load package libraries
  library(shiny)
  library(shinydashboard)  #Package for making cooler user-interface for Shiny applications
  library(ggplot2)  #Plotting
  library(grid)  #Plotting
  library(plyr)  #Split and rearrange data, ddply function
  library(dplyr)  #New plyr
  library(rmarkdown)  #Generate report to a Word, pdf or HTML document
  #Directories on Windows
    # dir <- "//psf/Home/Desktop/PipPrototypeApp2/"	#Directory where application files are saved
    # pandocdir <- "C:/Program Files/RStudio/bin/pandoc"	#Directory for pancdoc (writing to word document)
  #Directories on Mac
  	dir <- "/Users/JessWoj/Desktop/PrototypeAPAP2/"  #Application's directory
    pandocdir <- "/Applications/RStudio.app/Contents/MacOS/pandoc"  #Directory for pancdoc (writing to word document)
#Define a custom ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 14))
#------------------------------------------------------------------------------------------
#Define time sequence
  TIME.base <- c(seq(from = 0,to = 3,by = 0.5),
                seq(from = 4,to = 12,by = 2),
                seq(from = 16,to = 32,by = 8))
#Set the number of individuals that make up the 95% prediction intervals
  n <- 1000
#95% prediction interval functions
  CI95lo <- function(x) quantile(x,probs = 0.025)
  CI95hi <- function(x) quantile(x,probs = 0.975)
#------------------------------------------------------------------------------------------
#Population model parameters
  #THETAs
    POPCL <- 14.6076 #Clearance, L/h
    POPV <- 76.1352	#Volume of distribution for central compartment, L
    POPKA <- 0.66668 #Absorption rate constant, h^-1
    POPF <- 1	#Bioavailability
    COVSDAC_F <- -0.179735 #Effect of activated charcoal administration on F
    COVPROD_KA0 <- 0 #Effect of product category on KA; paracetamol alone
    COVPROD_KA1 <- 1.41279 #Effect of product category on KA; para+antihistamine
    COVPROD_KA2 <- -0.488444  #Effect of product category on KA; para+opioid
    COVPROD_KA3 <- 0.0222383  #Effect of product category on KA; para+other
    COVPROD_KA4 <- -0.348731  #Effect of product category on KA; para ER
  #OMEGAs (as SDs)
    PPVCL <- 0.035022858 #PPV for CL
    PPVV <- 0.0054543827	#PPV for V
    PPVKA <- 0.45608978	#PPV for KA
    PPVF <- 0.52338442	#PPV for F
  #SIGMA (as SDs)
    ERRPRO <- 0.318253  #Proportional residual error
#------------------------------------------------------------------------------------------
#Calculate concentrations at each time-point for the individual
  #Function for calculating concentrations in a loop
    conc.function <- function(df){
      for(i in 2:nrow(df)) {
        #Specify individual parameter values
        df$CLi[i] <- POPCL*((df$WT[i]/70)^0.75)*exp(df$ETA1[i])
        df$Vi[i] <- POPV*(df$WT[i]/70)*exp(df$ETA2[i])  #Individual value for V
        if (df$PROD[i] == 0) df$KAi[i] <- POPKA*(1+COVPROD_KA0)*exp(df$ETA3[i]) #Individual value for KA
        if (df$PROD[i] == 1) df$KAi[i] <- POPKA*(1+COVPROD_KA1)*exp(df$ETA3[i]) #Individual value for KA
        if (df$PROD[i] == 2) df$KAi[i] <- POPKA*(1+COVPROD_KA2)*exp(df$ETA3[i]) #Individual value for KA
        if (df$PROD[i] == 3) df$KAi[i] <- POPKA*(1+COVPROD_KA3)*exp(df$ETA3[i]) #Individual value for KA
        if (df$PROD[i] == 4) df$KAi[i] <- POPKA*(1+COVPROD_KA4)*exp(df$ETA3[i]) #Individual value for KA
        df$Fi[i] <- POPF*(1+df$SDAC[i]*COVSDAC_F)*exp(df$ETA4[i]) #Individual value for F
        #Specify initial conditions
        df$A1[df$TIME == 0] <- df$AMT[df$TIME == 0]*df$Fi[i]  #Drug amount in the absorption compartment at time zero
        df$A2[df$TIME == 0] <- 0 #Drug amount in the central compartment at time zero
        df$IPRE[df$TIME == 0] <- 0  #Drug concentration in the central compartment at time zero

        KEi <- df$CLi/df$Vi #Elimination rate-constant from central compartment
        KEi[i] <- df$CLi[i]/df$Vi[i]  #Elimination rate-constant from central compartment
        time <- df$TIME[i]-df$TIME[i-1] #Difference in time between current and previous time-point
        A1.prev <- df$A1[i-1] #Amount in absorption compartment at previous time-point
        A2.prev <- df$A2[i-1] #Amount in central compartment at previous time-point
        df$A1[i] <- A1.prev*exp(-df$KAi[i]*time) + df$AMT[i]*df$Fi[i] #Amount in the absorption compartment at current time
        df$A2[i] <- A1.prev*df$KAi[i]/(df$KAi[i]-KEi[i])*(exp(-KEi[i]*time)-exp(-df$KAi[i]*time))+A2.prev*exp(-KEi[i]*time) #Amount in the central compartment at current time
        df$IPRE[i] <- df$A2[i]/df$Vi[i] #Concentration in central compartment at current time
      }
      df
    }
#------------------------------------------------------------------------------------------
#Fit individual parameters given the observed concentrations, estimated doses and covariate values
  bayesian.function <- function(input.data) {
    #Initial parameter estimates
      initial.par <- c(exp(0),exp(0),exp(0),exp(0)) #Population values
      par <- initial.par
    #Observation - posterior
      Yobs <- input.data$PAC  #Most of this will be NA except for the samples
    #Function for estimating individual parameters by minimising the Bayesian objective function value
      bayesian.ofv <- function(par) {
        ETA1fit <- log(par[1])  #Bayesian estimated ETA for clearance
        ETA2fit <- log(par[2]) #Bayesian estimated ETA for volume
        ETA3fit <- log(par[3])  #Bayesian estimated ETA for absorption rate constant
        ETA4fit <- log(par[4])  #Bayesian estimated ETA for bioavailability
        input.bayes.data <- input.data
        input.bayes.data$ETA1 <- ETA1fit  #Bayesian estimated ETA for clearance
        input.bayes.data$ETA2 <- ETA2fit #Bayesian estimated ETA for volume
        input.bayes.data$ETA3 <- ETA3fit  #Bayesian estimated ETA for absorption rate constant
        input.bayes.data$ETA4 <- ETA4fit  #Bayesian estimated ETA for bioavailability
        input.bayes.data$CLi <- POPCL  #Initial value for clearance
        input.bayes.data$Vi <- POPV  #Initial value for volume
        input.bayes.data$KAi <- POPKA  #Initial value for absorption rate constant
        input.bayes.data$Fi <- POPF  #Initial value for bioavailability
        conc.data <- conc.function(input.bayes.data)  #Run the concentration function
        Yhat <- conc.data$IPRE
        #If Yobsx was NA, then Yhat needs to be NA too (for calculating the log-likelihood)
        Yhat[is.na(Yobs) == T] <- NA
        #Posterior component (from the data)
        #Log densities of residuals
        #Residual error model, Y = IPRE*(1+ERR), Y = IPRE + IPRE*ERR
        loglikpost <- dnorm(na.omit(Yobs),mean = na.omit(Yhat),sd = na.omit(Yhat)*ERRPRO,log = T)
        #Prior component (from the model)
        ETA <- c(ETA1fit,ETA2fit,ETA3fit,ETA4fit)
        ETABSV <- c(PPVCL,PPVV,PPVKA,PPVF)
        loglikprior <- dnorm(ETA,mean = 0,sd = ETABSV,log = T)
        #Calculate the combined likelihood
        OFVBayes <- -1*sum(loglikpost,loglikprior)
        OFVBayes
      }
    #Optimise the ETA parameters to minimise the OFVBayes
      resultfit <- optim(par,bayesian.ofv,hessian = TRUE,method = "L-BFGS-B",lower = c(0.001,0.001,0.001,0.001),upper = c(Inf,Inf,Inf,Inf),control = list(parscale = par,factr = 1e7))
    #Put results in a data frame
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
#Rumack-Matthew Nomogram
  CONCrm <- 400*exp(-log(2)/4*TIME.base)
#Treatment Line
  CONCtl <- 300*exp(-log(2)/4*TIME.base)
#Decision Rule 2
  decision.rule2.times <- c(0,4.1,6.1,8.1,10.1,12.1,24)  #Times when the rule changes
  decision.rule2.concs <- c(150,125,90,65,45,20,20)  #Concentration cut-offs for the respective time-points
  decision.rule2.nomogram <- approxfun(decision.rule2.times,decision.rule2.concs,method = "constant")
  CONCdr2 <- decision.rule2.nomogram(TIME.base)
  rule.data <- data.frame(TIME = TIME.base,CONCrm,CONCtl,CONCdr2)

#Function for flagging if an individual should receive NAC or not based on Rumack-Matthew Nomogram
#Function for BAYESIAN FORECASTED PAC
  rm.function2 <- function(input.data) {
    PAC_TIME <- input.data$TIME
    input.data$NAC_DEC <- 0
    input.data$NAC_DEC[input.data$IPRE[input.data$TIME == PAC_TIME] > rule.data$CONCrm[rule.data$TIME == PAC_TIME]] <- 1
    if (PAC_TIME < 4) {
      input.data$NAC_DEC[input.data$TIME == PAC_TIME] <- NA
    }
    input.data
  }

#Function for flagging if an individual should receive NAC or not based on "Treatment Line"
#Function for BAYESIAN FORECASTED PAC
  tline.function2 <- function(input.data) {
    PAC_TIME <- input.data$TIME
    input.data$NAC_DEC <- 0
    input.data$NAC_DEC[input.data$IPRE[input.data$TIME == PAC_TIME] > rule.data$CONCtl[rule.data$TIME == PAC_TIME]] <- 1
    if (PAC_TIME < 4) {
      input.data$NAC_DEC[input.data$TIME == PAC_TIME] <- NA
    }
    input.data
  }

#Function for flagging if an individual should receive NAC or not based on DecisionRule2
#Function for BAYESIAN FORECASTED PAC
  decision.rule2.function2 <- function(input.data) {
    PAC_TIME <- input.data$TIME
    input.data$NAC_DEC <- 0
    input.data$NAC_DEC[input.data$IPRE[input.data$TIME == PAC_TIME] > rule.data$CONCdr2[rule.data$TIME == PAC_TIME]] <- 1
    input.data
  }
