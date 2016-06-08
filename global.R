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
#Set the number of individuals that make up the 95% prediction intervals
  n <- 2000
#95% prediction interval functions
  CI95lo <- function(x) quantile(x,probs = 0.025)
  CI95hi <- function(x) quantile(x,probs = 0.975)
#Set seed for reproducible numbers
  set.seed(123456)
#One per ID function
  oneperID <- function(x) head(x,1)
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
    PPVCL <- sqrt(0.035022858) #PPV for CL
    PPVV <- sqrt(0.0054543827)	#PPV for V
    PPVKA <- sqrt(0.45608978)	#PPV for KA
    PPVF <- sqrt(0.52338442)	#PPV for F
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
#Calculate concentrations at each time-point for the individual
  #Using mrgsolve
    code <- '
    $PARAM  POPCL = 14.6076,
            POPV = 76.1352,
            POPKA = 0.66668,
            POPF = 1,
            ERR_CL = 0,
            ERR_V = 0,
            ERR_KA = 0,
            ERR_F = 0,
            WT_CL = 0.75,
            WT_V = 1,
            SDAC_F = -0.179735,
            PROD0_KA = 0,
            PROD1_KA = 1.41279,
            PROD2_KA = -0.488444,
            PROD3_KA = 0.0222383,
            PROD4_KA = -0.348731,
            WT = 70,
            SDAC = 0,
            PROD = 0

    $INIT   GUT = 0, CENT = 0, DUMP = 0

    $OMEGA  block = FALSE
            labels = s(ETA_CL,ETA_V,ETA_KA,ETA_F)
            0.035022858 0.0054543827 0.45608978 0.52338442

    $SIGMA  block = FALSE
            labels = s(ERR_PRO)
            0.101285

    $MAIN   double CL = POPCL*pow(WT/70,WT_CL)*exp(ETA_CL)*exp(ERR_CL);
            double V = POPV*pow(WT/70,WT_V)*exp(ETA_V)*exp(ERR_V);
            double KA = POPKA;
            if (PROD == 0) KA = POPKA*(1+PROD0_KA)*exp(ETA_KA)*exp(ERR_KA);
            if (PROD == 1) KA = POPKA*(1+PROD1_KA)*exp(ETA_KA)*exp(ERR_KA);
            if (PROD == 2) KA = POPKA*(1+PROD2_KA)*exp(ETA_KA)*exp(ERR_KA);
            if (PROD == 3) KA = POPKA*(1+PROD3_KA)*exp(ETA_KA)*exp(ERR_KA);
            if (PROD == 4) KA = POPKA*(1+PROD4_KA)*exp(ETA_KA)*exp(ERR_KA);
            double F = POPF*(1+SDAC_F*SDAC)*exp(ETA_F)*exp(ERR_F);
            F_GUT = F;

    $ODE    double CP = CENT/V;

            dxdt_GUT = -KA*GUT;
            dxdt_CENT = KA*GUT  -CL/V*CENT;
            dxdt_DUMP = CL/V*CENT;

    $TABLE  table(IPRE) = CENT/V;
            table(DV) = table(IPRE)*(1 + ERR_PRO);

    $CAPTURE CL V KA F
    '
    mod <- mcode("popAPAP",code)  #Compile the model code on application initiation
    #There is opportunity to simply update model parameters after the model code has been compiled

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
  TIME.rm <- seq(from = 0,to = max(TIME.base),by = 0.25)
#Rumack-Matthew Nomogram
  CONCrm <- 300*exp(-log(2)/4*TIME.rm)
  rule.data <- data.frame(TIME = TIME.rm,CONCrm)

#Function for flagging if an individual should receive NAC or not based on Rumack-Matthew Nomogram
#Function for BAYESIAN FORECASTED PAC
  rm.function <- function(input.data) {
    PAC_TIME <- input.data$TIME
    input.data$NAC_DEC <- 0
    input.data$NAC_DEC[input.data$IPRE[input.data$TIME == PAC_TIME] > rule.data$CONCrm[rule.data$TIME == PAC_TIME]] <- 1
    if (PAC_TIME < 4) {
      input.data$NAC_DEC[input.data$TIME == PAC_TIME] <- NA
    }
    input.data
  }
