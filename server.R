#server.R script for PrototypeAPAP3
#Reactive objects (i.e., those dependent on widget input) are written here
#------------------------------------------------------------------------------------------
#Define the "server" part of the Shiny application
shinyServer(function(input,output,session) {
	###########
	##_INPUT_##
	###########
	#Create a reactive input expression that takes widget values and makes a data frame ready for Bayesian estimation of individual pharmacokinetic parameters (Rinput.data)
	Rinput.data <- reactive({
		#Call in reactive widget input from ui.R
		WT <- input$WT  #Patient's weight (kg)
		AMT <- input$AMT*1000  #Estimated amount ingested (mg)
		PROD <- input$PROD  #Product category ingested
		if (input$SDAC == TRUE) SDAC <- 1	#If the patient received single-dose activated charcoal then SDAC = 1
		if (input$SDAC == FALSE) SDAC <- 0  #If the patient did not receive single-dose activated charcoal, then SDAC = 0
		PAC1 <- input$PAC1  #First plasma acetaminophen concentration (mg/L)
		TIME1 <- input$TIME1  #TIme for first plasma acetaminophen concentration (hours)
		PAC2 <- NA  #Make NA unless the appropriate "selectInput" option has been chosen
		TIME2 <- 0  #When PAC2 is NA, just make it correspond to TIME = 0
		if (input$NPAC > 1) {
			PAC2 <- input$PAC2  #Second plasma acetaminophen concentration (mg/L)
			TIME2 <- input$TIME2  #Time for second plasma acetaminophen concentration (hours)
		}
		#Slot sampling times into TIME sequence
		TIME <- sort(unique(c(TIME.base,TIME1,TIME2)))
		#Create a sequence of PAC values for corresponding times
		PAC <- rep(NA,times = length(TIME))
		PAC[TIME == TIME1] <- PAC1  #Input with PAC1 when TIME = TIME1
		PAC[TIME == TIME2] <- PAC2  #Input with PAC2 when TIME = TIME2
		#Collate into a data frame
		input.data <- data.frame(TIME,  #Time sequence
														AMT = c(AMT,rep(0,times = length(TIME)-1)),  #AMT input at time = 0, then no further doses at subsequent times
														PAC,  #Patient's plasma acetaminophen concentrations (mg/L)
														WT,  #Patient's weight (kg)
														SDAC,  #Single-dose activated charcoal status (0 = No, 1 = Yes)
														PROD,  #Product category ingested
														CLi = POPCL,  #Initial CL column
														Vi = POPV,  #Initial V column
														KAi = POPKA,  #Initial KA column
														Fi = POPF  #Initial F column
		)
		input.data
	})  #Brackets closing "Rinput.data"

	#Estimate individual parameter values based on the information in Rinput.data
	Rbayes.data <- reactive({
		withProgress(
			message = "Estimating individual parameters...",
			value = 0,
			{
			input.data <- Rinput.data()  #Read in the reactive "input.data"
			input.data <- input.data[input.data$TIME == 0 | is.na(input.data$PAC) == F,]	#Only use the time-points that are actually needed - i.e., when the amount was ingested and when samples were collected
			bayes.data <- bayesian.function(input.data)
			}  #Brackets closing expression for "withProgress"
		)  #Brackets closing "withProgress"
	})  #Brackets closing "Rbayes.data"

	#Use individual parameter estimates in Rbayes.data to simulate a concentration-time profile for the individual
	Rconc.data <- reactive({
		input.data <- Rinput.data()  #Read in reactive "input.data"
		bayes.data <- Rbayes.data()  #Read in reactive "bayes.data"
		input.conc.data <- merge(input.data,bayes.data,all = T)  #Merge the two data frames
		conc.data <- conc.function(input.conc.data)  #Simulate concentrations
	})  #Brackets closing "Rconc.data"

	#Use the hessian matrix from Rbayes.data to calculate standard errors for each parameter and simulate 95% prediction intervals for the individual
	Rci.data <- reactive({
		withProgress(
			message = "Simulating 95% prediction intervals...",
			value = 0,
			{
			  input.data <- Rinput.data() #Read in reactive "input.data"
				bayes.data <- Rbayes.data()	#Read in reactive "bayes.data"
				hessian.matrix <- matrix(c(bayes.data$HESS11,bayes.data$HESS12,bayes.data$HESS13,bayes.data$HESS14,bayes.data$HESS21,bayes.data$HESS22,bayes.data$HESS23,bayes.data$HESS24,bayes.data$HESS31,bayes.data$HESS32,bayes.data$HESS33,bayes.data$HESS34,bayes.data$HESS41,bayes.data$HESS42,bayes.data$HESS43,bayes.data$HESS44),4,4)
		  	VCmatrix <- solve(hessian.matrix)	#Calculate the variance-covariance matrix
				se.par <- sqrt(diag(VCmatrix))	#Calculate the parameter standard errors
				#Simulate an error distribution for each parameter
				ETA.list <- lapply(1:n, function(x) {
				  ETA1 <- log(rlnorm(1,meanlog = bayes.data$ETA1,sd = se.par[1]))
				  ETA2 <- log(rlnorm(1,meanlog = bayes.data$ETA2,sd = se.par[2]))
				  ETA3 <- log(rlnorm(1,meanlog = bayes.data$ETA3,sd = se.par[3]))
				  ETA4 <- log(rlnorm(1,meanlog = bayes.data$ETA4,sd = se.par[4]))
				  ETA.list <- list(ETA1,ETA2,ETA3,ETA4,x)
				})
				#Calculate concentrations at each time-point for each individual
				ci.data <- lapply(1:n, function(x) {
				  pop.data <- data_frame(TIME = TIME.ci,
				                        AMT = c(input$AMT*1000,rep(0,times=length(TIME.ci)-1)),
				                        SDAC = input.data$SDAC[1],
				                        WT = input$WT,
				                        ETA1 = ETA.list[[x]][[1]],
				                        ETA2 = ETA.list[[x]][[2]],
				                        ETA3 = ETA.list[[x]][[3]],
				                        ETA4 = ETA.list[[x]][[4]],
				                        PROD = input.data$PROD[1],
				                        CLi = POPCL,
				                        Vi = POPV,
				                        KAi = POPKA,
				                        Fi = POPF)
          conc.function(pop.data)
				}) %>% bind_rows
				ci.data <- as.data.frame(ci.data)
			} #Brackets closing expression for "withProgress"
		)  #Brackets closing "withProgress"
	})

	#Use the hessian matrix from Rbayes.data to calculate relative standard errors for each parameter
	#If relative standard errors are poor, then make the app show a message recommending to collect a further sample
	Rrse.par <- reactive({
		bayes.data <- Rbayes.data()  #Read in reactive "bayes.data"
		hessian.matrix <- matrix(c(bayes.data$HESS11,bayes.data$HESS12,bayes.data$HESS13,bayes.data$HESS14,bayes.data$HESS21,bayes.data$HESS22,bayes.data$HESS23,bayes.data$HESS24,bayes.data$HESS31,bayes.data$HESS32,bayes.data$HESS33,bayes.data$HESS34,bayes.data$HESS41,bayes.data$HESS42,bayes.data$HESS43,bayes.data$HESS44),4,4)
  	VCmatrix <- solve(hessian.matrix)	#Calculate the variance-covariance matrix
		se.par <- sqrt(diag(VCmatrix))	#Calculate the parameter standard errors
		CL.rse <- se.par[1]/exp(bayes.data$ETA1)*100	#Relative standard error for the ETA for CL
		V.rse <- se.par[2]/exp(bayes.data$ETA2)*100	#Relative standard error for the ETA for V
		KA.rse <- se.par[3]/exp(bayes.data$ETA3)*100 #Relative standard error for the ETA for KA
		F.rse <- se.par[4]/exp(bayes.data$ETA4)*100	#Relative standard error for the ETA for F
		rse.par <- c(CL.rse,V.rse,KA.rse,F.rse)  #Vector of parameter relative standard errors
	})	#Brackets closing "Rrse.par"

	#Use individual simulated concentration-time profile (Rconc.data) to decide whether the individual should receive NAC or not
	Rdecision.data <- reactive({
		conc.data <- Rconc.data()	#Read in reactive "conc.data"
		rm.decision.data <- ddply(conc.data, .(TIME), rm.function)  #Decide for each time-point in "conc.data" whether the individual should receive NAC or not according to the RM nomogram
			rm.decision <- sum(na.omit(rm.decision.data$NAC_DEC))
			if (rm.decision > 1) rm.decision <- 1
		#Combine results into a single data frame
		decision.data <- data.frame(row.names = c("Rumack-Matthew Nomogram"),"Decision" = rm.decision)
		decision.data$Decision[decision.data$Decision == 0] <- "No"
		decision.data$Decision[decision.data$Decision == 1] <- "Yes"
		decision.data
	})  #Brackets closing "Rdecision.data"

	############
	##_OUTPUT_##
	############
	observeEvent(input$IND_BAY, {
		if (input$IND_BAY == FALSE) reset("CI95")	#Observe the IND_BAY widget, if it is "false", i.e., unticked, then reset the CI95 widget value to its original state, which is also unticked
	})

	output$CONCplotOutput <- renderPlot({
		input.data <- Rinput.data()  #Read in reactive "input.data"
		conc.data <- Rconc.data()  #Read in reactive "conc.data"
		ci.data <- Rci.data()	#Read in reactive "ci.data"
		# decision.data <- Rdecision.data()	#Read in reactive "decision.data"

		#Calculate the maximum plottable value for shaded ribbons (Rumack-Matthew Nomogram)
		max.ribbon <- max(c(na.omit(input.data$PAC),conc.data$IPRE,rule.data$CONCrm[rule.data$TIME == 4]))+20
		#Calculate the maximum plottable value for y-axis
		max.conc <- max(c(na.omit(input.data$PAC),conc.data$IPRE))+20
		if (input$CI95 == TRUE) {
		  #Calculate the maximum plottable value for shaded ribbons (Rumack-Matthew Nomogram)
		  max.ribbon <- max(c(na.omit(input.data$PAC),conc.data$IPRE,CI95hi(ci.data$IPRE),rule.data$CONCrm[rule.data$TIME == 4]))+20
		  #Calculate the maximum plottable value for y-axis
		  max.conc <- max(c(na.omit(input.data$PAC),conc.data$IPRE,CI95hi(ci.data$IPRE)))+20
		}

		#Start plotting
		plotobj3 <- NULL
		plotobj3 <- ggplot()

		#Shaded regions representing the Rumack-Matthew Nomogram and when to treat with NAC
		if (input$RMN == TRUE) {
			plotobj3 <- plotobj3 + geom_ribbon(aes(x = TIME,ymin = 0.1,ymax = CONCrm),data = rule.data[rule.data$TIME %in% TIME,],alpha = 0.3,fill = "darkgreen")  #Range between min concentration and treatment line
		  plotobj3 <- plotobj3 + geom_ribbon(aes(x = TIME,ymin = CONCrm,ymax = max.ribbon),data = rule.data[rule.data$TIME %in% TIME,],alpha = 0.3,fill = "red")  #Range between Rumack-Matthew Nomogram and max concentration
		  plotobj3 <- plotobj3 + geom_line(aes(x = TIME,y = CONCrm),data = rule.data[rule.data$TIME %in% TIME,],linetype = "dashed",size = 1)  #Rumack-Matthew Nomogram
		}

		#95% prediction intervals
		if (input$IND_BAY == TRUE & input$CI95 == TRUE) {
			plotobj3 <- plotobj3 + stat_summary(aes(x = TIME,y = IPRE),data = ci.data,geom = "ribbon",fun.ymin = "CI95lo",fun.ymax = "CI95hi",alpha = 0.2,fill = "#3c8dbc",colour = "#3c8dbc",linetype = "dashed")
		}

	  #Individual patient data
		if (input$IND_BAY == TRUE) {
			plotobj3 <- plotobj3 + geom_line(aes(x = TIME,y = IPRE),data = conc.data,colour = "#3c8dbc",size = 1)  #Bayesian estimated
		}
		plotobj3 <- plotobj3 + geom_point(aes(x = TIME,y = PAC),data = input.data,size = 2)  #Observations

	  #Axes
		plotobj3 <- plotobj3 + scale_x_continuous("\nTime since ingestion (hours)")
		if (input$LOGS == FALSE & input$RMN == FALSE) {
			plotobj3 <- plotobj3 + scale_y_continuous("Plasma paracetamol concentration (mg/L)\n",lim = c(0,max.conc))
		}
		if (input$LOGS == FALSE & input$RMN == TRUE) {
			plotobj3 <- plotobj3 + scale_y_continuous("Plasma paracetamol concentration (mg/L)\n",lim = c(0,max.ribbon))
		}
		if (input$LOGS == TRUE & input$RMN == FALSE) {
			plotobj3 <- plotobj3 + scale_y_log10("Plasma paracetamol concentration (mg/L)\n",lim = c(0.1,max.conc))
		}
		if (input$LOGS == TRUE & input$RMN == TRUE) {
			plotobj3 <- plotobj3 + scale_y_log10("Plasma paracetamol concentration (mg/L)\n",lim = c(0.1,max.ribbon))
		}
		print(plotobj3)
	})	#Brackets closing "renderPlot"

	output$RSEtextOutput <- renderText({
		rse.par <- Rrse.par()	#Read in reactive "rse.par"
		warning.text <- " "
		if (rse.par[1] > 50 | rse.par[2] > 50 | rse.par[3] > 50 | rse.par[4] > 50) warning.text <- "Poor precision of at least one parameter estimate (relative standard error > 50%).  Recommend examining 95% prediction intervals before making a decision."
		warning.text
	})	#Brackets closing "renderText"

	output$NACtextOutput <- renderText({
		decision.data <- Rdecision.data()	#Read in reactive "decision.data"
		if (input$IND_BAY == FALSE) {
			if (input$NPAC == 1) {
				if (input$PAC1 >= rule.data$CONCrm[rule.data$TIME == input$TIME1]) {
					recommendation.text <- "Give N-acetylcysteine according to the Rumack-Matthew Nomogram"
				} else if (input$TIME1 < 4) {
					recommendation.text <- "Sampling is too early to use the Rumack-Matthew Nomogram"
				} else {
					recommendation.text <- "No requirement for N-acetylcysteine according to the Rumack-Matthew Nomogram"
				}
			}
			if (input$NPAC == 2) {
				if (input$PAC2 >= rule.data$CONCrm[rule.data$TIME == input$TIME2]) {
					recommendation.text <- "Give N-acetylcysteine according to the Rumack-Matthew Nomogram"
				} else if (input$TIME2 < 4) {
					recommendation.text <- "Sampling is too early to use the Rumack-Matthew Nomogram"
				} else if (input$PAC1 < rule.data$CONCrm[rule.data$TIME == input$TIME1] & input$PAC2 < rule.data$CONCrm[rule.data$TIME == input$TIME2]) {
					recommendation.text <- "No requirement for N-acetylcysteine according to the Rumack-Matthew Nomogram"
				}
			}
		}
		if (input$IND_BAY == TRUE) {
			if (decision.data$Decision[1] == "Yes") recommendation.text <- "Give N-acetylcysteine according to the Rumack-Matthew Nomogram"
			if (decision.data$Decision[1] == "No") recommendation.text <- "No requirement for N-acetylcysteine according to the Rumack-Matthew Nomogram"
		}
		recommendation.text
	})	#Brackets closing "renderText"

	############
	##_REPORT_##
	############
	#Generate a document of patient summary results
	output$downloadReport <- downloadHandler(
		filename = function() {
			paste(format(input$DDATE,"%Y-%m-%d"),input$LNAME,input$MRN,"Paracetamol_Report.pdf",sep = "_")
		},
		content = function(file) {
			src <- normalizePath("report.Rmd")
			inputEnv <- new.env()
			inputEnv$input.data <- Rinput.data()
			inputEnv$bayes.data <- Rbayes.data()
			inputEnv$conc.data <- Rconc.data()
			inputEnv$decision.data <- Rdecision.data()
			inputEnv$ci.data <- Rci.data()
			inputEnv$rse.par <- Rrse.par()
			#Temporarily switch to the temp dir, in case you don't have the permission to write to the current working directory
			owd <- setwd(tempdir())
			on.exit(setwd(owd))
			file.copy(src,"report.Rmd")
			#Sys.setenv(RSTUDIO_PANDOC = pandocdir)	#Required if running the application locally
			out <- render("report.Rmd",pdf_document(fig_width = 5,fig_height = 3),envir = inputEnv)
			#out <- render("report.Rmd",word_document(fig_width = 4,fig_height = 2,reference_docx = paste0(dir,"mystyles.docx")),envir = inputEnv)
			file.rename(out,file)
		}
	)	#Brackets closing "downloadHandler"

  #############
  ##_SESSION_##
  #############
  #Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })
})  #Brackets closing "shinyServer" function
