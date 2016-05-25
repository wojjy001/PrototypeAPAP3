#ui.R script for PrototypeAPAP2
#The user-interface and widget input for the Shiny application is defined here
#Sends user-defined input to server.R, calls created output from server.R
#Now using shinydashboard for the user-interface
#------------------------------------------------------------------------------------------
#Application's header
header <-
  dashboardHeader(
		title = "Acetaminophen Overdose Application",
		titleWidth = 400
	)	#Brackets closing "dashboardHeader"
#Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 250,	#Width of sidebar the same as width of header
		sidebarMenu(
			menuItem("Patient Information",tabName = "patient",icon = icon("child")),
			menuItem("Acetaminophen Information",tabName = "acetaminophen",icon = icon("medkit")),
			menuItem("Plot and Numerical Output",tabName = "results",icon = icon("line-chart"))
		)	#Brackets closing "sidebarMenu"
	) #Brackets closing "dashboardSidebar"
#Application's body
body <-
	dashboardBody(
		tags$head(
			tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
		),
		tabItems(
			tabItem(tabName = "patient",
				h4("Patient Information:"),	#Heading for Patient Information section
				numericInput("MRN","Medical Record Number (MRN):",value = 000000,step = 1),  #Numeric input for patient's medical record number (or unit record number)
				fixedRow(
					column(4,
						textInput("FNAME", "First Name:","First Name"),	#Text input for patient's first name
						dateInput("BDATE","Date of Birth (DD-MM-YYYY):",value = "1980-01-01",format = "dd-mm-yyyy",startview = "year"),	#Date input for patient's date of birth
						selectInput("SEX","Gender:",choices = list("Male" = 1,"Female" = 2),selected = 1)	#Select input for patient's gender
					),	#Brackets closing "column"
					column(4,
						textInput("LNAME","Last Name:","Last Name"),	#Text input for patient's last name
						numericInput("WT","Weight (kg):",min = 0,max = 200,value = 70)	#Numeric input for patient weight
					)	#Brackets closing "column"
				)	#Brackets closing "fixedRow"
			),	#Brackets closing "tabItem" for "patient"
			tabItem(tabName = "acetaminophen",
				h4("Acetaminophen Information:"),	#Heading for Acetaminophen Information section
				dateInput("DDATE", "Date of Acetaminophen Overdose (DD-MM-YYYY):",value = NULL,format = "dd-mm-yyyy",startview = "month"),
				numericInput("AMT","Estimated amount ingested (g):",min = 0,value = 25),	#Numeric input for estimated acetaminophen amount ingested
				selectInput("PROD","Product type ingested:",choices = list("Acetaminophen alone" = 1,"Acetaminophen and antihistamine" = 2,"Acetaminophen and opioid" = 3,"Acetaminophen and other" = 4,"Extended-release acetaminophen" = 5),selected = 1),	#Select input for product category ingested
				fixedRow(
					column(8,
						h5(strong("Number of plasma acetaminophen concentrations sampled:")),
						selectInput("NPAC","",choices = list("1" = 1,"2" = 2),selected = 2)	#Select input for number of plasma acetaminophen concentrations measured
					)	#Brackets closing "column"
				),	#Brackets closing "fixedRow"
				fixedRow(
					column(4,
						numericInput("TIME1","1: Time since ingestion (hours)",min = 0,value = 1),  #Numeric input for time of first plasma acetaminophen concentration
						conditionalPanel(condition = "input.NPAC > 1",
							numericInput("TIME2","2: Time since ingestion (hours)",min = 0,value = 8)  #Numeric input for time of second plasma acetaminophen concentration
						)  #Brackets closing "conditionalPanel"
					),  #Brackets closing "column"
					column(4,
						numericInput("PAC1","1: Concentration (mg/L)",min = 0,value = 500),	#Numeric input for first plasma acetaminophen concentration
						conditionalPanel(condition = "input.NPAC > 1",
							numericInput("PAC2","2: Concentration (mg/L)",min = 0,value = 150)	#Numeric input for second plasma acetaminophen concentration
						)  #Brackets closing "conditionalPanel"
					)  #Brackets closing "column"
				),  #Brackets closing "fixedRow"
				h4("Activated Charcoal Information:"),
				checkboxInput("SDAC","Was single-dose activated charcoal administered?",value = FALSE,width = 500),	#Checkbox input for single-dose activated charcoal administration
				conditionalPanel(condition = "input.SDAC",
					h5(strong("Time of administration (hours post-acetaminophen ingestion):")),
					numericInput("SDAC_TIME","",min = 0,max = 40,value = 4)
				)	#Brackets closing "conditionalPanel"
			),  #Brackets closing "tabItem" for "dosing"
			tabItem(tabName = "results",
				box(
					fixedRow(
						column(7,
							h4(strong("Individual Acetaminophen Concentration-Time Profile")),
							plotOutput("CONCplotOutput"),
							br(),	#Add a space between plot and "warning text"
							textOutput("RSEtextOutput"),	#Sentence that appears if the precision of parameter estimates is poor
							align = "center",
							fixedRow(
								column(6,
									checkboxInput("LOGS","Plot concentrations on log-scale",value = FALSE),  #Checkbox input for plotting y-axis on a log-scale
									checkboxInput("CI95","Show 95% prediction intervals",value = FALSE)	#Checkbox input for plotting empirical 95% confidence intervals
								),  #Brackets closing "column"
								column(6,
									checkboxInput("RMN","Show Rumack-Matthew nomogram",value = FALSE) #Checkbox input for plotting Rumack-Matthew Nomogram
								),	#Brackets closing "column"
								align = "left"
							)	#Brackets closing "fixedRow"
						),	#Brackets closing "column"
						column(5,
							fixedRow(
								h4(strong("N-acetylcysteine Decisions")),
								textOutput("NACtextOutput"),
								hr(),
								downloadLink("downloadReport", label = h4(strong("Click here to download patient summary report"))),
								align = "center"
							)	#Brackets closing "fixedRow"
						)	#Brackets closing "column"
					),	#Brackets closing "fixedRow"
					width = 12,
					status = "primary"
				)	#Brackets closing "box"
			) #Brackets closing "tabItem" for "results"
		)  #Brackets closing "tabItems"
	) #Brackets closing "dashboardBody"
#------------------------------------------------------------------------------------------
#User-interface Object
dashboardPage(header,sidebar,body,skin = "blue")
