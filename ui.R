#ui.R script for PrototypeAPAP2
#The user-interface and widget input for the Shiny application is defined here
#Sends user-defined input to server.R, calls created output from server.R
#Now using shinydashboard for the user-interface
#------------------------------------------------------------------------------------------
#Application's header
header <-
  dashboardHeader(
		title = "To Antidote or Not?",
		titleWidth = 250
	)	#Brackets closing "dashboardHeader"
#Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 250,	#Width of sidebar the same as width of header
		sidebarMenu(
      menuItem("Introduction",tabName = "intro",icon = icon("question-circle")),
      menuItem("Paracetamol Overdose",tabName = "para-overdose",icon = icon("eyedropper")),
      menuItem("Rumack-Matthew Nomogram",tabName = "rm-nomo",icon = icon("calculator")),
      menuItem("Population PK Modelling",tabName = "pop-pk",icon = icon("bullseye")),
      menuItem("Application",tabName = "app",icon = icon("chrome"),
        menuSubItem("Patient Information",tabName = "patient",icon = icon("child")),
        menuSubItem("Overdose Information",tabName = "para-info",icon = icon("medkit")),
			  menuSubItem("Plot and Numerical Output",tabName = "results",icon = icon("line-chart"))
      ) #Brackets closing "menuItem"
		)	#Brackets closing "sidebarMenu"
	) #Brackets closing "dashboardSidebar"
#Application's body
body <-
	dashboardBody(
		tags$head(
			tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
		),
		tabItems(
      tabItem(tabName = "intro",
        h1(strong("Web-Based Antidote Recommendation Tool for Acute Paracetamol Overdose")),
        h2("Jessica Wojciechowski"),
        h3("University of South Australia supervisors: Richard Upton, David Foster, Michael Wiese"),
        h3("University of Maryland, Baltimore co-authors:")
      ),  #Brackets closing "tabItem" for "intro"
      tabItem(tabName = "para-overdose",
        h2(strong("Paracetamol Overdose")),
        h3("How much is a paracetamol overdose?"),
        h3("How many people overdose?"),
        h3("What are the consequences when somebody overdoses on paracetamol?"),
        h3("What is the antidotal treatment?")
      ),  #Brackets closing "tabItem" for "para-overdose"
      tabItem(tabName = "rm-nomo",
        h2(strong("Rumack-Matthew Nomogram")),
        fixedRow(
          column(4,
            h3("Decisions to administer N-acetylcysteine (NAC) are based upon a single plasma paracetamol concentration measured at least 4 hours since acute overdose against the Rumack-Matthew nomogram"),
            h3("Case reports of nomogram failure have been reported particularly in those patients who ingest paracetamol combination products (i.e., with an opioid or antihistamine)")
          ),  #Brackets closing column
          column(8,
            box(
              fixedRow(
                column(4,
                  selectInput("DEMO_TYPE","Example:",choices = list("Free input" = 1,"Opioid-combination product" = 2),selected = 1)
                ),  #Brackets closing "column"
                conditionalPanel(condition = "input.DEMO_TYPE == 1",
                  column(4,
                    numericInput("DEMO_TIME","Time since ingestion (hours):",min = 0,value = 4,width = 200)  #Numeric input for demonstration time
                    # Demonstration example: t = 6
                  ),  #Brackets closing "column"
                  column(4,
                    numericInput("DEMO_PAC","Concentration (mg/L):",min = 0,value = 150,width = 200)  #Numeric input for demonstration concentration
                    # Demonstration example: conc = 70
                  ) #Brackets closing "column"
                ),  #Brackets closing "conditionalPanel"
                conditionalPanel(condition = "input.DEMO_TYPE == 2",
                  br(),
                  h4("A single observation may fail to capture prolonged absorption")
                ) #Brackets closing "conditionalPanel"
              ), #Brackets closing "fixedRow"
              fixedRow(
                plotOutput("DEMOplotOutput",width = 600),  #Plot with Rumack-Matthew nomogram reactive to the widget input below (DEMO_TIME and DEMO_PAC)
                align = "center"
              ),  #Brackets closing "fixedRow"
              width = 12,
              status = "primary",
              title = "Rumack-Matthew Nomogram",
              footer = h4(strong(textOutput("DEMOtextOutput"))),
              solidHeader = TRUE,
              collapsible = TRUE
            ) #Brackets closing "box"
          ) #Brackets closing "column"
        ) #Brackets closing "fixedRow"
      ),  #Brackets closing "tabItem" for "rm-nomo"
      tabItem(tabName = "pop-pk",
        h2(strong("Role of Population Pharmacokinetic Modelling")),
        p("Demonstrate the layers of a population PK model")
      ), #Brackets closing "tabItem" for "pop-pk"
      tabItem(tabName = "app"
        #Leave this blank so nothing happens when this tab is clicked
      ),  #Brackets closing "tabItem" for "app"
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
			tabItem(tabName = "para-info",
				h4("Overdose Information:"),	#Heading for Overdose Information section
				dateInput("DDATE", "Date of Paracetamol Overdose (DD-MM-YYYY):",value = NULL,format = "dd-mm-yyyy",startview = "month"),
				numericInput("AMT","Estimated amount ingested (g):",min = 0,value = 25),	#Numeric input for estimated acetaminophen amount ingested
				selectInput("PROD","Product type ingested:",choices = list("Paracetamol alone" = 1,"Paracetamol and antihistamine" = 2,"Paracetamol and opioid" = 3,"Paracetamol and other" = 4,"Extended-release paracetamol" = 5),selected = 1),	#Select input for product category ingested
				fixedRow(
					column(8,
						h5(strong("Number of plasma paracetamol concentrations sampled:")),
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
							h4(strong("Individual Paracetamol Concentration-Time Profile")),
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
