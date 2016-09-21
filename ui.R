# ui.R script for PrototypeAPAP3
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R
# Now using shinydashboard for the user-interface
# ------------------------------------------------------------------------------
# Application's header
header <-
  dashboardHeader(
		title = "To Antidote or Not?",
		titleWidth = 250
	)	# Brackets closing "dashboardHeader"
# Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 250,	# Width of sidebar the same as width of header
		sidebarMenu(
			menuItem("About",tabName = "about",icon = icon("question-circle"),
        menuSubItem("Background and Objective",tabName = "objective",icon = icon("angle-double-right")),
        menuSubItem("Population Model",tabName = "model",icon = icon("angle-double-right")),
				menuSubItem("Resources",tabName = "packages",icon = icon("angle-double-right")),
        menuSubItem("Acknowledgements",tabName = "acknowledgements",icon = icon("angle-double-right"))
      ),			
      menuItem("Patient Information",tabName = "patient",icon = icon("child")),
      menuItem("Overdose Information",tabName = "para-info",icon = icon("medkit")),
		  menuItem("Plot and Numerical Output",tabName = "results",icon = icon("line-chart"))
		)	# Brackets closing "sidebarMenu"
	) # Brackets closing "dashboardSidebar"
# Application's body
body <-
	dashboardBody(
    useShinyjs(),
		tags$head(
			tags$link(rel = "stylesheet",type = "text/css",href = "custom.css")
		),
    tabItems(
			tabItem(tabName = "patient",
				h4("Patient Information:"),	# Heading for Patient Information section
        fixedRow(
          column(6,
    				numericInput("MRN","Medical Record Number (MRN):",value = 000000,step = 1)  # Numeric input for patient's medical record number (or unit record number)
          ) # Brackets closing "column"
        ),  # Brackets closing "fixedRow"
				fixedRow(
					column(6,
						textInput("FNAME", "First Name:","First Name")	# Text input for patient's first name
          ),  # Brackets closing "column"
          column(6,
						textInput("LNAME","Last Name:","Last Name")	# Text input for patient's last name
          ) # Brackets closing "column"
        ),  # Brackets closing "fixedRow"
        fixedRow(
          column(6,
						dateInput("BDATE","Date of Birth (DD-MM-YYYY):",value = "1980-01-01",format = "dd-mm-yyyy",startview = "year")  # Date input for patient's date of birth
          ),  # Brackets closing "column"
          column(6,
						numericInput("WT","Weight (kg):",min = 0,max = 200,value = 70)	# Numeric input for patient weight
          ) # Brackets closing "fixedRow"
        ),  # Brackets closing "fixedRow"
        fixedRow(
          column(6,
						selectInput("SEX","Gender:",choices = list("Male" = 1,"Female" = 2),selected = 1)	# Select input for patient's gender
					)	# Brackets closing "column"
				)	# Brackets closing "fixedRow"
			),	# Brackets closing "tabItem" for "patient"
			tabItem(tabName = "para-info",
				h4("Overdose Information:"),	# Heading for Overdose Information section
				dateInput("DDATE", "Date of Acetaminophen Overdose (DD-MM-YYYY):",value = NULL,format = "dd-mm-yyyy",startview = "month"),
				numericInput("AMT","Estimated amount ingested (g):",min = 0,value = 25),	# Numeric input for estimated acetaminophen amount ingested
				selectInput("PROD","Product type ingested:",choices = list("Acetaminophen alone" = 1,"Acetaminophen and antihistamine" = 2,"Acetaminophen and opioid" = 3,"Acetaminophen and other" = 4,"Extended-release acetaminophen" = 5),selected = 1),	# Select input for product category ingested
				fixedRow(
					column(8,
						h5(strong("Number of plasma acetaminophen concentrations sampled:")),
						selectInput("NPAC","",choices = list("1" = 1,"2" = 2),selected = 1)	# Select input for number of plasma acetaminophen concentrations measured
					)	# Brackets closing "column"
				),	# Brackets closing "fixedRow"
				fixedRow(
					column(6,
						numericInput("TIME1","1: Time since ingestion (hours)",min = 0,value = 1)  # Numeric input for time of first plasma acetaminophen concentration
          ),  # Brackets closing "column"
          column(6,
						numericInput("PAC1","1: Concentration (mg/L)",min = 0,value = 100)	# Numeric input for first plasma acetaminophen concentration
          ) # Brackets closing "column"
        ),  # Brackets closing "fixedRow"
				conditionalPanel(condition = "input.NPAC > 1",
          fixedRow(
            column(6,
  							numericInput("TIME2","2: Time since ingestion (hours)",min = 0,value = 8)  # Numeric input for time of second plasma acetaminophen concentration
            ), # Brackets closing "column"
            column(6,
  							numericInput("PAC2","2: Concentration (mg/L)",min = 0,value = 70)	# Numeric input for second plasma acetaminophen concentration
            ) # Brackets closing "column"
          ) # Brackets closing "fixedRow"
				),  # Brackets closing "conditionalPanel"
				h4("Activated Charcoal Information:"),
				checkboxInput("SDAC","Was single-dose activated charcoal administered?",value = FALSE,width = 500),	# Checkbox input for single-dose activated charcoal administration
				conditionalPanel(condition = "input.SDAC",
					h5(strong("Time of administration (hours post-acetaminophen ingestion):")),
					numericInput("SDAC_TIME","",min = 0,max = 40,value = 4)
				)	# Brackets closing "conditionalPanel"
			),  # Brackets closing "tabItem" for "dosing"
			tabItem(tabName = "results",
				box(
          fixedRow(
            column(12,
    					h4(strong("Individual Acetaminophen Concentration-Time Profile")),
    					plotOutput("CONCplotOutput")#,
    					# br(),	# Add a space between plot and "warning text"
              # conditionalPanel(condition = "input.IND_BAY",
    					  # textOutput("RSEtextOutput")	# Sentence that appears if the precision of parameter estimates is poor
              # )  # Brackets closing "conditionalPanel"
            ),  # Brackets closing "column"
  					align = "center"
          ),  # Brackets closing "fixedRow"
					fixedRow(
						column(6,
							checkboxInput("LOGS","Plot concentrations on log-scale",value = FALSE),  # Checkbox input for plotting y-axis on a log-scale
							checkboxInput("RMN","Show Rumack-Matthew nomogram",value = FALSE) # Checkbox input for plotting Rumack-Matthew Nomogram
						),  # Brackets closing "column"
						column(6,
              checkboxInput("IND_BAY","Show Bayesian forecast",value = TRUE),  # Checkbox input for plotting empirical Bayesian prediction for the individual
              conditionalPanel(condition = "input.IND_BAY",
							  div(
                  checkboxInput("CI95","Show 95% confidence intervals",value = TRUE)	# Checkbox input for plotting empirical 95% confidence intervals
                ) # Brackets closing "div"
              ) # Brackets closing "conditionalPanel"
						),	# Brackets closing "column"
						align = "left"
					),	# Brackets closing "fixedRow"
          hr(),
					fixedRow(
            column(8,
  						strong(textOutput("NACtextOutput")),
              conditionalPanel(condition = "input.IND_BAY",
                p("(Recommendations are based on the Bayesian forecasted concentration profile and not 95% confidence intervals)")
              )  # Brackets closing "conditionalPanel"
            ),  # Brackets closing "column"
            column(4,
						  downloadButton("downloadReport", label = strong("Download Summary Report"))
            ),  # Brackets closing "column"
						align = "center"
					),	# Brackets closing "fixedRow"
					width = 12,
					status = "primary"
				)	# Brackets closing "box"
			), # Brackets closing "tabItem" for "results"
			tabItem(tabName = "objective",
				includeMarkdown("background.Rmd"),
				box(
					fixedRow(
						column(4, offset = 2,
							numericInput("DEMO_TIME","Time since ingestion (hours)",min = 0,value = 4)  #Numeric input for demonstration time
						),  #Brackets closing "column"
						column(4,
							numericInput("DEMO_PAC","Concentration (mg/L)",min = 0,value = 150)  #Numeric input for demonstration concentration
						), #Brackets closing "column"
						align = "center"
					), #Brackets closing "fixedRow"
					plotOutput("DEMOplotOutput"),  #Plot with Rumack-Matthew nomogram reactive to the widget input below (DEMO_TIME and DEMO_PAC)
					checkboxInput("DEMO_LOG","Plot concentrations on a log-scale",value = FALSE),
					h4(strong(textOutput("DEMOtextOutput"))),
					width = 12,
					title = "Rumack-Matthew Nomogram",
					solidHeader = TRUE,
					status = "primary"
				), #Brackets closing "box"
        includeMarkdown("objective.Rmd")
      ), # Brackets closing "tabItem" for "objective"
      tabItem(tabName = "model",
				includeMarkdown("model_description.Rmd"),
        pre(includeText("model.R"))
      ), # Brackets closing "tabItem" for "model"
			tabItem(tabName = "packages",
				pre(htmlOutput("session.info"))
			), # Brackets closing "tabItem" for "packages"
      tabItem(tabName = "acknowledgements",
        includeMarkdown("acknowledgements.Rmd"),
        fixedRow(
          column(3,
            img(src = "ACP_logo.png",width = 225,height = 75) # University of South Australia, Australian Centre for Pharmacometrics logo
          ),  # Brackets closing "column"
          column(3,offset = 1,
            img(src = "umb_ctm_logo.png",width = 240,height = 80)  # University of Maryland, Baltimore logo
          ) # Brackets closing "column"
        ) # Brackets closing "fixedRow"
      ) # Brackets closing "tabItem" for "acknowledgements"
		)  # Brackets closing "tabItems"
	) # Brackets closing "dashboardBody"
# ------------------------------------------------------------------------------
# User-interface Object
  dashboardPage(header,sidebar,body,skin = "blue")
