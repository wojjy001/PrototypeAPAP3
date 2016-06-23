#ui.R script for PrototypeAPAP3
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
      ), #Brackets closing "menuItem"
      menuItem("Code",tabName = "code",icon = icon("github"),
        menuSubItem("ui.R",tabName = "ui"),
        menuSubItem("server.R",tabName = "server"),
        menuSubItem("global.R",tabName = "global"),
        menuSubItem("report.Rmd",tabName = "report")
      ) #Brackets closing "menuItem"
		)	#Brackets closing "sidebarMenu"
	) #Brackets closing "dashboardSidebar"
#Application's body
body <-
	dashboardBody(
    useShinyjs(),
		tags$head(
			tags$link(rel = "stylesheet",type = "text/css",href = "custom.css")
		),
		tabItems(
      tabItem(tabName = "intro",
        h1(strong("Web-Based Antidote Recommendation Tool for Acute Paracetamol Overdose")),
        h2("Jessica Wojciechowski"),
        fixedRow(
          column(6,
            h3(strong("University of South Australia supervisors:")," Richard Upton, David Foster, Michael Wiese")
          ),  #Brackets closing "column"
          column(6,
            img(src = "unisa_logo.png",width = 225,height = 75)
          ) #Brackets closing "column"
        ),  #Brackets closing "fixedRow"
        fixedRow(
          column(6,
            h3(strong("University of Maryland, Baltimore co-authors:")," Julie Desrochers, Wendy Klein-Schwartz, Joga Gobburu, Mathangi Gopalakrishnan")
          ),  #Brackets closing "column"
          column(6,
            img(src = "umb_ctm_logo.png",width = 300,height = 100) #University of Maryland, Baltimore logo
          ) #Brackets closing "column"
        )  #Brackets closing "fixedRow"
      ),  #Brackets closing "tabItem" for "intro"
      tabItem(tabName = "para-overdose",
        h2(strong("Paracetamol Overdose")),
        hr(),
        fixedRow(
          box(
            h4("- Recommended dose: 1 g (paediatrics: 15 mg/kg) every 4 to 6 hours up to four times a day (maximum 4 g in 24 hours)"),
            h4("- Acute overdose: Single ingestion amount greater than 10 g (paediatrics: 200 mg/kg)"),
            title = strong("How much is a paracetamol overdose?"),
            status = "primary",
            solidHeader = TRUE,
            width = 12
          ),  #Brackets closing "box"
          box(
            h4("- Primary concern is hepatotoxicity"),
            title = strong("What are the consequences of paracetamol overdose?"),
            status = "primary",
            solidHeader = TRUE,
            width = 12
          ),  #Brackets closing "box"
          box(
            h4("- Single-dose activated charcoal (SDAC)"),
            h4("- N-acetylcysteine (NAC)"),
            title = strong("What is the antidotal treatment?"),
            status = "primary",
            solidHeader = TRUE,
            width = 12
          ) #Brackets closing "box"
        ) #Brackets closing "fixedRow"
      ),  #Brackets closing "tabItem" for "para-overdose"
      tabItem(tabName = "rm-nomo",
        fixedRow(
          column(4,
            h2(strong("Rumack-Matthew Nomogram")),
            hr(),
            h4("Decisions to administer N-acetylcysteine (NAC) are based upon a single plasma paracetamol concentration measured at least 4 hours since acute overdose against the Rumack-Matthew nomogram"),
            h4("Rumack-Matthew nomogram cannot be used prior to 4 hours since acute overdose")
          ),  #Brackets closing column
          column(8,
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
              plotOutput("DEMOplotOutput1"),  #Plot with Rumack-Matthew nomogram reactive to the widget input below (DEMO_TIME and DEMO_PAC)
              checkboxInput("DEMO_LOG","Plot concentrations on a log-scale",value = FALSE),
              h4(strong(textOutput("DEMOtextOutput1"))),
              width = 12,
              status = "primary"
            ) #Brackets closing "box"
          ) #Brackets closing "column"
        ) #Brackets closing "fixedRow"
      ),  #Brackets closing "tabItem" for "rm-nomo"
      tabItem(tabName = "pop-pk",
        fixedRow(
          column(4,
            h2(strong("Role of Population Pharmacokinetic Modelling")),
            hr(),
            box(
              fixedRow(
                column(12,
                  h4("- Population average concentration time-course"),
                  h4("- Parameterised as clearance (CL), volume of distribution (V), and absorption rate-constant (KA)"),
                  h4("- Quantify between-subject variability (%CV; coefficient of variation) in parameters"),
                  h4("- Quantify covariate effects on parameters")
                ) #Brackets closing "column"
              ), #Brackets closing "fixedRow"
              title = strong("Population Pharmacokinetic Modelling"),
              width = 12,
              status = "primary",
              solidHeader = TRUE
            ),  #Brackets closing "box"
            box(
              fixedRow(
                column(12,
                  h4("- Predict a specific individual's most likely concentration-time profile given sampled concentrations, covariate profile and prior population pharmacokinetic model"),
                  h4(strong("- Sample paracetamol concentrations before 4 hours, forecast the most likely profile and use forecasted concentrations against the Rumack-Matthew nomogram to make antidotal decisions"))
                ) #Brackets closing "column"
              ), #Brackets closing "fixedRow"
              title = strong("Bayesian forecasting"),
              width = 12,
              status = "primary",
              solidHeader = TRUE
            ) #Brackets closing "box"
          ),  #Brackets closing "column"
          column(8,
            box(
              selectInput("POPPK","Population or Individual?",choices = list("Population" = 1,"Individual" = 2),selected = 1,width = 250),
              conditionalPanel(condition = "input.POPPK == 1",
                fixedRow(
                  column(4,
                    checkboxInput("POP_MED","Plot population median line",value = FALSE)
                  ),  #Brackets closing "column"
                  column(4,
                    checkboxInput("POP_PARM","Show population parameter values",value = FALSE)
                  ), #Brackets closing "column"
                  column(4,
                    checkboxInput("POP_CI","Plot 95% prediction intervals",value = FALSE)
                  )  #Brackets closing "column"
                ) #Brackets closing "fixedRow"
              ),  #Brackets closing "conditionalPanel"
              conditionalPanel(condition = "input.POPPK == 2",
                fixedRow(
                  column(4,
                    checkboxInput("IND_LINES","Plot individual predictions",value = TRUE)
                  ), #Brackets closing "column"
                  column(4,
                    checkboxInput("IND_PARM","Show individual parameter values",value = TRUE)
                  ) #Brackets closing "column"
                ) #Brackets closing "fixedRow"
              ),  #Brackets closing "conditionalPanel"
              plotOutput("DEMOplotOutput2"),
              checkboxInput("DEMO_LOGS","Plot concentrations on a log-scale",value = TRUE),
              width = 12,
              status = "primary"
            ) #Brackets closing "box"
          ) #Brackets closing "column"
        ) #Brackets closing "fixedRow"
      ), #Brackets closing "tabItem" for "pop-pk"
      tabItem(tabName = "app"
        #Leave this blank so nothing happens when this tab is clicked
      ),  #Brackets closing "tabItem" for "app"
			tabItem(tabName = "patient",
				h4("Patient Information:"),	#Heading for Patient Information section
        fixedRow(
          column(6,
    				numericInput("MRN","Medical Record Number (MRN):",value = 000000,step = 1)  #Numeric input for patient's medical record number (or unit record number)
          ) #Brackets closing "column"
        ),  #Brackets closing "fixedRow"
				fixedRow(
					column(6,
						textInput("FNAME", "First Name:","First Name")	#Text input for patient's first name
          ),  #Brackets closing "column"
          column(6,
						textInput("LNAME","Last Name:","Last Name")	#Text input for patient's last name
          ) #Brackets closing "column"
        ),  #Brackets closing "fixedRow"
        fixedRow(
          column(6,
						dateInput("BDATE","Date of Birth (DD-MM-YYYY):",value = "1980-01-01",format = "dd-mm-yyyy",startview = "year")  #Date input for patient's date of birth
          ),  #Brackets closing "column"
          column(6,
						numericInput("WT","Weight (kg):",min = 0,max = 200,value = 70)	#Numeric input for patient weight
          ) #Brackets closing "fixedRow"
        ),  #Brackets closing "fixedRow"
        fixedRow(
          column(6,
						selectInput("SEX","Gender:",choices = list("Male" = 1,"Female" = 2),selected = 1)	#Select input for patient's gender
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
						selectInput("NPAC","",choices = list("1" = 1,"2" = 2),selected = 1)	#Select input for number of plasma acetaminophen concentrations measured
					)	#Brackets closing "column"
				),	#Brackets closing "fixedRow"
				fixedRow(
					column(6,
						numericInput("TIME1","1: Time since ingestion (hours)",min = 0,value = 1)  #Numeric input for time of first plasma acetaminophen concentration
          ),  #Brackets closing "column"
          column(6,
						numericInput("PAC1","1: Concentration (mg/L)",min = 0,value = 100)	#Numeric input for first plasma acetaminophen concentration
          ) #Brackets closing "column"
        ),  #Brackets closing "fixedRow"
				conditionalPanel(condition = "input.NPAC > 1",
          fixedRow(
            column(6,
  							numericInput("TIME2","2: Time since ingestion (hours)",min = 0,value = 8)  #Numeric input for time of second plasma acetaminophen concentration
            ), #Brackets closing "column"
            column(6,
  							numericInput("PAC2","2: Concentration (mg/L)",min = 0,value = 150)	#Numeric input for second plasma acetaminophen concentration
            ) #Brackets closing "column"
          ) #Brackets closing "fixedRow"
				),  #Brackets closing "conditionalPanel"
				h4("Activated Charcoal Information:"),
				checkboxInput("SDAC","Was single-dose activated charcoal administered?",value = FALSE,width = 500),	#Checkbox input for single-dose activated charcoal administration
				conditionalPanel(condition = "input.SDAC",
					h5(strong("Time of administration (hours post-paracetamol ingestion):")),
					numericInput("SDAC_TIME","",min = 0,max = 40,value = 4)
				)	#Brackets closing "conditionalPanel"
			),  #Brackets closing "tabItem" for "dosing"
			tabItem(tabName = "results",
				box(
          fixedRow(
            column(12,
    					h4(strong("Individual Paracetamol Concentration-Time Profile")),
    					plotOutput("CONCplotOutput"),
    					br(),	#Add a space between plot and "warning text"
              conditionalPanel(condition = "input.IND_BAY",
    					  textOutput("RSEtextOutput")	#Sentence that appears if the precision of parameter estimates is poor
              )  #Brackets closing "conditionalPanel"
            ),  #Brackets closing "column"
  					align = "center"
          ),  #Brackets closing "fixedRow"
					fixedRow(
						column(6,
							checkboxInput("LOGS","Plot concentrations on log-scale",value = FALSE),  #Checkbox input for plotting y-axis on a log-scale
							checkboxInput("RMN","Show Rumack-Matthew nomogram",value = FALSE) #Checkbox input for plotting Rumack-Matthew Nomogram
						),  #Brackets closing "column"
						column(6,
              checkboxInput("IND_BAY","Show Bayesian forecast",value = FALSE),  #Checkbox input for plotting empirical Bayesian prediction for the individual
              conditionalPanel(condition = "input.IND_BAY",
							  div(
                  checkboxInput("CI95","Show 95% prediction intervals",value = FALSE)	#Checkbox input for plotting empirical 95% confidence intervals
                ) #Brackets closing "div"
              ) #Brackets closing "conditionalPanel"
						),	#Brackets closing "column"
						align = "left"
					),	#Brackets closing "fixedRow"
          hr(),
					fixedRow(
            column(8,
              # h4(strong("N-acetylcysteine Decisions")),
  						strong(textOutput("NACtextOutput")),
              conditionalPanel(condition = "input.IND_BAY",
                p("(Recommendations are based on the Bayesian forecasted concentration profile and not 95% prediction intervals)")
              )  #Brackets closing "conditionalPanel"
            ),  #Brackets closing "column"
            column(4,
						  downloadButton("downloadReport", label = strong("Download Summary Report"))
            ),  #Brackets closing "column"
						align = "center"
					),	#Brackets closing "fixedRow"
					width = 12,
					status = "primary"
				)	#Brackets closing "box"
			), #Brackets closing "tabItem" for "results"
      tabItem(tabName = "ui",
        pre(includeText("ui.R"))
      ), #Brackets closing "tabItem" for "ui"
      tabItem(tabName = "server",
        pre(includeText("server.R"))
      ),  #Brackets closing "tabItem" for "server"
      tabItem(tabName = "global",
        pre(includeText("global.R"))
      ),  #Brackets closing "tabItem" for "global"
      tabItem(tabName = "report",
        pre(includeText("report.Rmd"))
      )  #Brackets closing "tabItem" for "report"
		)  #Brackets closing "tabItems"
	) #Brackets closing "dashboardBody"
#------------------------------------------------------------------------------------------
#User-interface Object
dashboardPage(header,sidebar,body,skin = "blue")
