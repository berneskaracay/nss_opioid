setwd("C:\\Users\\karacb1\\Desktop\\nss_opioid")

# Define UI
shinyUI(
    dashboardPage(
        dashboardHeader(title = 'Opioid Overdoses'),
        dashboardSidebar(tags$blockquote('More than 72,000 Americans 
                         died from drug overdoses in 2017, 
                         including illicit drugs and 
                         prescription opioids — a 2-fold 
                         increase in a decade. 
                         Source: CDC WONDER')),
        dashboardBody(
                fluidRow(
                    box(
                        title = "State", status = "primary", solidHeader = TRUE,
                        "Select a State to see opioids prescribed", width=3,
                        selectInput("state", 
                                    label = "State:", 
                                    choices = states,
                                    selected = 'TN')
                    ),
                    
                fluidRow(
                    box(
                        title = "Opioids Prescribed in 2014", status = "primary", solidHeader = TRUE,
                        plotOutput("drugbars")
                    )
                    )
                )    
            )
        )
    )
