setwd("C:\\Users\\karacb1\\Desktop\\nss_opioid")

# Define UI
shinyUI(
    dashboardPage(
        dashboardHeader(title = 'Top prescribers!!!'),
        #dashboardSidebar(tags$blockquote('Get drug you want.')),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
                fluidRow(
                    box(
                        title = "State", status = "primary", solidHeader = TRUE,
                        "Select a State to see top prescribes", width=3,
                        selectInput("state", 
                                    label = "State:", 
                                    choices = states,
                                    selected = 'TN')
                    ),
                fluidRow(
                    box(
                        title = "Top prescriber", status = "primary", solidHeader = TRUE,height=20,
                        plotOutput("drugbars")
                    )
                    )
                )    
            )
        )
    )

