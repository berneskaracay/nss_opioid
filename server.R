
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$drugbars <- renderPlot({
    
    # filter drug_counts dataset by the state selected
    data_n <- mod2 %>% 
      filter(year == 2013)%>%
      top_n(n = 53,wt = Opioid.Claim.Count)
    
    top_pres<- c(data_n$NPI)
    
    data_model<-mod2 %>% 
      filter(NPI %in% top_pres)
    # create plot from filtered data
    data_model <- data_model %>% 
      filter(NPPES.Provider.State == input$state)
    
    ggplot(data_model, aes(x=year,
                           y=Opioid.Claim.Count)) + 
      facet_wrap(~ `id`) +
      geom_col() +
      xlab("Year") +
      ylab("Total Opioid claims") +
      theme(axis.text=element_text(size=18), axis.title=element_text(size=18),strip.text = element_text(size = 20))
    
    
  })
  
})


