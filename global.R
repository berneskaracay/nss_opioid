mod2 <- readRDS("data\\mymodel.rds")

states <- as.data.frame(mod2) %>% 
  select('NPPES.Provider.State') %>% 
  unique()

states <- sort(states$NPPES.Provider.State)