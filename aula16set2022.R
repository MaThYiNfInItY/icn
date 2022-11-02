library(radiant)
dados <- readr::read_rds("dados.rds") # %>%   fix_names()
## filter and sort the dataset
dadosf<-dados %>%
  filter(`Brasil`!=0) %>%
  select(codSubclasse:Brasil) #%>%
  #dtab(dec = 2, pageLength = -1, nr = 100) %>% 
  #render()  # exibir no Viewer
class(dadosf)
