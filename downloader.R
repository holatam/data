library(tidyverse)
library(purrr)
library(glue)
library(haven)
library(eph)

### 30/10/2019 Paso bases a tibble

## bases puntuales hogar

bases_hogar <- get_microdata(year = 1996:2003, wave = c(1,2),type = 'hogar')

bases_hogar <- bases_hogar %>% 
  filter(!(year==2003 & wave ==2))

bases_hogar <- bases_hogar %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}O{wave}.RDS'),
         microdata = map(microdata, as.tibble))
  
walk2(bases_hogar$microdata,bases_hogar$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})

## bases puntuales individual

bases <- get_microdata(year = 1996:2003, wave = c(1,2),type = 'individual')

bases <- bases %>% 
  filter(!(year==2003 & wave ==2)) %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}O{wave}.RDS'),
         microdata = map(microdata, as.tibble))

# arreglo una variable que tenia diferentes tipos
bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){x %>% mutate(P13AUS = as.character(P13AUS))}))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})
