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


## bases continuas 2003:2007 hogar

bases <- get_microdata(year = 2003:2007, trimester = 1:4,type = 'hogar')

bases <- bases %>% 
  filter(!(year==2003 & trimester%in%c(1,2)),
         !(year==2007 & trimester%in%c(3,4))) %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         test = file.exists(filename))

# test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


## bases continuas 2008:2016 hogar

bases <- get_microdata(year = 2008:2016, trimester = 1:4,type = 'hogar')

bases <- bases %>% 
  filter(!(year==2015 & trimester%in%3:4),
    !(year==2016 & trimester==1)) %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases%>% 
  mutate(microdata = map(microdata, 
                         function(x){x %>% mutate(DECIFR = as.character(DECIFR),
                                                  IDECIFR = as.character(IDECIFR),
                                                  GDECIFR = as.character(GDECIFR),
                                                  PDECIFR = as.character(PDECIFR),
                                                  ADECIFR = as.character(ADECIFR),
                                                  DECCFR  = as.character(DECCFR),
                                                  IDECCFR = as.character(IDECCFR),
                                                  RDECCFR = as.character(RDECCFR),
                                                  GDECCFR = as.character(GDECCFR),
                                                  PDECCFR = as.character(PDECCFR),
                                                  ADECCFR = as.character(ADECCFR),
                                                  RDECIFR = as.character(RDECIFR))
                           }))

test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})

## bases continuas 2003:2007 individual

bases <- get_microdata(year = 2003:2007, trimester = 1:4,type = 'individual')

bases <- bases %>% 
  filter(!(year==2003 & trimester%in%c(1,2)),
         !(year==2007 & trimester%in%c(3,4))) %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         test = file.exists(filename))

# test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


## bases continuas 2008:2016 individual

bases <- get_microdata(year = 2008:2016, trimester = 1:4,type = 'individual')

bases <- bases %>% 
  filter(!(year==2015 & trimester%in%3:4),
         !(year==2016 & trimester==1)) %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){
                           if (!'PP04B_COD'%in% names(x)) { x$PP04B_COD <- NA_character_}
                           if (!'PP11B_COD'%in% names(x)) {x$PP11B_COD <- NA_character_}
                        x %>% mutate(CH14 = as.character(CH14),
                                     PP04B_COD = as.character(PP04B_COD),
                                     PP04D_COD = as.character(PP04D_COD),
                                     PP11B_COD = as.character(PP11B_COD),
                                     PP11D_COD = as.character(PP11D_COD),
                                     DECOCUR   = as.character(DECOCUR),
                                     IDECOCUR  = as.character(IDECOCUR),
                                     RDECOCUR  = as.character(RDECOCUR),
                                     GDECOCUR  = as.character(GDECOCUR),
                                     PDECOCUR  = as.character(PDECOCUR),
                                     ADECOCUR  = as.character(ADECOCUR),
                                     DECINDR   = as.character(DECINDR),
                                     IDECINDR   = as.character(IDECINDR),
                                     RDECINDR   = as.character(RDECINDR),
                                     GDECINDR   = as.character(GDECINDR),
                                     PDECINDR   = as.character(PDECINDR),
                                     ADECINDR   = as.character(ADECINDR),
                                     DECIFR    = as.character(DECIFR),
                                     IDECIFR   = as.character(IDECIFR),
                                     RDECIFR   = as.character(RDECIFR),
                                     GDECIFR   = as.character(GDECIFR),
                                     PDECIFR   = as.character(PDECIFR),
                                     ADECIFR   = as.character(ADECIFR),
                                     DECCFR    = as.character(DECCFR),
                                     IDECCFR   = as.character(IDECCFR),
                                     RDECCFR   = as.character(RDECCFR),
                                     GDECCFR   = as.character(GDECCFR),
                                     PDECCFR   = as.character(PDECCFR),
                                     ADECCFR   = as.character(ADECCFR)
                                     
                                     )
                         }))

test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


### Bases post 2016

## Hogar
bases <- get_microdata(year = 2017:2018, trimester = 1:4,type = 'hogar')

bases <- bases %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases%>% 
  mutate(microdata = map(microdata, 
                         function(x){x %>% mutate(DECIFR = as.character(DECIFR),
                                                  IDECIFR = as.character(IDECIFR),
                                                  GDECIFR = as.character(GDECIFR),
                                                  PDECIFR = as.character(PDECIFR),
                                                  ADECIFR = as.character(ADECIFR),
                                                  DECCFR  = as.character(DECCFR),
                                                  IDECCFR = as.character(IDECCFR),
                                                  RDECCFR = as.character(RDECCFR),
                                                  GDECCFR = as.character(GDECCFR),
                                                  PDECCFR = as.character(PDECCFR),
                                                  ADECCFR = as.character(ADECCFR),
                                                  RDECIFR = as.character(RDECIFR))
                         }))

test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})

### individual


bases <- get_microdata(year = 2017:2018, trimester = 1:4,type = 'individual')

bases <- bases %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){
                           if (!'PP04B_COD'%in% names(x)) { x$PP04B_COD <- NA_character_}
                           if (!'PP11B_COD'%in% names(x)) {x$PP11B_COD <- NA_character_}
                           x %>% mutate(CH14 = as.character(CH14),
                                        PP04B_COD = as.character(PP04B_COD),
                                        PP04D_COD = as.character(PP04D_COD),
                                        PP11B_COD = as.character(PP11B_COD),
                                        PP11D_COD = as.character(PP11D_COD),
                                        DECOCUR   = as.character(DECOCUR),
                                        IDECOCUR  = as.character(IDECOCUR),
                                        RDECOCUR  = as.character(RDECOCUR),
                                        GDECOCUR  = as.character(GDECOCUR),
                                        PDECOCUR  = as.character(PDECOCUR),
                                        ADECOCUR  = as.character(ADECOCUR),
                                        DECINDR   = as.character(DECINDR),
                                        IDECINDR   = as.character(IDECINDR),
                                        RDECINDR   = as.character(RDECINDR),
                                        GDECINDR   = as.character(GDECINDR),
                                        PDECINDR   = as.character(PDECINDR),
                                        ADECINDR   = as.character(ADECINDR),
                                        DECIFR    = as.character(DECIFR),
                                        IDECIFR   = as.character(IDECIFR),
                                        RDECIFR   = as.character(RDECIFR),
                                        GDECIFR   = as.character(GDECIFR),
                                        PDECIFR   = as.character(PDECIFR),
                                        ADECIFR   = as.character(ADECIFR),
                                        DECCFR    = as.character(DECCFR),
                                        IDECCFR   = as.character(IDECCFR),
                                        RDECCFR   = as.character(RDECCFR),
                                        GDECCFR   = as.character(GDECCFR),
                                        PDECCFR   = as.character(PDECCFR),
                                        ADECCFR   = as.character(ADECCFR)
                                        
                           )
                         }))

test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


