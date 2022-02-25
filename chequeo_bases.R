##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                       CHEQUEO DE ESTRUCTURA DE BASES - Año 2020          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Librerias
library(tidyverse)
library(eph)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    A través de la función get_microdata                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................una por una...........................
b_2020_t1 <- get_microdata(2020, 1)
b_2020_t2 <- get_microdata(2020, 2)
b_2020_t3 <- get_microdata(2020, 3)
b_2020_t4 <- get_microdata(2020, 4)

## Apilo en una sola
b_2020_tot_dplyr <- dplyr::bind_rows(b_2020_t1, b_2020_t2, b_2020_t3, b_2020_t4)

class(b_2020_t3$PP04B_COD)
class(b_2020_t4$PP04B_COD)


#................Pruebo la serie entera para 2020................
b_2020_tot_eph <- get_microdata(year = 2020, trimester = 1:4) %>% 
  select(microdata) %>% 
  unnest(microdata)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          a través de la base local                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

b_2021_t1 <- readRDS("eph/individual/base_individual_2021T1.RDS")
b_2021_t2 <- readRDS("eph/individual/base_individual_2021T2.RDS")
b_2021_t3 <- readRDS("eph/individual/base_individual_2021T3.RDS")
#b_2020_t4 <- readRDS("eph/individual/base_individual_2020T4.RDS")


b_2021_tot <- dplyr::bind_rows(
  b_2021_t1, 
  b_2021_t2, 
  b_2021_t3, 
  #b_2020_t4,
  )

class(b_2021_t3$IPCF)







##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                       CHEQUEO DE ESTRUCTURA DE BASES - Interanuales_t3   ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Librerias
library(tidyverse)
library(eph)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    A través de la función get_microdata                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................una por una...........................
b_2016_t2 <- get_microdata(2016, 2)
b_2017_t2 <- get_microdata(2017, 2)
b_2018_t2 <- get_microdata(2018, 2)
b_2019_t2 <- get_microdata(2019, 2)
b_2020_t2 <- get_microdata(2020, 2)
b_2021_t2 <- get_microdata(2021, 2)

## Apilo en una sola
b_interanual_dplyr <- dplyr::bind_rows(b_2016_t2, b_2017_t2, b_2018_t2, b_2019_t2, b_2020_t2, b_2021_t2)


#................Pruebo la serie entera para 2020................
b_interanual_tot_eph <- get_microdata(year = 2016:2021, trimester = 2) %>% 
  select(microdata) %>% 
  unnest(microdata)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          a través de la base local                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

b_2016_t2 <- readRDS("eph/individual/base_individual_2016T2.RDS")
b_2017_t2 <- readRDS("eph/individual/base_individual_2017T2.RDS")
b_2018_t2 <- readRDS("eph/individual/base_individual_2018T2.RDS")
b_2019_t2 <- readRDS("eph/individual/base_individual_2019T2.RDS")
b_2020_t2 <- readRDS("eph/individual/base_individual_2020T2.RDS")
b_2021_t2 <- readRDS("eph/individual/base_individual_2021T2.RDS")


b_interanual_tot <- dplyr::bind_rows(
  b_2016_t2,
  b_2017_t2,
  b_2018_t2,
  b_2019_t2,
  b_2020_t2,
  b_2021_t2
)


class(b_2020_t2$PP04B_COD)
class(b_2020_t2$PP04B_COD)


