##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                       CHEQUEO DE ESTRUCTURA DE BASES                     ----
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

b_2020_t1 <- readRDS("eph/individual/base_individual_2020T1.RDS")
b_2020_t2 <- readRDS("eph/individual/base_individual_2020T2.RDS")
b_2020_t3 <- readRDS("eph/individual/base_individual_2020T3.RDS")
b_2020_t4 <- readRDS("eph/individual/base_individual_2020T4.RDS")


b_2020_tot <- dplyr::bind_rows(
  b_2020_t1, 
  b_2020_t2, 
  b_2020_t3, 
  b_2020_t4,
  )


class(b_2020_t3$PP04B_COD)
class(b_2020_t4$PP04B_COD)


