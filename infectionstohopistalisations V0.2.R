library(lubridate)
library(conflicted)
library(tidyverse)
library(bsts)
library(clipr)
library(dplyr)
library(readr)
conflict_prefer("filter", "dplyr")

hosp_raw <- read_csv("NHS hosps.csv")
hosp_raw$Date <- dmy(hosp_raw$Date)

infections_raw <- read_csv("ONS England.csv")
infections_raw$Date <- dmy(infections_raw$Date)
infections_raw <- infections_raw %>% mutate(across(-c(1), function(x) as.numeric(str_replace(x, "%", ""))))

populations <- read_csv("populations selected census21.csv")
population_matrix<- read_csv("population matrix.csv")

#Multiply by the Census 2021 population for each age-group
pops_for_infections <- rep(v, times = 1, each = 3)
infections <- cbind(select(infections_raw, c(1)), sweep(select(infections_raw, -c(1)), MARGIN = 2, pops_for_infections, `*`))

#Convert (approximately) to infection rates per hospitalisation bucket
sweep(select(infections_raw, c(2, 5, 8, 11, 14, 17, 20)), MARGIN = 2, t(population_matrix[1, 2:7]), '*')


first_day <- min(hosp_raw$Date) + 5
last_day <- max(infections$Date) - 5

lag_infs_to_hosps <- 3

b <- model_with_lag(infections, hosp_raw, first_day, last_day, lag_infs_to_hosps)

model_with_lag <- function(infections_raw, hosp_raw, first_day, last_day, lag_infs_to_hosps){
  
  #Grab infections we are using
  infections <- infections_raw %>% filter(Date >= first_day) %>% filter(Date <= last_day)
  infec <- infections$`50-69 modelled`
  infec_dates <- infections$Date[1:(length(infections$Date))]

  #Select appropriate hospitalisation values
  hosp_raw <- hosp_raw %>% filter(Date >= first_day + lag_infs_to_hosps) %>% filter(Date <= last_day + lag_infs_to_hosps)
  hosp <- hosp_raw$`Age 55-64`
  hosp_dates <- hosp_raw$Date


  ss <- list()
  ss <- AddDynamicRegression(ss, hosp ~ infec)

  model <- bsts(hosp, state.specification = ss, niter = 1000)
  dyn_coeffs <- apply(model$dynamic.regression.coefficients, 3, mean)


plot(infec_dates, dyn_coeffs, type = "l")
return(list(model = model, dyn_coeffs = dyn_coeffs, infec_dates = infec_dates, hosp_dates = hosp_dates, var = var(dyn_coeffs)))
}

