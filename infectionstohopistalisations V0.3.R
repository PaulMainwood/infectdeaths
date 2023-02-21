library(lubridate)
library(conflicted)
library(tidyverse)
library(bsts)
library(clipr)
library(dplyr)
library(readr)
library(matrixStats)
library(ggplot2)
library(reshape2)

ggplot(df, aes(time,value)) + geom_line(aes(colour = series))
conflict_prefer("filter", "dplyr")

hosp_raw <- read_csv("NHS hosps.csv")
hosp_raw$Date <- dmy(hosp_raw$Date)

infections_raw <- read_csv("ONS England.csv")
infections_raw$Date <- dmy(infections_raw$Date)
infections_raw <- infections_raw %>% mutate(across(-c(1), function(x) as.numeric(str_replace(x, "%", ""))/100))

populations <- read_csv("populations selected census21.csv")
population_matrix <- read_csv("population matrix.csv")

a <- bind_cols(lapply(seq(1:dim(population_matrix)[1]), function(n) rowWeightedMeans(as.matrix(select(infections_raw, c(2, 5, 8, 11, 14, 17, 20))), w = as.matrix(population_matrix[n, 2:dim(population_matrix)[2]]), rows = NULL)))


colnames(a) <- colnames(populations)[1:10]
infections <- a
#Convert (approximately) infection rates to match the hospitalisation buckets


#Multiply by the Census 2021 population for each age-group
pops <- populations[1:10]
infections <- cbind(select(infections_raw, c(1)), sweep(infections, MARGIN = 2, as.matrix(pops), `*`))


first_day <- min(hosp_raw$Date) + 10
last_day <- max(infections$Date) - 10

lag_infs_to_hosps <- 3
ages <- colnames(infections)[2:length(colnames(infections))]


b <- lapply(ages, function(n) model_with_lag(infections, hosp_raw, first_day, last_day, lag_infs_to_hosps, n))

model_with_lag <- function(infections_raw, hosp_raw, first_day, last_day, lag_infs_to_hosps, age_group){
  
  #Grab infections we are using
  infections <- infections_raw %>% filter(Date >= first_day) %>% filter(Date <= last_day)
  infec <- pull(infections, age_group)
  infec_dates <- infections$Date
  print(length(infec))

  #Select appropriate hospitalisation values
  hosp_raw <- hosp_raw %>% filter(Date >= first_day + lag_infs_to_hosps) %>% filter(Date <= last_day + lag_infs_to_hosps)
  hosp <- pull(hosp_raw, age_group)
  hosp_dates <- hosp_raw$Date
  print(length(hosp))


  ss <- list()
  ss <- AddDynamicRegression(ss, hosp ~ infec)

  model <- bsts(hosp, state.specification = ss, niter = 1000)
  dyn_coeffs <- apply(model$dynamic.regression.coefficients, 3, mean)


plot(infec_dates, dyn_coeffs, type = "l")
return(list(model = model, dyn_coeffs = dyn_coeffs, infec_dates = infec_dates, hosp_dates = hosp_dates, var = var(dyn_coeffs)))
}

