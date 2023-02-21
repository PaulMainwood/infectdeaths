library(lubridate)
library(conflicted)
library(tidyverse)
library(bsts)
library(clipr)
library(dplyr)
library(readr)
conflict_prefer("filter", "dplyr")

hosp_raw <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumAdmissionsByAge&format=csv")
infections_raw <- read_csv("ONS England.csv")
infections_raw$Date <- dmy(infections_raw$Date)

first_day <- min(infections_raw$Date)
last_day <- max(infections_raw$Date)-10

lag_infs_to_hosps <- 3

a <- sapply(seq(0:10), function(x) model_with_lag(infections_raw, hosp_raw, first_day, last_day, x))

b <- model_with_lag(infections_raw, hosp_raw, first_day, last_day, 3)

model_with_lag <- function(infections_raw, hosp_raw, first_day, last_day, lag_infs_to_hosps){
  
  #Grab infections we are using
  infections <- infections_raw %>% filter(Date >= first_day) %>% filter(Date <= last_day)
  infec <- infections$`70+ modelled` %>% str_replace("%", "") %>% as.numeric / 100
  infec <- infec[1:(length(infec)-1)]
  infec_dates <- infections$Date[1:(length(infections$Date)-1)]

  #Select appropriate hospitalisation values
  hosp_cum <- hosp_raw %>% filter(age == "65_to_84") %>% arrange(date) %>% filter(date >= first_day + lag_infs_to_hosps) %>% filter(date <= last_day + lag_infs_to_hosps)
  hosp <- diff(hosp_cum$value)
  hosp_dates <- hosp_cum$date[2:length(hosp_cum$date)]


  ss <- list()
  ss <- AddDynamicRegression(ss, hosp ~ infec)

  model <- bsts(hosp, state.specification = ss, niter = 3000)
  dyn_coeffs <- apply(model$dynamic.regression.coefficients[50:1000,,], 2, mean)


plot(hosp_dates, dyn_coeffs, type = "l")
return(list(model = model, dyn_coeffs = dyn_coeffs, infec_dates = infec_dates, hosp_dates = hosp_dates, var = var(dyn_coeffs)))
}
