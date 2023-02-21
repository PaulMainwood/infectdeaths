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
library(stringr)
library(ISOweek)

conflict_prefer("filter", "dplyr")
working_directory <- "G://My Drive/R/infectdeaths"

deaths_raw <- read_csv("ONS death certificates.csv")
deaths <- deaths_raw %>% filter(sex == "all") %>% filter(Deaths == "Deaths involving COVID-19: occurrences") %>% select(Time, Week, 'age-groups', v4_1)
deaths$Week <- as.integer(str_sub(deaths$Week, 6))
deaths$Year <- as.integer(deaths$Time)
deaths <- deaths %>% 
  select(-c("Time")) %>% 
  select(c("Year", "Week", "age-groups", "v4_1")) %>% 
  pivot_wider(names_from = 'age-groups', values_from = v4_1) %>%
  arrange(Year, Week) %>%
  na.omit

deaths <- deaths %>% mutate(Date = ISOweek2date(paste0(deaths$Year, "-W", str_pad(deaths$Week, 2, pad = "0"), "-1")))


infections_raw <- read_csv("ONS England.csv")
infections_raw$Date <- dmy(infections_raw$Date)
infections_raw <- infections_raw %>% mutate(across(-c(1), function(x) as.numeric(str_replace(x, "%", ""))/100))
infections_week_num <- infections_raw %>% mutate(Year = isoyear(Date)) %>% mutate(Week = isoweek(Date)) %>% select(c(2, 5, 8, 11, 14, 17, 20, 23, 24)) %>% group_by(Year, Week) %>% summarise_all(mean) %>% ungroup 


populations <- read_csv("populations selected census21.csv")
population_matrix <- read_csv("population matrix deaths.csv")

a <- bind_cols(lapply(seq(1:dim(population_matrix)[1]), function(n) rowWeightedMeans(as.matrix(select(infections_week_num, -c(1, 2))), w = as.matrix(population_matrix[n, 2:dim(population_matrix)[2]]), rows = NULL)))
colnames(a) <- colnames(populations)[18:37]

infections <- a
infections <- rename(infections, '1-4' = "01-Apr")
infections <- rename(infections, '5-9' = "05-Sep")
infections <- rename(infections, '10-14' = "Oct-14")

#Multiply by the Census 2021 population for each age-group
pops <- populations[18:37]
infections <- cbind(select(infections_week_num, c(1,2)), sweep(infections, MARGIN = 2, as.matrix(pops), `*`))
infections <- infections %>% mutate(Date = ISOweek2date(paste0(infections$Year, "-W", str_pad(infections$Week, 2, pad = "0"), "-1")))

#TO HERE, GET DATES RIGHT FOR WEEKS/YEARS

lag_infs_to_deaths <- 14

first_week <- min(infections$Date)
last_week <- max(infections$Date) - lag_infs_to_deaths
ages <- colnames(infections[3:20])


model_with_lag <- function(infections_raw, deaths_raw, first_week, last_week, lag_infs_to_deaths, age_group){
  
  #Grab appropriate infections values
  infections <- infections_raw %>% filter(Date >= first_week) %>% filter(Date <= last_week)
  infec <- pull(infections, age_group)
  infec_dates <- infections$Date
  print(length(infec))

  #Select appropriate hospitalisation values
  deaths_raw <- deaths_raw %>% filter(Date >= first_week + lag_infs_to_deaths) %>% filter(Date <= last_week + lag_infs_to_deaths)
  hosp <- pull(deaths_raw, age_group)
  hosp_dates <- deaths_raw$Date
  print(length(hosp))


  ss <- list()
  ss <- AddDynamicRegression(ss, hosp ~ infec)

  model <- bsts(hosp, state.specification = ss, niter = 4000)
  dyn_coeffs <- apply(model$dynamic.regression.coefficients, 3, mean)


plot(hosp_dates, dyn_coeffs, type = "l")
return(list(model = model, dyn_coeffs = dyn_coeffs, infec_dates = infec_dates, hosp_dates = hosp_dates, var = var(dyn_coeffs)))
}

model_with_lag(infections, deaths, first_week, last_week, lag_infs_to_deaths, '60-64')
b <- lapply(ages, function(n) model_with_lag(infections, deaths, first_week, last_week, lag_infs_to_deaths, n))
c <- cbind(b[[1]]$hosp_dates, bind_cols(lapply(seq(1:length(b)), function(n) b[[n]]$dyn_coeffs)))
colnames(c) <- c("Week", ages)
data_long <- melt(c, id = "Week") %>% filter(variable != '0-1' & variable != '1-4' & variable != '5-9' & variable != '10-14')
lin_plot <- ggplot(data_long, aes(x = Week, y = value, colour = variable)) +  geom_line(size = 1) + scale_y_continuous(limits = c(0.000001, 0.11))
lin_plot
log_plot <- ggplot(data_long, aes(x = Week, y = value, colour = variable)) +  geom_line(size = 1) + scale_y_continuous(trans = "log2", limits = c(0.000001, 0.11))
log_plot
