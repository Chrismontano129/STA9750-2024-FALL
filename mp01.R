if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("DT")) install.packages("DT")

library(tidyverse)
library(readxl)
library(DT)
library(dplyr)
library(lubridate)

FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`) |>
  group_by(`NTD ID`,       
           `Agency Name`,  
           `Mode`) |>      
  summarize(`Total Fares` = sum(`Total Fares`)) |>
  ungroup()

EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) 
MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, 
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month))

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()

##T ask 1
USAGE <- USAGE %>%
  rename(
    metro_area = `UZA Name`,
    Unlinked_Passenger_Trips = UPT,
    Vehicle_Revenue_Miles = VRM
  )

## Task 2
unique_modes <- USAGE %>% distinct(Mode)
print(unique_modes)

USAGE <- USAGE |>
  mutate(Mode=case_when(
    Mode == "HR" ~ "Heavy Rail", 
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferry Boat",
    Mode == "MB" ~ "Bus",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "VP" ~ "Vanpool",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "RB" ~ "Rapid Bus",
    Mode == "LR" ~ "Light Rail",
    Mode == "YR" ~ "Hybrid Rail",
    Mode == "MG" ~ "Monorail/Automated Guideway",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "TR" ~ "Tram",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "PB" ~ "Public Bike",
    Mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"))

sample_n(USAGE, 1000) %>%
  select(-`NTD ID`, -`3 Mode`) %>%
  mutate(month = as.character(month)) %>%
  DT::datatable(options = list(pageLength = 25, lengthMenu = c(25, 50, 100), scrollX = TRUE))


##Task 3
### What transit agency had the most total VRM in our data set?
most_vrm_agency <- USAGE %>%
  group_by(Agency) %>%
  summarize(total_VRM = sum(Vehicle_Revenue_Miles, na.rm = TRUE)) %>%
  arrange(desc(total_VRM)) %>%
  slice(1)

print(most_vrm_agency)

###What transit mode had the most total VRM in our data set?
most_vrm_mode <- USAGE %>%
  group_by(Mode) %>%
  summarize(total_VRM = sum(Vehicle_Revenue_Miles, na.rm = TRUE)) %>%
  arrange(desc(total_VRM)) %>%
  slice(1)

print(most_vrm_mode)

###How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
nyc_subway_may_2024 <- USAGE %>%
  filter(Agency == "MTA New York City Transit", Mode == "Heavy Rail", month == as.Date("2024-05-01")) %>%
  summarize(total_trips = sum(Unlinked_Passenger_Trips, na.rm = TRUE))

print(nyc_subway_may_2024)

###How much did NYC subway ridership fall between April 2019 and April 2020?
nyc_subway_ridership_fall <- USAGE %>%
  filter(Agency == "MTA New York City Transit", Mode == "Heavy Rail", month %in% as.Date(c("2019-04-01", "2020-04-01"))) %>%
  group_by(month) %>%
  summarize(total_trips = sum(Unlinked_Passenger_Trips, na.rm = TRUE))

fall_in_ridership <- diff(nyc_subway_ridership_fall$total_trips)
print(fall_in_ridership)

##Task 4
###Which agency recorded the highest number of unlinked passenger trips (UPT) in 2022.
top_agency_2022 <- USAGE %>%
  filter(year(month) == 2022) %>%
  group_by(Agency) %>%
  summarize(total_trips = sum(Unlinked_Passenger_Trips, na.rm = TRUE)) %>%
  arrange(desc(total_trips)) %>%
  slice(1)

print(top_agency_2022)

###Identify the mode of transport that showed the highest percentage growth in unlinked passenger trips (UPT) from 2021 to 2022.
growth_mode <- USAGE %>%
  filter(year(month) %in% c(2021, 2022)) %>%
  group_by(Mode, year = year(month)) %>%
  summarize(total_trips = sum(Unlinked_Passenger_Trips, na.rm = TRUE)) %>%
  spread(year, total_trips) %>%
  mutate(growth = (`2022` - `2021`) / `2021` * 100) %>%
  arrange(desc(growth)) %>%
  slice(1)

print(growth_mode)

### which transit mode was the most common in terms of vehicle revenue miles (VRM) in 2022.
common_mode_2022 <- USAGE %>%
filter(year(month) == 2022) %>%
  group_by(Mode) %>%
  summarize(total_vrm = sum(Vehicle_Revenue_Miles, na.rm = TRUE)) %>%
  arrange(desc(total_vrm)) %>%
  slice(1)

print(common_mode_2022)

##Task 5

USAGE_2022_ANNUAL <- USAGE %>%
  mutate(year = year(as.Date(month))) %>% 
  filter(year == 2022) %>%  
  group_by(`NTD ID`, Agency, metro_area, Mode) %>%  
  summarize(
    UPT = sum(Unlinked_Passenger_Trips, na.rm = TRUE),  
    VRM = sum(Vehicle_Revenue_Miles, na.rm = TRUE)  
  ) %>%
  ungroup()

USAGE_2022_ANNUAL <- USAGE_2022_ANNUAL %>%
  mutate(
    Mode = case_when(
      Mode == "Bus" ~ "MB",
      Mode == "Heavy Rail" ~ "HR",
      Mode == "Commuter Rail" ~ "CR",
      Mode == "Ferry Boat" ~ "FB",
      Mode == "Light Rail" ~ "LR",
      Mode == "Trolleybus" ~ "TB",
      Mode == "Vanpool" ~ "VP",
      Mode == "Demand Response" ~ "DR",
      Mode == "Streetcar Rail" ~ "SR",
      Mode == "Hybrid Rail" ~ "YR",
      Mode == "Monorail/Automated Guideway" ~ "MG",
      Mode == "Alaska Railroad" ~ "AR",
      Mode == "Tram" ~ "TR",
      Mode == "Inclined Plane" ~ "IP",
      Mode == "Public Bike" ~ "PB",
      Mode == "Cable Car" ~ "CC",
      
      TRUE ~ "Unknown"
    )
  )

USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL, FINANCIALS, by = c("NTD ID", "Mode")) %>%
  drop_na()


print(USAGE_AND_FINANCIALS)

##Task 6 
### Which transit system (agency and mode) had the most UPT in 2022?
most_upt <- USAGE_AND_FINANCIALS %>%
  arrange(desc(UPT)) %>%
  slice(1) %>%
  select(Agency, Mode, UPT)

print(most_upt)

### Which transit system (agency and mode) had the highest farebox recovery, defined as the highest ratio of Total Fares to Expenses?
highest_farebox_recovery <- USAGE_AND_FINANCIALS %>%
  mutate(farebox_recovery = `Total Fares` / Expenses) %>%
  arrange(desc(farebox_recovery)) %>%
  slice(1) %>%
  select(Agency, Mode, farebox_recovery)

print(highest_farebox_recovery)

### Which transit system (agency and mode) has the lowest expenses per UPT?
lowest_expenses_per_upt <- USAGE_AND_FINANCIALS %>%
  mutate(expenses_per_upt = Expenses / UPT) %>%
  arrange(expenses_per_upt) %>%
  slice(1) %>%
  select(Agency, Mode, expenses_per_upt)

print(lowest_expenses_per_upt)

### Which transit system (agency and mode) has the highest total fares per UPT?
highest_fares_per_upt <- USAGE_AND_FINANCIALS %>%
  mutate(fares_per_upt = `Total Fares` / UPT) %>%
  arrange(desc(fares_per_upt)) %>%
  slice(1) %>%
  select(Agency, Mode, fares_per_upt)

print(highest_fares_per_upt)


### Which transit system (agency and mode) has the lowest expenses per VRM?
lowest_expenses_per_vrm <- USAGE_AND_FINANCIALS %>%
  mutate(expenses_per_vrm = Expenses / VRM) %>%
  arrange(expenses_per_vrm) %>%
  slice(1) %>%
  select(Agency, Mode, expenses_per_vrm)

print(lowest_expenses_per_vrm)

### Which transit system (agency and mode) has the highest total fares per VRM?
highest_fares_per_vrm <- USAGE_AND_FINANCIALS %>%
  mutate(fares_per_vrm = `Total Fares` / VRM) %>%
  arrange(desc(fares_per_vrm)) %>%
  slice(1) %>%
  select(Agency, Mode, fares_per_vrm)

print(highest_fares_per_vrm)

# Conclusion
Based on all of this, what do you believe to be the most efficient transit system in the country? (Your answer may differ depending on which form of ‘efficiency’ you care most about)
Farebox Recovery (income Efficiency): The transit system with the highest farebox recovery would be regarded as the most efficient if you were to rank the systems according to how much income they produce in comparison to their costs. This demonstrates how well fare revenue covers system costs.
Operational Efficiency (Cost per Vehicle Revenue Mile or UPT): Find the system with the lowest costs per Vehicle Revenue Mile (VRM) or Unlinked Passenger Trip (UPT) if operational efficiency is more significant. This shows which system is able to maintain low operating costs while maintaining service availability.
User Efficiency (Total Fares per UPT or VRM): The system with the highest total fares per UPT or per VRM might be observed if the goal is to maximize fare income per trip or mile.


