# Internal code for solar project
# YEN's note:
# We summarise and do eda here, then we can plot graphs using either Tableau or Shiny
# Summary of Findings:
# (1): Update later
# (2): Update later
# (3): Update later

library(tidyverse)
library(ggplot2)
library(scales)
library(usmap)
library(sf)

source <- read.csv('deepsolar_tract.csv')

###############################################################
########## FIRST STEP: SELECT INTERESTED VARIABLES ############
###############################################################
# Note: dataset was aggregated from satellite images and ACS 2015 (5-Year Estimates)
# Collector: DeepSolar team from Stanford
# Some similar but different terms:
# + household vs housing unit: a household is a group of pp living in the same house -> human, housing unit is a physical house/apartment/studio...
# => household is about number of families while housing unit is about number of physical houses
# + 

df <- source %>%
  select(
    # Solar system features/ Target features:
    solar_system_count, # number of solar power systems
    total_panel_area, # total area of solar panels (m^2)
    solar_panel_area_per_capita, # solar panel area per capita (m^2/capita)
    solar_panel_area_divided_by_area, # solar panel area divided by total area (m^2/mile^2)
    
    # Energy features:
    electricity_price_overall, # average overall electricity price (cents/kWh)
    electricity_consume_total, # average monthly total electricity consumption (kWh)
    
    # Data level:
    fips, # census tract FIPS number, this is also the unique key of a row
    county, # county name
    state, # state name
    
    # Residential features:
    education_high_school_graduate, # no. of high school graduate level people  (as highest degree) after 25 years old
    education_less_than_high_school, # no. of less than high school level people after 25 years old
    employed,# no. of employed people
    age_median, # median age

    household_count, # total number of households
    housing_unit_count,# total number of housing units
    housing_unit_occupied_count,# total number of occupied housing units,ACS 2015
    housing_unit_median_value, # median housing unit value ($),ACS 2015
    housing_unit_median_gross_rent,# median housing unit gross rent ($)
    
    population, # total population,ACS 2015 (5-Year Estimates)
    population_density, # population density (/mile^2)
    poverty_family_count, # number of families
    unemployed, # number of unemployed people
    
    average_household_income, # average annual houshold income ($)
    median_household_income, # median annual houshold income
    per_capita_income,
    
    # Percentage of Democrats vs Republicans
    voting_2016_dem_percentage, #  DEM voting percentage in 2016 election,townhall.com
    voting_2016_gop_percentage, # GOP voting percentage in 2016 election,townhall.com
    incentive_count_residential, # number of incentives for residenial solar,www.dsireusa.org
    incentive_count_nonresidential,# number of incentives for residenial solar,www.dsireusa.org
    
    # Transportation:
    transportation_bicycle_rate, #ratio of using bicycle as transportation to work
    transportation_public_rate, # ratio of using public transportation to work
    
    #Geographical features:
    water_area, # total water area (mile^2),
    elevation, #elevation
    heating_design_temperature, # heating design temperature (celsius)
    cooling_design_temperature, # cooling design temperature (celsius) 
    earth_temperature_amplitude, # earth temperature amplitude (celsius)
    frost_days, # number of frost days 
    air_temperature, # air temperature (celsius)
    relative_humidity,# relative humidity
    daily_solar_radiation,# daily solar radiation (kWh/m^2/d) 
    atmospheric_pressure,# atmospheric pressure
    wind_speed, # wind speed
    earth_temperature, # earth temperature (celsius) 
    heating_degree_days, # heating degree days 
    cooling_degree_days, #cooling degree days
  )

# Check percentage of missing values
colMeans(is.na(df))*100 # %
# geographical features have NA ration ~ 8%. So using these with cautiousness.

# Write this dataframe to csv to export to tableau later
write.csv(df, 'solar_selected_vars.csv')

###############################################################
############# SECOND STEP: SUMMARISE by STATE #################
###############################################################

state_level <- df %>%
  group_by(state) %>%
  summarise(
    total_solar_count = sum(solar_system_count),
    total_solar_panel_area = sum(total_panel_area),
    solar_panel_area_per_capita = mean(solar_panel_area_per_capita, na.rm = TRUE),
    solar_system_count_per_capita = sum(solar_system_count)/sum(population),
    solar_system_count_per_household = sum(solar_system_count)/sum(household_count),
    solar_system_count_per_housing_units = sum(solar_system_count)/sum(housing_unit_count)
  )

# Write this dataframe to csv to export to tableau later
write.csv(state_level, 'solar_selected_vars_by_state_level.csv')

# plot a simple US map

# TODO for Asthetics: convert numbers from 600,000 to 600k, add numbers to some largest states
# Get centroids
plot_usmap(
  data = state_level,
  values = "total_solar_count",
  color = "navy") + 
  labs(
    title = "Number of Total Solar Systems by State",
    subtitle = "California leads in solar installments.") +
  scale_fill_continuous(
    low = "white",
    high = "navy",
    name = "Number of Total Solar Systems",
    label = scales::comma
  ) +
  theme(legend.position = "right")

###############################################################
############# THIRD STEP: EXPLORE GEOGRAPHICAL FEATURES #######
###############################################################


###############################################################
############# FOURTH STEP: EXPLORE SPECIFIC STATES ############
###############################################################