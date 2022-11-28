# Internal code for solar project.
# Prepare data to import to Tableau. EDA work proceeded in another file. Ranges, incremental increase unit used
# in this file were explored by the EDA + outlier treatment work which are not included in this file.
library(tidyverse)
library(ggplot2)
library(scales)
library(usmap)
library(sf)

source <- read.csv('deepsolar_tract.csv')

###############################################################
########## FIRST STEP: SELECT INTERESTED VARIABLES ############
###############################################################
df <- source %>%
  select(
    # Solar system features/ Target features:
    solar_system_count, # number of solar power systems
    solar_system_count_residential, # count residential systems only, exclude industrial
    total_panel_area, # total area of solar panels (m^2)
    solar_panel_area_per_capita, # solar panel area per capita (m^2/capita)
    solar_panel_area_divided_by_area, # solar panel area divided by total area (m^2/mile^2)
    daily_solar_radiation, 
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
    number_of_years_of_education,
    employed,# no. of employed people
    #age_median, # median age

    household_count, # total number of households
    #housing_unit_count,# total number of housing units
    #housing_unit_occupied_count,# total number of occupied housing units,ACS 2015
    #housing_unit_median_value, # median housing unit value ($),ACS 2015
    #housing_unit_median_gross_rent,# median housing unit gross rent ($)
    
    #population, # total population,ACS 2015 (5-Year Estimates)
    population_density, # population density (/mile^2)
    #poverty_family_count, # number of families
    #unemployed, # number of unemployed people
    
    average_household_income, # average annual houshold income ($)
    median_household_income, # median annual houshold income
    per_capita_income,
    gini_index,
    
    # Percentage of Democrats vs Republicans
    #voting_2016_dem_percentage, #  DEM voting percentage in 2016 election,townhall.com
    #voting_2016_gop_percentage, # GOP voting percentage in 2016 election,townhall.com
    #incentive_count_residential, # number of incentives for residenial solar,www.dsireusa.org
    #incentive_count_nonresidential,# number of incentives for residenial solar,www.dsireusa.org
    
    # Transportation:
    #transportation_bicycle_rate, #ratio of using bicycle as transportation to work
    #transportation_public_rate, # ratio of using public transportation to work
    
    #Geographical features:
    #water_area, # total water area (mile^2),
    #elevation, #elevation
    #heating_design_temperature, # heating design temperature (celsius)
    #cooling_design_temperature, # cooling design temperature (celsius) 
    #earth_temperature_amplitude, # earth temperature amplitude (celsius)
    #frost_days, # number of frost days 
    #air_temperature, # air temperature (celsius)
    #relative_humidity,# relative humidity
    #daily_solar_radiation,# daily solar radiation (kWh/m^2/d) 
    #atmospheric_pressure,# atmospheric pressure
    #wind_speed, # wind speed
    #earth_temperature, # earth temperature (celsius) 
    #heating_degree_days, # heating degree days 
    #cooling_degree_days, #cooling degree days
  )

# Check percentage of missing values
colMeans(is.na(df))*100 # %

###############################################################
############# SECOND STEP: SUMMARISE by STATE #################
###############################################################

#state_level <- df %>%
#  group_by(state) %>%
#  summarise(
#    total_solar_count = sum(solar_system_count),
#    total_solar_panel_area = sum(total_panel_area),
#    solar_panel_area_per_capita = mean(solar_panel_area_per_capita, na.rm = TRUE),
#   solar_system_count_per_capita = sum(solar_system_count)/sum(population),
#    solar_system_count_per_household = sum(solar_system_count)/sum(household_count),
#    solar_system_count_per_housing_units = sum(solar_system_count)/sum(housing_unit_count)
#  )

# Write this dataframe to csv to export to tableau later
#write.csv(state_level, 'solar_selected_vars_by_state_level.csv')

# plot a simple US map

# TODO for Asthetics: convert numbers from 600,000 to 600k, add numbers to some largest states
# Get centroids
#plot_usmap(
#  data = state_level,
#  values = "total_solar_count",
# color = "navy") + 
#  labs(
#    title = "Number of Total Solar Systems by State",
#    subtitle = "California leads in solar installments.") +
#  scale_fill_continuous(
#    low = "white",
#    high = "navy",
#    name = "Number of Total Solar Systems",
#   label = scales::comma
#  ) +
#  theme(legend.position = "right")

###################################################################################
############# THIRD STEP: EXPLORE ASSOCIATION between DEMOGRAPHIC and SOLAR #######
#############  NOTE: CONSIDERING ONLY RESIDENTIAL SOLAR SYSTEMS ###################
###################################################################################

# We will aggregate by bin with size = 64, median metric. In case of even number of observations 
# -> chose the 1st one rather than average out 2 median, do this to keep fips, state and county.
priority_median <- function(x){
  if (length(x) %% 2 == 1) {
    return(median(x))
  }
  return(x[length(x)/2])
}

plot <- df %>%
  mutate(no_solar_per_1000household = (solar_system_count_residential*1000)/household_count) %>%
  select(daily_solar_radiation,
         fips,
         state,
         county,
         no_solar_per_1000household,
         population_density,
         average_household_income,
         number_of_years_of_education,
         gini_index)

plot <- plot %>% drop_na()
# consider high solar radiation only, which means > 5
plot <- plot[plot$daily_solar_radiation >= 5, ]

# remove outliers for each factor. These numbers are based on another work, in another code file
plot <- plot[plot$no_solar_per_1000household < 500, ]
plot <- plot[plot$gini_index > 0.14, ] # 2 obs out
plot <- plot[plot$population_density > 0.5, ]
plot <- plot[plot$average_household_income < 500000 & plot$average_household_income >15000, ]
plot <- plot[plot$number_of_years_of_education >= 0, ]

# gini index,group by 64 bins
gini_plot <- plot %>%
  select(no_solar_per_1000household, gini_index, fips, state, county) # Tableau can display state, county from fips
gini_bin = cut(gini_plot$gini_index,seq(0.28,0.6, 0.0048)) # from outlier treatment work
gini_solar_count <- aggregate(gini_plot %>% select(no_solar_per_1000household, gini_index),list(gini_bin),function(x) {return(median(x)) } )
gini_fips <- aggregate(gini_plot %>% select(fips, state, county),list(gini_bin),function(x) {return(priority_median(x)) } )
gini_plot <- merge(gini_solar_count, gini_fips)
ggplot(gini_plot, aes(x = gini_index, y = no_solar_per_1000household)) + 
  geom_point()
gini_plot <- gini_plot[gini_plot$no_solar_per_1000household < 70, ] #rm outliers

# education average,group by 64 bins
education_plot <- plot %>%
  select(no_solar_per_1000household, number_of_years_of_education, fips, state, county) # Tableau can display state, county from fips
education_bin = cut(education_plot$number_of_years_of_education,seq(8, 18, 0.125)) #64 bins
education_solar_count <- aggregate(education_plot %>% select(no_solar_per_1000household, number_of_years_of_education),list(education_bin),function(x) {return(median(x)) } )
education_fips <- aggregate(education_plot %>% select(fips, state, county),list(education_bin),function(x) {return(priority_median(x)) } )
education_plot <- merge(education_solar_count, education_fips)
ggplot(education_plot, aes(x = number_of_years_of_education, y = no_solar_per_1000household)) + 
  geom_point()
education_plot <- education_plot[education_plot$no_solar_per_1000household < 60, ] #rm outliers


# household income,group by 64 bins
income_plot <- plot %>%
  select(no_solar_per_1000household, average_household_income, fips, state, county) # Tableau can display state, county from fips
income_bin = cut(income_plot$average_household_income,seq(15000, 250000, 3320)) #64 bins
income_solar_count <- aggregate(income_plot %>% select(no_solar_per_1000household, average_household_income),list(income_bin),function(x) {return(median(x)) } )
income_fips <- aggregate(income_plot %>% select(fips, state, county),list(income_bin),function(x) {return(priority_median(x)) } )
income_plot <- merge(income_solar_count, income_fips)
ggplot(income_plot, aes(x = average_household_income, y = no_solar_per_1000household)) + 
  geom_point()
income_plot <- income_plot[income_plot$no_solar_per_1000household < 95, ] #rm outliers

# population density,group by 64 bins
pop_break = c(0,5,10,20,30,40,50,60,70,75,80,85,90,95,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,
              1000,1500,2000,2500,3000,3500,4000,4500, 5000, 5500,6000,6500,7000,7500,8000,8500,9000,9500,
              10000,15000,20000,25000,30000,35000, 40000,45000,50000, 55000,60000, 65000,70000, 75000,80000,85000)

pop_plot <- plot %>%
  select(no_solar_per_1000household, population_density, fips, state, county) # Tableau can display state, county from fips
pop_bin = cut(pop_plot$population_density,pop_break) #64 bins
pop_solar_count <- aggregate(pop_plot %>% select(no_solar_per_1000household, population_density),list(pop_bin),function(x) {return(median(x)) } )
pop_fips <- aggregate(pop_plot %>% select(fips, state, county),list(pop_bin),function(x) {return(priority_median(x)) } )
pop_plot <- merge(pop_solar_count, pop_fips)
ggplot(pop_plot, aes(x = population_density, y = no_solar_per_1000household)) + 
  geom_point() +
  scale_x_log10()

###############################################################
############# FOURTH STEP: WRITE TO CSV FOR TABLEAU ###########
###############################################################
#write.csv(df, 'solar_selected_vars.csv')
write.csv(gini_plot, 'gini_plot.csv')
write.csv(education_plot, 'education_plot.csv')
write.csv(income_plot, 'income_plot.csv')
write.csv(pop_plot, 'pop_plot.csv')