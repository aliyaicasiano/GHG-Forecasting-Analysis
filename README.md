# GHG Time Series Forecasting
library(readxl)
library(ggplot2)

# Load the data
df <- read_excel(file.choose())

######## correlation analysis ##################################################
cor_elec <- cor.test(df$Year, df$Electricity_and_heat, method = "pearson")
cor_transp <- cor.test(df$Year, df$transportation, method = "pearson")
cor_manu <- cor.test(df$Year, df$manufacturing_and_construction, method = "pearson")
cor_agriculture <- cor.test(df$Year, df$agriculture, method = "pearson")
cor_fugitive <- cor.test(df$Year, df$fugitive_emissions, method = "pearson")
cor_buildings <- cor.test(df$Year, df$buildings, method = "pearson")
cor_industry <- cor.test(df$Year, df$industry, method = "pearson")
cor_land <- cor.test(df$Year, df$land_use_change_and_forestry, method = "pearson")
cor_waste <- cor.test(df$Year, df$waste, method = "pearson")

cor_elec # R = 0.99
cor_transp # 0.99
cor_manu # 0.94
cor_agriculture # 0.93
cor_fugitive # 0.89
cor_buildings # 0.95
cor_industry # 0.77
cor_land # 0.47
cor_waste # 0.41

# Every sector has anywhere between a moderate and a positive linear correlation
# with time
###################################################################

######## Time series plot ####################################################################################

ggplot(df, aes(x = Year)) +
  theme_gray() +
  geom_line(aes(y = Electricity_and_heat, color = "Electricity and Heat"), size = 0.8) +
  geom_line(aes(y = transportation, color = "Transportation"), size = 0.8) +
  geom_line(aes(y = manufacturing_and_construction, color = "Manufacturing and Construction"), size = 0.8) +
  geom_line(aes(y = agriculture, color = "Agriculture"), size = 0.8) +
  geom_line(aes(y = fugitive_emissions, color = "Fugitive Emissions"), size = 0.8) +
  geom_line(aes(y = buildings, color = "Buildings"), size = 0.8) +
  geom_line(aes(y = industry, color = "Industry"), size = 0.8) +
  geom_line(aes(y = as.numeric(land_use_change_and_forestry), color = "Land Use Change and Forestry"), size = 0.8) +
  geom_line(aes(y = waste, color = "Waste"), size = 0.8) +
  xlab("Year") +
  ylab("GHG Emissions (Billions of Tons)") +
  ggtitle("Time Series Plot of GHG Emissions") +
  scale_color_manual(values = c("Electricity and Heat" = "blue",
                                "Transportation" = "green",
                                "Manufacturing and Construction" = "red",
                                "Agriculture" = "lightblue",
                                "Fugitive Emissions" = "purple",
                                "Buildings" = "yellow",
                                "Industry" = "pink",
                                "Land Use Change and Forestry" = "chocolate",
                                "Waste" = "magenta"),
                     breaks = c("Electricity and Heat",
                                "Transportation",
                                "Manufacturing and Construction",
                                "Agriculture",
                                "Fugitive Emissions",
                                "Buildings",
                                "Industry",
                                "Land Use Change and Forestry",
                                "Waste")) +
  labs(color = "Sector")  # Setting the legend title

###############################################################################################

# Developing linear trend equations 

# electricity and heat sector
electricity_model <- lm(Electricity_and_heat ~ Period, data = df)
summary(electricity_model)

# transportation sector
transportation_model <- lm(transportation ~ Period, data = df)
summary(transportation_model)

# Manufacturing sector
manufacturing_model <- lm(manufacturing_and_construction ~ Period, data = df)
summary(manufacturing_model)

# Agriculture sector
agriculture_model <- lm(agriculture ~ Period, data = df)
summary(agriculture_model)

# Fugitive emissions sector
fugitive_emissions_model <- lm(fugitive_emissions[-1] ~ Period[-1], data = df) #taking out the first period because it's blank in the data
summary(fugitive_emissions_model)

# Buildings sector
buildings_model <- lm(buildings ~ Period, data = df)
summary(buildings_model)

# Industry sector
industry_model <- lm(industry ~ Period, data = df)
summary(industry_model)

# land_use change and forestry sector
land_use_forestry_model <- lm(`land_use_change_and_forestry` ~ Period, data = df)
summary(land_use_forestry_model)

# Waste sector
waste_model <- lm(waste ~ Period, data = df)
summary(waste_model)



########################## Calculate MAPE for each sector ###############################
calc_MAPE <- function(actual, predicted) {
  MAPE <- mean(abs((actual - predicted) / actual)) * 100
  return(MAPE)
}

# Putting each calculation into a variable
MAPE_electricity <- calc_MAPE(df$Electricity_and_heat, df$electricity_predicted)
MAPE_transportation <- calc_MAPE(df$transportation, df$transportation_predicted)
MAPE_manufacturing <- calc_MAPE(df$manufacturing_and_construction, df$manufacturing_predicted)
MAPE_agriculture <- calc_MAPE(df$agriculture, df$agriculture_predicted)
###### because first value is blank, calculating manually
MAPE_fugitive_emissions <- mean(abs((as.numeric(df$fugitive_emissions[-1]) - as.numeric(df$fugitive_emissions_predicted[-1])) / as.numeric(df$fugitive_emissions[-1]))) * 100
MAPE_buildings <- calc_MAPE(df$buildings, df$buildings_predicted)
MAPE_industry <- calc_MAPE(df$industry, df$industry_predicted)
MAPE_land_use_forestry <- calc_MAPE(df$`land_use_change_and_forestry`, df$land_use_forestry_predicted)
MAPE_waste <- calc_MAPE(df$waste, df$waste_predicted)

# Record each MAPE
MAPE_electricity # Result:2.80%
MAPE_transportation # Result: 1.73%
MAPE_manufacturing # Result: 3.27%
MAPE_agriculture # Result: 5.81%
MAPE_fugitive_emissions # Result: 2.27%
MAPE_buildings # Result: 3.22%
MAPE_industry # Result: 11.81%
MAPE_land_use_forestry # Result: 4.09%
MAPE_waste # Result: 10.70%
############################################################################


######## Forecast for each sector
electric_forecast_2025 <- predict(electricity_model, newdata = data.frame(Period = 36)) # period 1 = starting date of 1990 [adding 35 years to it]
transportation_forecast_2025 <- predict(transportation_model, newdata = data.frame(Period = 36))
manufacturing_forecast_2025 <- predict(manufacturing_model, newdata = data.frame(Period = 36))
agriculture_forecast_2025 <- predict(agriculture_model, newdata = data.frame(Period = 36))
fugitive_emissions_forecast_2025 <- predict(fugitive_emissions_model, newdata = data.frame(Period = 1:36)) # because first row is void
buildings_forecast_2025 <- predict(buildings_model, newdata = data.frame(Period = 36))
industry_forecast_2025 <- predict(industry_model, newdata = data.frame(Period = 36))
land_use_forestry_forecast_2025 <- predict(land_use_forestry_model, newdata = data.frame(Period = 36)) 
waste_forecast_2025 <- predict(waste_model, newdata = data.frame(Period = 36))

electric_forecast_2025 # 18.12 (billion tons of GHG gasses)
transportation_forecast_2025 # 9.18
manufacturing_forecast_2025 # 6.81
agriculture_forecast_2025 # 6.61
fugitive_emissions_forecast_2025 #3.39
buildings_forecast_2025 # 3.38
industry_forecast_2025 # 2.93
land_use_forestry_forecast_2025 # 1.65
waste_forecast_2025 # 1.44

#####################################################################################################
