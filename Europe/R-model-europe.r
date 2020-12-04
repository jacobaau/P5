# Packages
library(readxl)
library(regclass)

#==========================================================
#==================== Model 2 - Europe ====================
#==========================================================

# Importing datasets
Covid_data <- read_excel("Covid-data Europe.xlsx")
View(Covid_data)

# Defining vectors consisting of the data
deaths = Covid_data$total_deaths_per_million
avg_life = Covid_data$life_expectancy
gdp = Covid_data$gdp_per_capita
pop_dens = Covid_data$population_density
med_age = Covid_data$median_age
hum_dev = Covid_data$human_development_index
age_70 = Covid_data$aged_70_older
diab = Covid_data$diabetes_prevalence
health_e = Covid_data$`health_expenditure_pr_capita ($)`
asthma = Covid_data$`asthma_prevalence_age-standardized (%)`
obes = Covid_data$`obesity, %`

# Defining data frame
data2 = data.frame(log(deaths), med_age, avg_life, log(gdp), log(pop_dens), hum_dev,
                   age_70, diab, log(health_e), asthma, obes)

# Correlation matrix with three decimals
round(cor(data2),3)

# Model 2.1: with all 10 factors
# Fitting model y = beta0 + beta1*x1 + ... + beta10*x10 + epsilon
mod2.1 = lm(log(deaths)~ med_age + avg_life + log(gdp) + log(pop_dens) + hum_dev
         + age_70 + diab + log(health_e) + asthma + obes,
         na.action = na.omit) 
summary(mod2.1)

# VIF
VIF(mod2.1)

# Model 2.2: Removing hum_dev, med_age and avg_life due to VIF>5
# Fitting model y = beta0 + beta1*x1 + ... + beta7*x7 + epsilon
# and computing VIF
mod2.2 = lm(log(deaths)~ log(gdp) + log(pop_dens)
          + age_70 + diab + log(health_e) + asthma + obes,
          na.action = na.omit)
summary(mod2.2); VIF(mod2.2)

# Hence, the null model fits the data better
