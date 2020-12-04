# Packages
library(readxl)
library(regclass)

#========================================================================
#==================== Model 3 - Countries with cases ====================
#========================================================================

# Importing datasets
Covid_data <- read_excel("Google Drev/AAU/Projekter/P5-projekt/Covid-data-with-cases.xlsx")
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
data3 = data.frame(log(deaths), med_age, avg_life, log(gdp), log(pop_dens), hum_dev,
                   age_70, diab, log(health_e), asthma, obes)

# Correlation matrix with three decimals
round(cor(data3),3)

# Model 3.1: with all 10 factors
# Fitting model y = beta0 + beta1*x1 + ... + beta10*x10 + epsilon
mod3.1 = lm(log(deaths)~ med_age + avg_life + log(gdp) + log(pop_dens) + hum_dev
          + age_70 + diab + log(health_e) + asthma + obes,
          na.action = na.omit) 
summary(mod3.1)

# VIF
VIF(mod3.1)

# Model 3.2: Removing hum_dev and med_age due to VIF>5
# Fitting model y = beta0 + beta1*x + ... + beta8*x8 + epsilon 
# and computing VIF
mod3.2 = lm(log(deaths)~ avg_life + log(gdp) + log(pop_dens)
          + age_70 + diab + log(health_e) + asthma + obes,
          na.action = na.omit)  
summary(mod3.2);VIF(mod3.2)


# Model 3.3: Removing asthma, pop_dens, aged_70, health_e, gdp, obes and diab due to F-tests
# Fitting model y = beta0 + beta1*x + + beta2*x2 + epsilon
mod3.3 = lm(log(deaths)~ avg_life + obes
          , na.action = na.omit)  
summary(mod3.3)

# Computing residuals and plots
res = residuals(mod3.3)
plot(avg_life,res, pch=20); abline(0,0)
plot(obes,res, pch=20); abline(0,0)

# QQ-plot
qqnorm(res); qqline(res)
