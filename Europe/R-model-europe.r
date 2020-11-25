# Packages
library(readxl)
library(regclass)

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

# Model 1: with all 10 parameters
data1 = data.frame(deaths, med_age, avg_life, log(gdp), log(pop_dens), hum_dev,
                   age_70, diab, log(health_e), asthma, obes)
plot(data1, pch=20)

mod1 = lm(deaths~ med_age + avg_life + log(gdp) + log(pop_dens) + hum_dev
         + age_70 + diab + log(health_e) + asthma + obes
           , na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod1)

# vif
VIF(mod1)

# Model 2: Removing hum_dev, med_age and avg_life
mod2 = lm(deaths~ log(gdp) + log(pop_dens)
          + age_70 + diab + log(health_e) + asthma + obes
          ,data=data1, na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod2)

VIF(mod2)

# F-test, testing the model against the null model
anova(lm(deaths~1, data = data1), mod2)

