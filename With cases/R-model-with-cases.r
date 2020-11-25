# Packages
library(readxl)
library(regclass)

# Importing datasets
Covid_data <- read_excel("Covid-data with cases.xlsx")
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

# Model 2: Removing hum_dev and med_age
mod2 = lm(deaths~ avg_life + log(gdp) + log(pop_dens)
          + age_70 + diab + log(health_e) + asthma + obes
          , na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod2)

# vif
VIF(mod2)

# F-test, testing the model against the null model
anova(lm(deaths~1, data = data1), mod2)

# Model 3: Removing asthma, pop_dens, aged_70, health_e, gdp, obes and diab
mod3 = lm(deaths~ avg_life
          , na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod3)

# residuals
res = residuals(mod3)
plot(res)
plot(avg_life,res, pch=20)
qqnorm(res)
qqline(res)

# Model 4: log(deaths)
plot(log(deaths),deaths)
mod4 = lm(log(deaths)~ avg_life + log(gdp) + log(pop_dens)
          + age_70 + diab + log(health_e) + asthma + obes
          , na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod4)

# Model 5: log(deaths) when removing age_70, pop_dens, asthma, diab, health_e and gdp
plot(log(deaths),deaths)
mod5 = lm(log(deaths)~ avg_life + obes
          , na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod5)

# residuals
res2 = residuals(mod5)
plot(res2,pch=20)
plot(avg_life,res2,pch=20)
plot(obes,res2,pch=20)
qqnorm(res2)
qqline(res2)

