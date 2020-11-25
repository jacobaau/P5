# Packages
library("readxl")
library("regclass")

# Importing datasets
library(readxl)
Covid_data <- read_excel("Covid-data-World.xlsx")
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
births = Covid_data$`births_per_woman (estimates 2020: total fertility)`
obes = Covid_data$`obesity, %`

# Model 1: with all 10 parameters
data1 = data.frame(deaths, med_age, avg_life, gdp, pop_dens, hum_dev,
                   age_70, diab, health_e, asthma, obes)
plot(avg_life,deaths, pch = 20)
plot(log(gdp),deaths, pch = 20)
plot(log(pop_dens),deaths, pch = 20)
plot(med_age,deaths, pch = 20)
plot(hum_dev,deaths, pch = 20)
plot(age_70,deaths, pch = 20)
plot(diab,deaths, pch = 20)
plot(log(health_e),deaths, pch = 20)
plot(asthma,deaths, pch = 20)
plot(obes,deaths, pch = 20)

mod1 = lm(deaths~ med_age + avg_life + log(gdp) + log(pop_dens) + hum_dev
         + age_70 + diab + log(health_e) + asthma + obes,
        na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod1)
# vif
VIF(mod1)

log(deaths)

mod2 = lm(deaths~ avg_life + log(gdp) + log(pop_dens)
          + age_70 + diab + log(health_e) + asthma + obes,
          na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
VIF(mod2)
summary(mod2)

mod3 = lm(deaths~ avg_life + diab + obes, na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod3)

# residuals
res = residuals(mod3)
plot(avg_life,res, pch = 20)
plot(diab,res, pch = 20)
plot(obes,res, pch = 20)
qqnorm(res)
qqline(res)



# Model 2: without diab, pop_dens, age_70, births, avg_life, asthma, med_age
data2 = data.frame(deaths, gdp, hum_dev,
                   health_e)

mod2 = lm(deaths~ gdp + hum_dev
          + health_e
          ,data=data2, na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod2)

# Model 3: with 1st, 2nd and 3rd order terms
mod3 = lm(deaths~ med_age + avg_life + gdp + pop_dens + hum_dev
          + age_70 + diab + health_e + asthma + births
          + I(med_age^2) + I(avg_life^2) + I(gdp^2) + I(pop_dens^2) + I(hum_dev^2)
          + I(age_70^2) + I(diab^2) + I(health_e^2) + I(asthma^2) + I(births^2)
          + I(med_age^3) + I(avg_life^3) + I(gdp^3) + I(pop_dens^3) + I(hum_dev^3)
          + I(age_70^3) + I(diab^3) + I(health_e^3) + I(asthma^3) + I(births^3)
          ,data=data1, na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
b2 = coefficients(mod3); b2  # estimated intercept and slope, i.e. beta-hat
summary(mod3)

# Model 4: Model 3 when removing worst factors
mod4 = lm(deaths~ hum_dev
          + I(med_age^2) + I(hum_dev^2)
          + I(age_70^2)
          + I(avg_life^3) + I(hum_dev^3)
          + I(health_e^3)
          ,data=data1, na.action = na.omit)  # fitting model y = beta0 + beta1*x + ... + betak*x + epsilon
summary(mod4)


