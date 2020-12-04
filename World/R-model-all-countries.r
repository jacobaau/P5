# Packages
library("readxl")
library("regclass")

#=================================================================
#==================== Model 1 - All countries ====================
#=================================================================

# Importing datasets
Covid_data <- read_excel("C:/Users/ChristianSoendergaard/Dropbox/AAU - Matematik-økonomi/5. Semester/P5 - projekt/Dataset/Covid-data-all-countries.xlsx")
View(Covid_data)

# Defining vectors consisting of the data
deaths = Covid_data$total_deaths_per_million
avg_life = Covid_data$life_expectancy
gdp = Covid_data$gdp_per_capita
pop_dens = Covid_data$population_density
med_age = Covid_data$median_age
hum_dev = Covid_data$human_development_index*100
age_70 = Covid_data$aged_70_older
diab = Covid_data$diabetes_prevalence
health_e = Covid_data$`health_expenditure_pr_capita ($)`
asthma = Covid_data$`asthma_prevalence_age-standardized (%)`
obes = Covid_data$`obesity, %`

# Plotting the factors against log(deaths)
plot(avg_life,log(deaths), pch = 20)
plot(log(gdp),log(deaths), pch = 20)
plot(log(pop_dens),log(deaths), pch = 20)
plot(med_age,log(deaths), pch = 20)
plot(hum_dev,log(deaths), pch = 20)
plot(age_70,log(deaths), pch = 20)
plot(diab,log(deaths), pch = 20)
plot(log(health_e),log(deaths), pch = 20)
plot(asthma,log(deaths), pch = 20)
plot(obes,log(deaths), pch = 20)

# Defining data frame
data1 = data.frame(log(deaths), med_age, avg_life, log(gdp), log(pop_dens), hum_dev,
                   age_70, diab, log(health_e), asthma, obes)

# Correlation matrix with three decimals
round(cor(data1),3)

# Model 1.1: with all 10 factors
# Fitting model y = beta0 + beta1*x1 + ... + beta10*x10 + epsilon
mod1.1 = lm(log(deaths)~ med_age + avg_life + log(gdp) + log(pop_dens) + hum_dev
         + age_70 + diab + log(health_e) + asthma + obes, 
         na.action = na.omit)  
summary(mod1.1)

# VIF
VIF(mod1.1)

# Model 1.2:Removing hum_dev and med_age due to VIF>5
# Fitting model y = beta0 + beta1*x1 + ... + beta8*x8 + epsilon
# and computing VIF
mod1.2 = lm(log(deaths)~ avg_life + log(gdp) + log(pop_dens)
          + age_70 + diab + log(health_e) + asthma + obes,
          na.action = na.omit)  
summary(mod1.2); VIF(mod1.2)

# Model 1.3: Removing log(gdp), age_70, log(pop_dens) and log(health_e) due to F-tests
# Fitting model y = beta0 + beta1*x1 + ... + beta4*x4 + epsilon
mod1.3 = lm(log(deaths)~ avg_life + diab + asthma + obes,
          na.action = na.omit)  
summary(mod1.3)

# Computing residuals and plots
res = residuals(mod1.3)
plot(avg_life,res, pch = 20); abline(0,0)
plot(diab,res, pch = 20); abline(0,0)
plot(asthma,res, pch = 20); abline(0,0)
plot(obes,res, pch = 20); abline(0,0)

# QQ-plot
qqnorm(res); qqline(res)
