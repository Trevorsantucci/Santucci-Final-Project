## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline


## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)


# Load data 
library(readr)
dataset <- read_csv("raw_data.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table(dataset$song_length)
mean(dataset$song_length)
sd(dataset$song_length)

table(dataset$number_of_listens)
mean(dataset$number_of_listens)
sd(dataset$number_of_listens)

table(dataset$song_descriptor)

table(dataset$song_topic)

table(dataset$personal_enjoyment)
mean(dataset$personal_enjoyment)
sd(dataset$personal_enjoyment)

table(dataset$album)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################

anova<- aov(crime_avg ~ land_locked, data = dataset)
summary(anova)

boxplot(crime_avg ~ land_locked, data = dataset)



##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(dataset$tour_inf, dataset$crime_avg)
print(linear_plot)
meany <- mean(dataset$tour_inf)
meanx <- mean(dataset$crime_avg)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(crime_avg ~ tour_inf, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$tour_inf, residuals(linear_plot))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$land_locked, dataset$pov_over10)

chisq.test(table(dataset$land_locked, dataset$pov_over10))