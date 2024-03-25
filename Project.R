# Clear the Environment
rm(list = ls())

# Load necessary libraries
library(rio)
library(haven)
library(dplyr)
library(ggplot2)
library(dataRetrieval)
library(tidyr)

# Set working directory
getwd()
setwd("D:\\Projects\\Participation Rate\\Data and Code")

path_report <- "D:\\Projects\\Participation Rate\\Report"
path_Tables <- "D:\\Projects\\Participation Rate\\Tables and Graphs"

# Create directories if they don't exist
if (!file.exists(path_report)) {
  dir.create(path_report, recursive = TRUE)
}

if (!file.exists(path_Tables)) {
  dir.create(path_Tables, recursive = TRUE)
}

# Read and compact LFS data for multiple years
compact_and_save <- function(year) {
  lfs <- read_stata(paste0("LFS ", year, ".dta"))
  lfs <- select(lfs, pkey, NobatAmargiri, F2_D19, F2_D07, F2_D17, F2_D04,
                IW_Yearly, ActivityStatus, F2_D03, F2_D01, F2_D15, F2_D16,
                F3_D16SHHAMSA)
  lfs <- mutate(lfs, year = year)
  saveRDS(lfs, paste0("lfs_", year, "_Compact.RDS"))
}

years <- 1396:1400
lapply(years, compact_and_save)

# Bind rows for all years
lfs_list <- lapply(paste0("lfs_", years, "_Compact.RDS"), readRDS)
lfs <- bind_rows(lfs_list)
lfs <- mutate(lfs ,province = substr(pkey , 3, 4))

# Rename columns
lfs <- rename(lfs,
              literacy = F2_D16,
              maritalstatus = F2_D19,
              age = F2_D07,
              gender = F2_D04,
              relate = F2_D03,
              radif = F2_D01,
              education = F2_D17,
              edustat = F2_D15,
              Working_hours = F3_D16SHHAMSA)

# Convert columns to numeric
cols_to_convert <- c("age", "ActivityStatus", "year", "gender", "maritalstatus",
                     "education", "relate", "edustat", "literacy", "Working_hours",
                     "IW_Yearly", "province")
lfs[cols_to_convert] <- lapply(lfs[cols_to_convert], as.numeric)

lfs$ActivityStatus <- ifelse(lfs$ActivityStatus == 3, 0, 
                             ifelse(lfs$ActivityStatus == 2, 1, lfs$ActivityStatus))

# Write CSV and save RDS
write.csv(lfs, "LFS.csv")
saveRDS(lfs, "lfs.RDS")

# Define functions to calculate employment, working population, and participation rate
employment <- function(ostan, sal, lfs_data) {
  lfs_filtered <- lfs_data %>%
    filter(province == ostan, gender == 2, age < 56, year == sal, ActivityStatus %in% c(1, 2))
  sum(lfs_filtered$IW_Yearly)
}

working_population <- function(ostan, sal, lfs_data) {
  lfs_filtered <- lfs_data %>%
    filter(gender == 2, province == ostan, age >= 15, age < 56, year == sal)
  sum(lfs_filtered$IW_Yearly)
}

participation_rate <- function(prov, yea, lfs_data) {
  emp <- employment(prov, yea, lfs_data)
  work_pop <- working_population(prov, yea, lfs_data)
  return(emp / work_pop * 100)
}

# Define provinces
provinces <- c(23, 25, 01, 09)
names(provinces) <- c("Tehran", "Qom", "Gilan", "Mashhad")

# Calculate participation rate for each province and year
part_rate <- sapply(provinces, function(ostan) {
  sapply(1396:1400, function(year) {
    participation_rate(ostan, year, lfs)
  })
})

# Create data frame for participation rate
part_rate <- t(part_rate)

# Create data frame for participation rate
data_of_partRate <- data.frame(
  year = 1396:1400,
  part_rate,
  row.names = NULL
)

# Perform a  proportion test to compare the participation rates of the cities
prop.test(data_of_partRate$Qom , data_of_partRate$Gilan )
prop.test(data_of_partRate$Qom , data_of_partRate$Tehran )
prop.test(data_of_partRate$Mashhad , data_of_partRate$Gilan )


# Save the plot as jpg
filename <- paste0(path_Tables, "/participation_rate_plot.jpg")
jpeg(filename, quality = 100, width = 800, height = 600)
plot(data_of_partRate$year, data_of_partRate$Tehran, type = "o", col = "blue",
     ylim = c(10, 27), xlab = "", ylab = "")
lines(data_of_partRate$year, data_of_partRate$Gilan, type = "o", col = "green")
lines(data_of_partRate$year, data_of_partRate$Mashhad, type = "o", col = "orange")
lines(data_of_partRate$year, data_of_partRate$Qom, type = "o", col = "red")
title(main = "Participation Rate by Year", xlab = "Year", ylab = "Participation Rate")
legend("topright", legend = c("Tehran", "Gilan", "Mashhad", "Qom"), 
       col = c("blue", "green", "orange", "red"), lty = 1, bty = "n", 
       title = "Provinces", cex = 0.8, inset = c(0, 0.05), xjust = 1, yjust = 1)
dev.off()

lfs$ActivityStatus[lfs$ActivityStatus == 3] <- 0
lfs$ActivityStatus[lfs$ActivityStatus == 2] <- 1

active <- filter(lfs , lfs$gender == 2  & lfs$year == 1400 &
                   lfs$ActivityStatus %in% 1)[c("IW_Yearly" , "age" , "education" )]

inactive <- filter(lfs , lfs$gender == 2  & lfs$year == 1400 &
                     lfs$ActivityStatus %in% 0)[c("IW_Yearly" , "age" , "education")]

# Histogram Diagram for Age and Education by Status
g_age <- ggplot(active, aes(x = age, weight = IW_Yearly)) + 
  geom_histogram(binwidth = 1) + 
  ggtitle("Age Distribution of Active Women") +
  xlab("Age") + ylab("Frequency")

g_education <- ggplot(inactive, aes(x = education, weight = IW_Yearly)) + 
  geom_histogram(binwidth = 1) + 
  ggtitle("Education Distribution of Inactive Women") +
  xlab("Education Level") + ylab("Frequency")

# Save the plots
file_age <- paste(path_Tables, "Age_Distribution_Active_Women.png", sep = "/")
file_education <- paste(path_Tables, "Education_Distribution_Inactive_Women.png", sep = "/")

# Save the plots with higher quality (dpi = 300)
ggsave(filename = file_age, plot = g_age, dpi = 300)
ggsave(filename = file_education, plot = g_education, dpi = 300)

# Calculating the working time
hours_lfs <- subset(lfs, gender == 2 & year == 1400)
hours_lfs$maritalstatus[hours_lfs$maritalstatus != 1] <- 0
hours_lfs <- mutate(hours_lfs, hours = Working_hours * IW_Yearly)
hours_lfs <- drop_na(hours_lfs)
saveRDS(hours_lfs, "hours_lfs.RDS")


# average working hours of single women
single_hours <- function(ostan){
  sum(filter(hours_lfs , hours_lfs$province == ostan &
    hours_lfs$maritalstatus == 0)["hours"])/sum(filter(hours_lfs ,
      hours_lfs$province == ostan & hours_lfs$maritalstatus == 0)["IW_Yearly"])
}

single_women <- c()
for(i in 0:30){
  single_women <- c(single_women , single_hours(i))
}
print(which.max(single_women)-1)
print(which.min(single_women)-1)

# Unemployment rate for single women
single_unemployee <- function(ostan){
  sum(filter(hours_lfs , hours_lfs$province == ostan &
    hours_lfs$ActivityStatus == 2)["IW_Yearly"])/sum(filter(hours_lfs ,
      hours_lfs$province == ostan & hours_lfs$ActivityStatus != 3)["IW_Yearly"])
}

unemployee_women <- c()
for (i in 0:30){
  unemployee_women <- c(unemployee_women , single_unemployee(i))
}
print(which.max(unemployee_women))
print(which.min(unemployee_women))

# average working hours for single women for Iran
print(sum(filter(hours_lfs ,hours_lfs$maritalstatus == 0)["hours"])/
        sum(filter(hours_lfs , hours_lfs$maritalstatus == 0)["IW_Yearly"]))

# Average working hours for single women in Iran
print(sum(filter(hours_lfs, maritalstatus == 0)$hours) / sum(filter(hours_lfs, maritalstatus == 0)$IW_Yearly))

# Filtering and calculating working hours for the year 1400
work_lfs <- subset(lfs, year == 1400)
work_lfs <- mutate(work_lfs, hours = Working_hours * IW_Yearly)
work_lfs[is.na(work_lfs)] <- 0
saveRDS(work_lfs, "work_lfs.RDS")

# Filtering women with children under 7 years old
lis <- work_lfs %>%
  group_by(pkey) %>%
  filter(if(any(relate == 3 & age < 7)) gender == 2 & relate < 3 else FALSE) %>%
  ungroup
saveRDS(lis, "lis.RDS")

# Function to calculate average working hours for married women with children under 6
withchild_women <- function(ostan) {
  sum(filter(lis, province == ostan & maritalstatus == 1)$hours) /
    sum(filter(lis, province == ostan & maritalstatus == 1)$IW_Yearly)
}

# Calculate and print max and min average working hours for married women with children under 6
married_women_withchild <- sapply(0:30, withchild_women)
print(which.max(married_women_withchild) - 1) 
print(which.min(married_women_withchild) - 1) 

# Function to calculate unemployment rate for married women with children under 6
married_unemployee <- function(ostan) {
  sum(filter(lis, province == ostan & ActivityStatus == 2 & maritalstatus == 1)$IW_Yearly) /
    sum(filter(lis, province == ostan & ActivityStatus != 3 & maritalstatus == 1)$IW_Yearly)
}

# Calculate and print max and min unemployment rate for married women with children under 6
unemployee_women <- sapply(0:30, married_unemployee)
print(which.max(unemployee_women)) 
print(which.min(unemployee_women)) 

# Calculate and print the average working hours for married women with children under 6 for Iran
print(sum(filter(lis, maritalstatus == 1)$hours) / sum(filter(lis, maritalstatus == 1)$IW_Yearly))

# Filtering women without children under 7 years old
lis <- work_lfs %>%
  group_by(pkey) %>%
  filter(if(any((relate == 3 & age > 6) | relate != 3)) gender == 2 & relate < 3 else FALSE) %>%
  ungroup
saveRDS(lis, "lis.RDS")

# Function to calculate average working hours for married women without children under 6
childless_women <- function(ostan) {
  sum(filter(lis, province == ostan & maritalstatus == 1)$hours) /
    sum(filter(lis, province == ostan & maritalstatus == 1)$IW_Yearly)
}

# Calculate and print max and min average working hours for married women without children under 6
childless <- sapply(0:30, childless_women)
print(which.max(childless) - 1) 
print(which.min(childless) - 1) 

# Function to calculate unemployment rate for married women without children under 6
married_unemployee <- function(ostan) {
  sum(filter(lis, province == ostan & ActivityStatus == 2 & maritalstatus == 1)$IW_Yearly) /
    sum(filter(lis, province == ostan & ActivityStatus != 3 & maritalstatus == 1)$IW_Yearly)
}

# Calculate and print max and min unemployment rate for married women without children under 6
unemployee_women <- sapply(0:30, married_unemployee)
print(which.max(unemployee_women)) 
print(which.min(unemployee_women)) 

# Calculate and print the average working hours for married women without children under 6 for Iran
print(sum(filter(lis, maritalstatus == 1)$hours) / sum(filter(lis, maritalstatus == 1)$IW_Yearly))
