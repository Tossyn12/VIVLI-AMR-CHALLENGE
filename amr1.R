data2 = read.csv("/Users/oluwamayowaakinsuyi/Desktop/Result_amr/completepseudo.csv")
View(data2)
OR
# Assuming you have the data stored in a data frame called 'data2'


##############
#########Meropenem
# Clean 'ceftazidime' values by removing ">" and "<" signs and the equal to sign
data2$Meropenem<- gsub("[><=]", "", data2$Meropenem)

# Function to convert numeric values to categories
Meropenem_category <- function(value) {
  value <- as.numeric(value)  # Convert to numeric
  
  if (value <= 2) {
    return("Susceptible")
  } else if (value >4 && value < 8) {
    return("Intermediate")
  } else if (value >= 8) {
    return("Resistant")
  } else {
    return("Intermediate")  # Assign as Intermediate for any other values between 8 and 16
  }
}

data2$Meropenem <- sapply(data2$Meropenem,Meropenem_category)

#############

#Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')
total_males 

# Step 2: Filter the data for resistant cases for each gender
females <- data2[data2$Gender == 'Female' & data2$Meropenem== 'Resistant', ]
females
dim(resistant_females)
males <- data2[data2$Gender == 'Male' & data2$Meropenem== 'Resistant', ]
males 
# Step 3: Calculate the count of females and males with resistance to LVX_MIC
count_resistant_females <- nrow(resistant_females)
count_resistant_males <- nrow(resistant_males)
count_resistant_males
# Step 4: Calculate the percentage of females and males with resistance to LVX_MIC
percentage_resistant_females <- (count_resistant_females / total_females) * 100
percentage_resistant_females
percentage_resistant_males <- (count_resistant_males / total_males) * 100
percentage_resistant_males

# Values and label =s for the bar chart
labels <- c('Males', 'Females')
values <- c(percentage_resistant_males, percentage_resistant_females)

# Plot the bar chart
bar_colors <- c('blue', 'Orange')
v = barplot(values, names.arg = labels, col = bar_colors, ylab = "Percentage", ylim = c(0, 100))
v

#####Ceftazidime
############# Function to convert numeric values to categories

Clean 'ceftazidime' values by removing ">" and "<" signs and the equal to sign
data2$Ceftazidime <- gsub("[><=]", "", data2$Ceftazidime)

ceftazidime_category <- function(value) {
  # Remove rows with NA values
  value <- value[!is.na(value)]
  
  # Convert to numeric
  value <- as.numeric(value)
  
  if (length(value) == 0) {
    return("NA")  # Return "NA" if there are no valid numeric values
  } else if (any(value <= 8)) {
    return("Susceptible")
  } else if (any(value >= 16 & value < 32)) {
    return("Intermediate")
  } else if (all(value >= 32)) {
    return("Resistant")
  } else {
    return("Intermediate")  # Assign as Intermediate for any other values between 8 and 16
  }
}

# Replace numeric values in the 'ceftazidime' column with categories
data2$Ceftazidime <- sapply(data2$Ceftazidime, ceftazidime_category)
View(data2)

#############

#Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')
total_males 
Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Ceftazidime == 'Resistant', ]
resistant_females

resistant_males <- data2[data2$Gender == 'Male' & data2$Ceftazidime == 'Resistant', ]

# Step 2: Filter the data for resistant cases for each gender
females <- data2[data2$Gender == 'Female' & data2$Meropenem== 'Resistant', ]
females
dim(resistant_females)
males <- data2[data2$Gender == 'Male' & data2$Meropenem== 'Resistant', ]
males 
# Step 3: Calculate the count of females and males with resistance to LVX_MIC
count_resistant_females <- nrow(resistant_females)
count_resistant_males <- nrow(resistant_males)
count_resistant_males
# Step 4: Calculate the percentage of females and males with resistance to LVX_MIC
percentage_resistant_females <- (count_resistant_females / total_females) * 100
percentage_resistant_females
percentage_resistant_males <- (count_resistant_males / total_males) * 100
percentage_resistant_males

# Values and label =s for the bar chart
labels <- c('Males', 'Females')
values <- c(percentage_resistant_males, percentage_resistant_females)

# Plot the bar chart
bar_colors <- c('blue', 'Orange')
v = barplot(values, names.arg = labels, col = bar_colors, ylab = "Percentage", ylim = c(0, 100))
v





























# Plot the bar chart
bar_colors <- c('blue', 'Orange')
v = barplot(values, names.arg = labels, col = bar_colors, main = "Percentage of male and female carrying Meropenem resistant P.aeruginosa \nTotal Male and Female Population", ylab = "Percentage / Population", ylim = c(0, 100))
v
################
# Assuming you have 'dplyr' installed, load the library
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Country) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Meropenem == 'Resistant')
  )
View(data2_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
        main = "Percentage of Resistant Cases to Meropenem in Different Countries",
        ylab = "Percentage", ylim = c(0, 100), las = 2, cex.names = 0.7)















# Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')

# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Levofloxacin_I == 'Resistant', ]
resistant_females
dim(resistant_females)
resistant_males <- data2[data2$Gender == 'M' & data2$Levofloxacin_I == 'Resistant', ]

# Step 3: Calculate the count of females and males with resistance to LVX_MIC
count_resistant_females <- nrow(resistant_females)
count_resistant_males <- nrow(resistant_males)

# Step 4: Calculate the percentage of females and males with resistance to LVX_MIC
percentage_resistant_females <- (count_resistant_females / total_females) * 100
percentage_resistant_females
percentage_resistant_males <- (count_resistant_males / total_males) * 100
percentage_resistant_males

# Values and labels for the bar chart
labels <- c('Resistant Males', 'Resistant Females')
values <- c(percentage_resistant_males, percentage_resistant_females)

# Plot the bar chart
bar_colors <- c('blue', 'Orange')
barplot(values, names.arg = labels, col = bar_colors, main = "Percentage of Resistant Males and Females to Lenofloxacin \nTotal Male and Female Population", ylab = "Percentage / Population", ylim = c(0, 100))


#################
unique(data$Country)



# Assuming you have 'dplyr' installed, load the library
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Country) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Levofloxacin_I == 'Resistant')
  )
View(data2_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
        main = "Percentage of Resistant Meropenem resistant P.aeruginosa in Different Countries",
        ylab = "Percentage", ylim = c(0, 100), las = 2, cex.names = 0.7)


#################
# Assuming you have 'dplyr' installed, load the library
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Meropenem == 'Resistant')
  )
View(data2_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)
View(data2_summary)
data2_summary  = data2_summary[-7,]


# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Age.Group, col = bar_colors, las = 2,
        main = "Percentage of Meropenem Resistant P.aeruginosa in Different Countries", las = 2,
        ylab = "Percentage", ylim = c(0, 100), cex.names = 0.7)  # Reduce the size of the x-axis labels



###########
########## Clean 'Piparacilin' values by removing ">" and "<" signs and the equal to sign
data2$Piperacillin.tazobactam<- gsub("[><=]", "", data2$Piperacillin.tazobactam)

# Function to convert numeric values to categories

Clean 'ceftazidime' values by removing ">" and "<" signs and the equal to sign
data2$Ceftazidime <- gsub("[><=]", "", data2$Ceftazidime)

ceftazidime_category <- function(value) {
  # Remove rows with NA values
  value <- value[!is.na(value)]
  
  # Convert to numeric
  value <- as.numeric(value)
  
  if (length(value) == 0) {
    return("NA")  # Return "NA" if there are no valid numeric values
  } else if (any(value <= 8)) {
    return("Susceptible")
  } else if (any(value >= 16 & value < 32)) {
    return("Intermediate")
  } else if (all(value >= 32)) {
    return("Resistant")
  } else {
    return("Intermediate")  # Assign as Intermediate for any other values between 8 and 16
  }
}

# Replace numeric values in the 'ceftazidime' column with categories
data2$Ceftazidime <- sapply(data2$Ceftazidime, ceftazidime_category)
View(data2)

########

# Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')

# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Ceftazidime == 'Resistant', ]
resistant_females
dim(resistant_females)
resistant_males <- data2[data2$Gender == 'Male' & data2$Ceftazidime == 'Resistant', ]

# Step 3: Calculate the count of females and males with resistance to LVX_MIC
count_resistant_females <- nrow(resistant_females)
count_resistant_males <- nrow(resistant_males)

# Step 4: Calculate the percentage of females and males with resistance to LVX_MIC
percentage_resistant_females <- (count_resistant_females / total_females) * 100
percentage_resistant_females
percentage_resistant_males <- (count_resistant_males / total_males) * 100
percentage_resistant_males

# Values and labels for the bar chart
labels <- c('Resistant Males', 'Resistant Females')
values <- c(percentage_resistant_males, percentage_resistant_females)

# Plot the bar chart
bar_colors <- c('blue', 'Orange')
barplot(values, names.arg = labels, col = bar_colors, main = "Percentage of Resistant Males and Females to Ceftazidime \nTotal Male and Female Population", ylab = "Percentage / Population", ylim = c(0, 100))


#################
unique(data$Country)



# Assuming you have 'dplyr' installed, load the library
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Country) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Ceftazidime== 'Resistant')
  )
View(data2_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
        main = "Percentage of Resistant Ceftazidime resistant P.aeruginosa in Different Countries",
        ylab = "Percentage", ylim = c(0, 100), las = 2, cex.names = 0.7)


#################
# Assuming you have 'dplyr' installed, load the library
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Ceftazidime == 'Resistant')
  )
View(data2_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)
View(data2_summary)
data2_summary  = data2_summary[-7,]


# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Age.Group, col = bar_colors, las = 2,
        main = "Percentage of Ceftazidime Resistant P.aeruginosa in Different Countries", las = 2,
        ylab = "Percentage", ylim = c(0, 100), cex.names = 0.7)  # Reduce the size of the x-axis labels



#############
# Create a contingency table
contingency_table <- table(data$Age.Group, data$Levofloxacin_I)
contingency_table 


#### 
data2 = read.xl


# Calculate the proportions of resistance for each gender
total_02 <- sum(contingency_table["0 to 2 Years", ])
total_13_18 <- sum(contingency_table["13 to 18 Years", ])
total_19_64 <- sum(contingency_table["19 to 64 Years", ])
total_3_12 <- sum(contingency_table["3 to 12 Years", ])
total_65_84 <- sum(contingency_table["65 to 84 Years",])
total_65_84 
total_85_over <- sum(contingency_table["85 and Over",])
total_85_over

proportions <- prop.table(contingency_table, margin = 1)

Calculate the proportions of resistant males and females
resistant_02_proportion<- contingency_table['0 to 2 Years', 'Resistant'] / total_02 
resistant_13_18_proportion<- contingency_table['13 to 18 Years', 'Resistant'] / total_13_18 
resistant_19_64_proportion<- contingency_table['19 to 64 Years', 'Resistant'] / total_19_64 
resistant_3_12_proportion<- contingency_table['3 to 12 Years', 'Resistant'] / total_3_12 
resistant_65_84_proportion<- contingency_table['65 to 84 Years', 'Resistant'] / total_65_84 
resistant_85_over_proportion<- contingency_table['85 and Over', 'Resistant'] / total_85_over
resistant_85_over_proportion

chi2_result <- chisq.test(contingency_table)
print("Chi-square:", chi2_result$statistic)
print("P-value:", chi2_result$p.value)

chi2_result 





########### Age
contingency_table <- table(data$Age.Group, data$Levofloxacin_I)

# Calculate the proportions of resistance for each gender
total_02 <- sum(contingency_table["0 to 2 Years", ])
total_13_18 <- sum(contingency_table["13 to 18 Years", ])
total_19_64 <- sum(contingency_table["19 to 64 Years", ])
total_3_12 <- sum(contingency_table["3 to 12 Years", ])
total_65_84 <- sum(contingency_table["65 to 84 Years",])
total_65_84 
total_85_over <- sum(contingency_table["85 and Over",])
total_85_over


proportions <- prop.table(contingency_table, margin = 1)

# Calculate the proportions of resistant males and females
Calculate the proportions of resistant males and females
resistant_02_proportion<- contingency_table['0 to 2 Years', 'Resistant'] / total_02 
resistant_02_proportion
resistant_13_18_proportion<- contingency_table['13 to 18 Years', 'Resistant'] / total_13_18 
resistant_19_64_proportion<- contingency_table['19 to 64 Years', 'Resistant'] / total_19_64 
resistant_3_12_proportion<- contingency_table['3 to 12 Years', 'Resistant'] / total_3_12 
resistant_65_84_proportion<- contingency_table['65 to 84 Years', 'Resistant'] / total_65_84 
resistant_85_over_proportion<- contingency_table['85 and Over', 'Resistant'] / total_85_over
resistant_85_over_proportion



contingency_table <- table(data$Age.Group, data$Levofloxacin_I)
chi2_result <- chisq.test(contingency_table)

# Perform the chi-square test
chi2_result <- chisq.test(contingency_table)
chi2_result$

# Print the test statistics and p-value
print("Chi-square:", chi2_result$statistic)
print("P-value:", chi2_result$p.value)

# Interpret the results
if (chi2_result$p.value < 0.05) {
  print("The percentage of resistant gender significantly influences the resistance to TZP_MIC.")
} else {
  print("There is no significant influence of the percentage of resistant gender on the resistance to TZP_MIC.")
}

# Display the proportions
print("\nProportions of Resistance:")
print(proportions)

# Display the proportions of resistant males and females
print("\nProportions of Resistant Males and Females:")
print("Resistant Males:", resistant_males_proportion)
print("Resistant Females:", resistant_females_proportion)



##############3
contingency_table <- table(data$Gender, data$Levofloxacin_I)

# Calculate the proportions of resistance for each gender
total_males <- sum(contingency_table['Male', ])
total_females <- sum(contingency_table['Female', ])

proportions <- prop.table(contingency_table, margin = 1)

# Calculate the proportions of resistant males and females
resistant_males_proportion <- contingency_table['Male', 'Resistant'] / total_males
resistant_females_proportion <- contingency_table['Female', 'Resistant'] / total_females

# Perform the chi-square test
chi2_result <- chisq.test(contingency_table)
chi2_result$statistic

# Print the test statistics and p-value
print("Chi-square:", chi2_result$statistic)
print("P-value:", chi2_result$p.value)

# Interpret the results
if (chi2_result$p.value < 0.05) {
  print("The percentage of resistant gender significantly influences the resistance to TZP_MIC.")
} else {
  print("There is no significant influence of the percentage of resistant gender on the resistance to TZP_MIC.")
}

# Display the proportions
print("\nProportions of Resistance:")
print(proportions)

# Display the proportions of resistant males and females
print("\nProportions of Resistant Males and Females:")
print("Resistant Males:", resistant_males_proportion)
print("Resistant Females:", resistant_females_proportion)






###########
# Assuming you have already created the contingency_table and performed the chi-squared test
# Assuming you have already created the contingency_table and performed the chi-squared test
# Assuming you have already created the contingency_table and performed the chi-squared test
contingency_table <- table(data$Age.Group, data$Levofloxacin_I)
chi2_result <- chisq.test(contingency_table)
chi2_result
# Calculate residuals
residuals <- residuals(chi2_result)

# Add the residuals to the contingency table
contingency_table_with_residuals <- as.table(addmargins(residuals))

# Print the contingency table with residuals
print(contingency_table_with_residuals)


#######
## Assuming you have already created the contingency_table
# contingency_table <- table(data$Age.Group, data$Levofloxacin_I)

# Perform Chi-squared test
install.packages("stats")
chi2_result <- chisq.test(contingency_table)

# Assuming you have already created the contingency_table
# contingency_table <- table(data$Age.Group, data$Levofloxacin_I)

# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table, simulate.p.value = TRUE)
fisher_result 
# Print the results
print(fisher_result)



##############
data = read.csv('/Users/oluwamayowaakinsuyi/Desktop/Result_amr/COMPLETESTAPH.csv')
# Clean 'Leno' values by removing ">" and "<" signs and the equal to sign

# Step 1: Calculate the total number of females and males
total_females <- sum(data$Gender == 'Female')
total_females
total_males <- sum(data$Gender == 'Male')
total_males 
# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data[data$Gender == 'Female' & data$Levofloxacin_I == 'Resistant', ]
resistant_females
dim(resistant_females)
resistant_males <- data[data$Gender == 'Male' & data$Levofloxacin_I == 'Resistant', ]

# Step 3: Calculate the count of females and males with resistance to LVX_MIC
count_resistant_females <- nrow(resistant_females)
count_resistant_males <- nrow(resistant_males)

# Step 4: Calculate the percentage of females and males with resistance to LVX_MIC
percentage_resistant_females <- (count_resistant_females / total_females) * 100
percentage_resistant_females
percentage_resistant_males <- (count_resistant_males / total_males) * 100
percentage_resistant_males

# Values and labels for the bar chart
labels <- c('Resistant Males', 'Resistant Females')
values <- c(percentage_resistant_males, percentage_resistant_females)

# Plot the bar chart
bar_colors <- c('blue', 'Orange')
barplot(values, names.arg = labels, col = bar_colors, main = "% of Resistant Males and Females with Lenofloxacin resistant Pseudomonas aeruginosa \nTotal Male and Female Population", ylab = "Percentage / Population", ylim = c(0, 100))

#######
##############
data = read.csv('/Users/oluwamayowaakinsuyi/Desktop/Result_amr/COMPLETESTAPH.csv')
# Clean 'Leno' values by removing ">" and "<" signs and the equal to sign

# Step 1: Calculate the total number of females and males
total_females <- sum(data$Gender == 'Female')
total_females
total_males <- sum(data$Gender == 'Male')
total_males 
# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data[data$Gender == 'Female' & data$Levofloxacin_I == 'Resistant', ]
resistant_females
dim(resistant_females)
resistant_males <- data[data$Gender == 'Male' & data$Levofloxacin_I == 'Resistant', ]

# Step 3: Calculate the count of females and males with resistance to LVX_MIC
count_resistant_females <- nrow(resistant_females)
count_resistant_males <- nrow(resistant_males)

# Step 4: Calculate the percentage of females and males with resistance to LVX_MIC
percentage_resistant_females <- (count_resistant_females / total_females) * 100
percentage_resistant_females
percentage_resistant_males <- (count_resistant_males / total_males) * 100
percentage_resistant_males

# Values and labels for the bar chart
labels <- c('Resistant Males', 'Resistant Females')
values <- c(percentage_resistant_males, percentage_resistant_females)

# Plot the bar chart
bar_colors <- c('blue', 'Orange')
barplot(values, names.arg = labels, col = bar_colors, main = "% of Resistant Males and Females with Lenofloxacin resistant S.aureus \nTotal Male and Female Population", ylab = "Percentage / Population", ylim = c(0, 100))

#######
# Assuming you have 'dplyr' installed, load the library
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data_summary <- data %>%
  group_by(Country) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Levofloxacin_I == 'Resistant')
  )
View(data_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data_summary <- data_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data_summary))  # Colors for each country
barplot(data_summary$percentage_resistant, names.arg = data_summary$Country, col = bar_colors, las = 2,
        main = "Percentage of Resistant Levofloxacin resistant S.aureus in Different Countries",
        ylab = "Percentage", ylim = c(0, 100), las = 2, cex.names = 0.7)



data_summary <- data %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Levofloxacin_I == 'Resistant')
  )
View(data_summary)
data_summary = data_summary[-7,]

`# Step 2: Calculate the percentage of resistant cases for each country
data_summary <- data_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data_summary))  # Colors for each country
barplot(data_summary$percentage_resistant, names.arg = data_summary$Age.Group, col = bar_colors, las = 2,
        main = "Percentage of  Levofloxacin resistant S.aureus by Age.group",
        ylab = "Percentage", ylim = c(0, 100), las = 2, cex.names = 0.7)



























View(data)
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data_summary <- data %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(data$Levofloxacin_I == 'Resistant')
  )
View(data_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data_summary <- data_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)
View(data_summary)
data_summary  = data_summary[-7,]


# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data_summary))  # Colors for each country
barplot(data_summary$percentage_resistant, names.arg = data_summary$Age.Group, col = bar_colors, las = 2,
        main = "Percentage of lenofloxacin resistant S.aureus by age group", las = 2,
        ylab = "Percentage", ylim = c(0, 100), cex.names = 0.7)  # Reduce the size of the x-axis labels



################3
data3 = read.csv("/Users/oluwamayowaakinsuyi/Desktop/Result_amr/KEYSTON2.csv")
Clean 'ceftazidime' values by removing ">" and "<" signs and the equal to sign
data3$Ceftazidime <- gsub("[><=]", "", data3$Ceftazidime)

ceftazidime_category <- function(value) {
  # Remove rows with NA values
  value <- value[!is.na(value)]
  
  # Convert to numeric
  value <- as.numeric(value)
  
  if (length(value) == 0) {
    return("NA")  # Return "NA" if there are no valid numeric values
  } else if (any(value <= 8)) {
    return("Susceptible")
  } else if (any(value >= 16 & value < 32)) {
    return("Intermediate")
  } else if (all(value >= 32)) {
    return("Resistant")
  } else {
    return("Intermediate")  # Assign as Intermediate for any other values between 8 and 16
  }
}



# Replace numeric values in the 'ceftazidime' column with categories
data3$Ceftazidime <- sapply(data3$Ceftazidime, ceftazidime_category)

View(data3)
Step 1: Calculate the total number of females and males
total_females <- sum(data3$Gender == 'F')
total_females
total_males <- sum(data3$Gender == 'M')
total_males 
# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data3[data3$Gender == 'F' & data3$Ceftazidime == 'Resistant', ]
resistant_females
dim(resistant_females)
resistant_males <- data[data3$Gender == 'M' & data3$Ceftazidime == 'Resistant', ]

# Step 3: Calculate the count of females and males with resistance to LVX_MIC
count_resistant_females <- nrow(resistant_females)
count_resistant_males <- nrow(resistant_males)

# Step 4: Calculate the percentage of females and males with resistance to LVX_MIC
percentage_resistant_females <- (count_resistant_females / total_females) * 100
percentage_resistant_females
percentage_resistant_males <- (count_resistant_males / total_males) * 100
percentage_resistant_males

# Values and labels for the bar chart
labels <- c('Resistant Males', 'Resistant Females')
values <- c(percentage_resistant_males, percentage_resistant_females)

# Plot the bar chart
bar_colors <- c('blue', 'Orange')
barplot(values, names.arg = labels, col = bar_colors, main = "% of Resistant Males and Females with Lenofloxacin resistant S.aureus \nTotal Male and Female Population", ylab = "Percentage / Population", ylim = c(0, 100))

#######
# Assuming you have 'dplyr' installed, load the library
library(dplyr)

# Step 1: Calculate the total number of cases and resistant cases for each country
data_summary <- data %>%
  group_by(Country) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Levofloxacin_I == 'Resistant')
  )
View(data_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data_summary <- data_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data_summary))  # Colors for each country
barplot(data_summary$percentage_resistant, names.arg = data_summary$Country, col = bar_colors, las = 2,
        main = "Percentage of Resistant Levofloxacin resistant S.aureus in Different Countries",
        ylab = "Percentage", ylim = c(0, 100), las = 2, cex.names = 0.7)





