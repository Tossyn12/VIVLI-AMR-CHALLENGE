data2 = read.csv("/Users/oluwamayowaakinsuyi/Desktop/Result_amr/COMPLETESTAPH.csv")
View(data2)
#Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')
total_males 

# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Levofloxacin_I== 'Resistant', ]
resistant_females 
resistant_males <- data2[data2$Gender == 'Male' & data2$Levofloxacin_I== 'Resistant', ]
resistant_males 
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
v = barplot(values, names.arg = labels, col = bar_colors, ylab = "Percentage", xlab = "Gender", ylim = c(0, 100))
v
# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

###################
# Step 1: Calculate the total number of cases and resistant cases for each country
library(dplyr)
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
g = barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
        ylab = "Percentage", ylim = c(0, 100), las = 1, cex.names = 0.7)
# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

######
library(dplyr)
data2_summary <- data2 %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Levofloxacin_I == 'Resistant')
  )
View(data2_summary)
data2_summary = data2_summary[-7,]

`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
g = barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Age.Group, col = bar_colors, las = 2,
            ylab = "Percentage", xlab = "Age.Group", ylim = c(0, 100), las = 1, cex.names = 0.7)
# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)



##############################3
data2 = read.csv("/Users/oluwamayowaakinsuyi/Desktop/Result_amr/completepseudo1.csv")
View(data2)





#Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')
total_males 

# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Levofloxacin_I== 'Resistant', ]
resistant_females 
resistant_males <- data2[data2$Gender == 'Male' & data2$Levofloxacin_I== 'Resistant', ]
resistant_males 
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
v = barplot(values, names.arg = labels, col = bar_colors, ylab = "Percentage", xlab = "Gender", ylim = c(0, 100))
v
# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

#########

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
f = barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
        ylab = "Percentage", ylim = c(0, 100), las = 3, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

#########
# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Levofloxacin_I == 'Resistant')
  )
View(data2_summary)

data2_summary = data2_summary [-7,]
`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
k = barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Age.Group, col = bar_colors, las = 2,
            ylab = "Percentage", xlab = "Age.Group", ylim = c(0, 100), las = 1, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)


####Meropenem
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
#Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')
total_males 

# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Meropenem== 'Resistant', ]
resistant_females 
resistant_males <- data2[data2$Gender == 'Male' & data2$Meropenem== 'Resistant', ]
resistant_males 
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
g = barplot(values, names.arg = labels, col = bar_colors, ylab = "Percentage", xlab = "Gender", ylim = c(0, 100))
g
# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)
########################################3
# Assuming you have 'dplyr' installed, load the library
library(dplyr)
View(data)
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
 ffg =  barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
            ylab = "Percentage", ylim = c(0, 100), las = 3, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

#########
# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Meropenem == 'Resistant')
  )
View(data2_summary)

data2_summary = data2_summary [-7,]
`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
kkb = barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Age.Group, col = bar_colors, las = 2,
            ylab = "Percentage", xlab = "Age.Group", ylim = c(0, 100), las = 1, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)






####### PIperacillin






#########
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
#Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')
total_males 

# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Ceftazidime== 'Resistant', ]
resistant_females 
resistant_males <- data2[data2$Gender == 'Male' & data2$Ceftazidime== 'Resistant', ]
resistant_males 
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
g = barplot(values, names.arg = labels, col = bar_colors, ylab = "Percentage", xlab = "Gender", ylim = c(0, 100))
g
# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

#############
#######################################3
# Assuming you have 'dplyr' installed, load the library
library(dplyr)
View(data)
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
ffg =  barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
               ylab = "Percentage", ylim = c(0, 100), las = 3, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

#########
# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Meropenem == 'Resistant')
  )
View(data2_summary)

data2_summary = data2_summary [-7,]
`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
kkb = barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Age.Group, col = bar_colors, las = 2,
              ylab = "Percentage", xlab = "Age.Group", ylim = c(0, 100), las = 1, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)



















####### PIperacillin

data2$Piperacillin.tazobactam<- gsub("[><=]", "", data2$Piperacillin.tazobactam)

# Function to convert numeric values to categories
Piparacilin_category <- function(value) {
  value <- as.numeric(value)  # Convert to numeric
  
  if (value <= 4) {
    return("Susceptible")
  } else if (value <= 8 && value < 16) {
    return("Intermediate")
  } else if (value >= 16) {
    return("Resistant")
  } else {
    return("Intermediate")  # Assign as Intermediate for any other values between 8 and 16
  }
}

Replace numeric values in the 'ceftazidime' column with categories
data2$Piperacillin.tazobactam <- sapply(data2$Piperacillin.tazobactam, Piparacilin_category)
View(data2)
#Step 1: Calculate the total number of females and males
total_females <- sum(data2$Gender == 'Female')
total_females
total_males <- sum(data2$Gender == 'Male')
total_males 

# Step 2: Filter the data for resistant cases for each gender
resistant_females <- data2[data2$Gender == 'Female' & data2$Piperacillin.tazobactam== 'Resistant', ]
resistant_females 
resistant_males <- data2[data2$Gender == 'Male' & data2$Piperacillin.tazobactam== 'Resistant', ]
resistant_males 
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
g = barplot(values, names.arg = labels, col = bar_colors, ylab = "Percentage", xlab = "Gender", ylim = c(0, 100))
g
# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

#####
#######################################3
# Assuming you have 'dplyr' installed, load the library
library(dplyr)
View(data)
# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Country) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Piperacillin.tazobactam == 'Resistant')
  )
View(data2_summary)

`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
ffgj=  barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Country, col = bar_colors, las = 2,
               ylab = "Percentage", ylim = c(0, 100), las = 3, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)

#########
# Step 1: Calculate the total number of cases and resistant cases for each country
data2_summary <- data2 %>%
  group_by(Age.Group) %>%
  summarise(
    total_cases = n(),
    resistant_cases = sum(Piperacillin.tazobactam == 'Resistant')
  )
View(data2_summary)

data2_summary = data2_summary [-7,]
`# Step 2: Calculate the percentage of resistant cases for each country
data2_summary <- data2_summary %>%
  mutate(percentage_resistant = (resistant_cases / total_cases) * 100)

# Step 3: Plot the bar chart
bar_colors <- rainbow(nrow(data2_summary))  # Colors for each country
kkbu = barplot(data2_summary$percentage_resistant, names.arg = data2_summary$Age.Group, col = bar_colors, las = 2,
              ylab = "Percentage", xlab = "Age.Group", ylim = c(0, 100), las = 1, cex.names = 0.7)

# Add x-axis line
abline(h = 0, col = "gray", lwd = 2)


