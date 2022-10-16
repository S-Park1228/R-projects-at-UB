# Set the working directory. If needed, change the directory below.
setwd("C:/Users/1suof/Desktop")

# 0. Install the necessary libraries in advance.
install.packages('dplyr')
install.packages('cowplot')
install.packages('sm')
install.packages('tidyverse')
install.packages('performance')



###########################################################################
###########################################################################
###########################################################################



# 1. Loading the Data Set and Understanding its Structure

# Loading data
data = read.csv(file = "Data Group 7.csv", header = TRUE)
data = as.data.frame(data)
names(data) # Checking the variable names
dim(data) # Checking the number of the observations and the variables
str(data) # Browsing the data set
sum(is.na(data)) # Checking the existence of missing values in this data set

# Data Wrangling - Target Variable
library(dplyr)
# the number of "Existing" in the target variable
nrow(filter(data, Attrition_Flag == "Existing"))
# the number of "Attrited" in the target variable
nrow(filter(data, Attrition_Flag == "Attrited"))

# Data Wrangling - Categorical Features
# the differences within each categorical feature at a glance by the target category
# 1. Gender
table_gender1 <- table(data$Attrition_Flag, data$Gender)
prop_gender1 <- addmargins(round(prop.table(table_gender1) * 100, 1))
prop_gender1 # Contingency table for Gender
prop_gender2 <- round(prop.table(table_gender1, 1) * 100, 1) # by Attrition_Flag
prop_gender2
data$Gender2 <- ifelse(data$Gender == 'M', 1,
                       ifelse(data$Gender == 'F', 2, ''))
data$Gender2 <- as.integer(data$Gender2)
library(ggplot2)
ggplot_gender <- ggplot(data = data, aes(x = Gender2, fill = Attrition_Flag)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, position = 'identity') +
  theme_minimal() +
  facet_wrap(~ Attrition_Flag) +
  labs(title = 'Gaps by Gender') +
  stat_count(aes(y = ..count.. * 0, label = ifelse(..count.. / sum(..count..) < 0.07, '42.8%', ifelse(..count.. / sum(..count..) < 0.1, '57.2%', ifelse(..count.. / sum(..count..) < 0.41, '47.9%', '52.1%')))), geom = "text", vjust = -7, size = 5) +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'none',
        strip.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c('M', 'F'))
#ggplot(data = data, aes(x = Gender, fill = Attrition_Flag)) +
#  geom_bar(stat = 'identity', position = position_dodge()) +
#  geom_text(aes(label = Gender), vjust = 1.6, color = 'white',
#            position = position_dodge(0.9), size = 3.5) +
#  scale_fill_brewer(palette = 'Paired') +
#  theme_minimal()
#
#colors <- c("dodgerblue3", "skyblue1")
#y = as.matrix(t(prop_gender2))
#barplot_gender <- barplot(t(prop_gender2),
#                          ylab = 'Share by Gender (%)',
#                          ylim = c(0, 70),
#                          cex.lab = 1.5,
#                          cex.names = 1.5,
#                          col = colors, beside = TRUE) +
#  text(x = barplot_gender, y + 3, labels = as.character(y), cex = 1.5) +
#  legend("topright", legend = c("F", "M"), fill = colors)
# 2. Dependent_count
table_dependent_count1 <- table(data$Attrition_Flag, data$Dependent_count)
prop_dependent_count1 <- addmargins(round(prop.table(table_dependent_count1) * 100,
                                          1))
prop_dependent_count1 # Contingency table for Number of Dependents
prop_dependent_count2 <- round(prop.table(table_dependent_count1, 1) * 100, 1) # by Attrition_Flag
prop_dependent_count2
ggplot_dep <- ggplot(data = data, aes(x = Dependent_count, fill = Attrition_Flag)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, position = 'identity') +
  theme_minimal() +
  labs(title = 'Distributions: Number of Dependents') +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = unique(data$Dependent_count))
# 3. Education_Level
table_education_level1 <- table(data$Attrition_Flag, data$Education_Level)
colnames(table_education_level1) <- c(4, 7, 5, 3, 6, 2, 1)
prop_education_level1 <- addmargins(round(prop.table(table_education_level1) * 100,
                                          1))
prop_education_level1 # Contingency table for Education Level
data$Education_Level2 <- ifelse(data$Education_Level == 'Unknown', 1,
                                ifelse(data$Education_Level == 'Uneducated', 2,
                                       ifelse(data$Education_Level == 'High School', 3, ifelse(data$Education_Level == 'College', 4, ifelse(data$Education_Level == 'Graduate', 5, ifelse(data$Education_Level == 'Post-Graduate', 6, ifelse(data$Education_Level == 'Doctorate', 7, '')))))))
data$Education_Level2 <- as.integer(data$Education_Level2)
ggplot_edu <- ggplot(data = data, aes(x = Education_Level2, fill = Attrition_Flag)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, position = 'identity') +
  theme_minimal() +
  labs(title = 'Distributions: Education Levels') +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'bottom',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c('Unknown', 'Uneducated', 'High School', 'College',
                                'Graduate', 'Post-Graduate', 'Doctorate'))
# 4. Marital_Status
table_marital_status1 <- table(data$Attrition_Flag, data$Marital_Status)
prop_marital_status1 <- addmargins(round(prop.table(table_marital_status1) * 100, 1))
prop_marital_status1 # Contingency table for Marital_Status
data$Marital_Status2 <- ifelse(data$Marital_Status == 'Single', 1,
                               ifelse(data$Marital_Status == 'Married', 2,
                                      ifelse(data$Marital_Status == 'Divorced', 3, ifelse(data$Marital_Status == 'Unknown', 4, ''))))
data$Marital_Status2 <- as.integer(data$Marital_Status2)
ggplot_marital <- ggplot(data = data, aes(x = Marital_Status2, fill = Attrition_Flag)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, position = 'identity') +
  theme_minimal() +
  labs(title = 'Distributions: Marital_Status') +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1, 2, 3, 4),
                     labels = c('Single', 'Married', 'Divorced', 'Unknown'))
# 5. Income_Category
table_income_category1 <- table(data$Attrition_Flag, data$Income_Category)
prop_income_category1 <- addmargins(round(prop.table(table_income_category1) * 100, 1))
prop_income_category1 # Contingency table for Marital_Status
data$Income_Category2 <- ifelse(data$Income_Category == 'Unknown', 1,
                                ifelse(data$Income_Category == 'Less than $40K', 2,
                                       ifelse(data$Income_Category == '$40K - $60K', 3, ifelse(data$Income_Category == '$60K - $80K', 4, ifelse(data$Income_Category == '$80K - $120K', 5, ifelse(data$Income_Category == '$120K +', 6, ''))))))
data$Income_Category2 <- as.integer(data$Income_Category2)
ggplot_income <- ggplot(data = data, aes(x = Income_Category2, fill = Attrition_Flag)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, position = 'identity') +
  theme_minimal() +
  labs(title = 'Distributions: Income Category') +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6),
                     labels = c('Unknown', 'Less than $40K', '$40K-$60K', '$60K-$80K', '$80K-$120K', '$120K+'))
# 6. Card_Category
table_card_category1 <- table(data$Attrition_Flag, data$Card_Category)
prop_card_category1 <- addmargins(round(prop.table(table_card_category1) * 100, 1))
prop_card_category1 # Contingency table for Marital_Status
prop_card_category2 <- round(prop.table(table_card_category1, 1) * 100, 1) # by Attrition_Flag
prop_card_category2
data$Card_Category2 <- ifelse(data$Card_Category == 'Blue', 1,
                              ifelse(data$Card_Category == 'Silver', 2,
                                     ifelse(data$Card_Category == 'Gold', 3, ifelse(data$Card_Category == 'Platinum', 4, ''))))
data$Card_Category2 <- as.integer(data$Card_Category2)
ggplot_card <- ggplot(data = data, aes(x = Card_Category2, fill = Attrition_Flag)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.5, position = 'identity') +
  theme_minimal() +
  labs(title = 'Distributions: Card Category') +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'bottom',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1, 2, 3, 4),
                     labels = c('Blue', 'Silver', 'Gold', 'Platinum'))
#colors_card <- c("dodgerblue4", "dodgerblue2", "skyblue1", "white")
#barplot_card_category <- barplot(t(prop_card_category2),
#                          ylab = 'Share by Card Category (%)',
#                          ylim = c(0, 100),
#                          cex.lab = 1.5,
#                          cex.names = 1.5,
#                          col = colors_card, beside = TRUE)
#y = as.matrix(t(prop_card_category2))
#text(x = barplot_card_category, y + 3, labels = as.character(y), cex = 1.5)
#legend("topright", legend = c("Blue", "Silver", "Gold", "Platinum"), fill = colors_card)

# Combining the plots for the categorical features
library(cowplot)
plot_grid(ggplot_gender, ggplot_dep, ggplot_edu, ncol = 1, nrow = 3)
plot_grid(ggplot_marital, ggplot_income, ggplot_card, ncol = 1, nrow = 3)

# Data Wrangling - Continuous Features
# the differences within each continuous feature at a glance by the target category
# 1. Customer_Age
ggplot(data = data,
       aes(x = Attrition_Flag, y = Customer_Age, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# 2. Months_on_book
ggplot(data = data,
       aes(x = Attrition_Flag, y = Months_on_book, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# Take a closer look (kernel dist.) at Months_on_book due to many outliers.
library(sm)
sm.density.compare(data$Months_on_book, data$Attrition_Flag, xlab = "Months_on_book")
title(main = "Months_on_book Distribution by Attrition")
Attrition_Flag.f <- factor(data$Attrition_Flag, levels = c(1, 0),
                           labels = c('Attrited', 'Existing'))
colfill <- c(2:(2 + length(levels(Attrition_Flag.f))))
# legend(locator(1), levels(Attrition_Flag.f), fill = colfill) # Adding legend via mouse click.
# 3. Total_Relationship_Count (used in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Total_Relationship_Count, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# 4. Months_Inactive_12_mon (used in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Months_Inactive_12_mon, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# 5. Contacts_Count_12_mon (used in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Contacts_Count_12_mon, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# 6. Credit_Limit (addition needed, not in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Credit_Limit, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# Take a closer look (kernel dist.) at Credit_Limit due to many outliers.
sm.density.compare(data$Credit_Limit, data$Attrition_Flag, xlab = "Credit Limit")
title(main = "Credit Limit Distribution by Attrition")
colfill <- c(2:(2 + length(levels(Attrition_Flag.f))))
# legend(locator(1), levels(Attrition_Flag.f), fill = colfill) # Adding legend via mouse click.
# 7. Total_Revolving_Bal (used in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Total_Revolving_Bal, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# 8. Avg_Open_To_Buy
ggplot(data = data,
       aes(x = Attrition_Flag, y = Avg_Open_To_Buy, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# Take a closer look (kernel dist.) at Avg_Open_To_Buy due to many outliers.
sm.density.compare(data$Avg_Open_To_Buy, data$Attrition_Flag, xlab = "Avg_Open_To_Buy")
title(main = "Avg_Open_To_Buy Distribution by Attrition")
colfill <- c(2:(2 + length(levels(Attrition_Flag.f))))
# legend(locator(1), levels(Attrition_Flag.f), fill = colfill) # Adding legend via mouse click.
# 9. Total_Amt_Chng_Q4_Q1
ggplot(data = data,
       aes(x = Attrition_Flag, y = Total_Amt_Chng_Q4_Q1, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
sm.density.compare(data$Total_Amt_Chng_Q4_Q1, data$Attrition_Flag, xlab = "Total_Amt_Chng_Q4_Q1")
title(main = "Total_Amt_Chng_Q4_Q1 Distribution by Attrition")
colfill <- c(2:(2 + length(levels(Attrition_Flag.f))))
# legend(locator(1), levels(Attrition_Flag.f), fill = colfill) # Adding legend via mouse click.
# 10. Total_Trans_Amt (Used in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Total_Trans_Amt, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# 11. Total_Trans_Ct (Used in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Total_Trans_Ct, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# 12. Total_Ct_Chng_Q4_Q1
ggplot(data = data,
       aes(x = Attrition_Flag, y = Total_Ct_Chng_Q4_Q1, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
sm.density.compare(data$Total_Ct_Chng_Q4_Q1, data$Attrition_Flag, xlab = "Total_Ct_Chng_Q4_Q1")
title(main = "Total_Ct_Chng_Q4_Q1 Distribution by Attrition")
colfill <- c(2:(2 + length(levels(Attrition_Flag.f))))
# legend(locator(1), levels(Attrition_Flag.f), fill = colfill) # Adding legend via mouse click.
# 13. Avg_Utilization_Ratio (Used in the draft)
ggplot(data = data,
       aes(x = Attrition_Flag, y = Avg_Utilization_Ratio, fill = Attrition_Flag)) +
  geom_boxplot(alpha = 0.75)
# Take a closer look (kernel dist.) at Avg_Utilization_Ratio due to many outliers.
sm.density.compare(data$Avg_Utilization_Ratio, data$Attrition_Flag, xlab = "Avg_Utilization_Ratio")
title(main = "Avg_Utilization_Ratio Distribution by Attrition")
colfill <- c(2:(2 + length(levels(Attrition_Flag.f))))
# legend(locator(1), levels(Attrition_Flag.f), fill = colfill) # Adding legend via mouse click.

# Combining the plots for the continuous features
library(cowplot)
bp1 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Total_Relationship_Count, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
bp2 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Months_Inactive_12_mon, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
bp3 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Contacts_Count_12_mon, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
bp4 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Credit_Limit, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
bp5 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Total_Revolving_Bal, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
bp6 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Total_Trans_Amt, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
bp7 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Total_Trans_Ct, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
bp8 <- ggplot(data = data,
              aes(x = Attrition_Flag, y = Avg_Utilization_Ratio, fill = 
                    Attrition_Flag)) +
  geom_boxplot(alpha = 0.75) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank()) +
  coord_flip()
plot_grid(bp1, bp2, bp3 + remove("x.text"),
          ncol = 1, nrow = 3)
plot_grid(bp4, bp5, bp6, bp7, bp8 + remove("x.text"),
          ncol = 1, nrow = 5)



###################################################################
###################################################################
###################################################################



library(tidyverse)
library(caret)
library(performance)

# 1. Data cleaning ----

churn <- read.csv("Data Group 7.csv")

churn <-
  churn %>% select(
    -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1,
    -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2,
    -CLIENTNUM
  ) %>% mutate(
    Attrition_Flag = factor(Attrition_Flag),
    Income_Category = factor(Income_Category),
    Gender = factor(Gender),
    Marital_Status = factor(Marital_Status),
    Education_Level = factor(Education_Level),
    Card_Category = factor(Card_Category)
  ) %>%
  separate(col = Attrition_Flag,
           c("Attrition_Flag", "Customer"),
           sep = " ") %>%
  select(-Customer)

churn %>% glimpse()

## Factor the dependent variable
churn$Attrition_Flag <- factor(
  churn$Attrition_Flag,
  levels = c("Existing", "Attrited"),
  labels = c(1, 0)
)

## Correlation matrix
churn_cor <- churn
churn_cor[] <- lapply(churn_cor, as.double)
cor(churn_cor)
which(cor(churn_cor) > 0.80 & cor(churn_cor) < 1, arr.ind = T)

## Variables with correlation greater than 0.8
churn_cor_var <-
  churn %>% select(Credit_Limit,
                   Avg_Open_To_Buy,
                   Total_Trans_Amt,
                   Total_Trans_Ct)
cor(churn_cor_var)

## Removing multi-collinear variables
churn <- churn %>% select(-Avg_Open_To_Buy, -Total_Trans_Ct)

## Visualization ----

temp <- ggplot(data = churn, aes(churn$Customer_Age, fill = churn$Attrition_Flag)) +
  geom_histogram(binwidth = 3, color = "black")+
  facet_wrap(~ churn$Income_Category, ncol = 3, scales = "free") +
  labs(title = "Attrition_Flag Based on Customer_Age Distribution")
temp + facet_grid(vars(churn$Income_Category)) +
  theme(strip.text = element_text(size = 15))


churn %>% ggplot(aes(Age, Total_Trans_Amt , color = Attrition_Flag)) +
  geom_point()+
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  facet_wrap(~ Sex, ncol = 2) +
  labs(title = "Credit Amount vs. Age Distribution",
       y = "Credit Amount")


churn %>% mutate(Total_Relationship_Count = factor(Total_Relationship_Count)) %>% 
  group_by(Total_Relationship_Count) %>% 
  tally() %>% 
  ggplot(aes(reorder(Total_Relationship_Count, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Total number of products held by the customers", 
       y = " ",
       x = "Total_Relationship_Count")

#3. Data Analysis ----

## Applying RFE for feature selection
RFE <- rfe(
  churn[, 2:18],
  churn[, 1],
  sizes = c(1:10),
  rfeControl = rfeControl(functions = rfFuncs,
                          method = "cv",)
)

all_var <- RFE$variables
importance <-
  all_var %>% filter(Variables == 5, Resample == "Fold01") %>%
  relocate(var, .before = Overall) %>%
  select(var, Overall)

## Important features visualization
importance %>% ggplot(aes(x = reorder(var, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance Using RFE",
       y = "Importance",
       x = " ")


## Splitting the data set into train and test set
set.seed(123)

training <-
  createDataPartition(y = churn$Attrition_Flag,
                      p = 0.75,
                      list = FALSE)


train_set = churn[training, ]
test_set  = churn[-training, ]

control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = F,
  savePredictions = T
)

## Predicting the attrition_Flag from the train and test set.

## Logistic regression
fit_glm <-
  train(
    Attrition_Flag ~ Total_Trans_Amt + Total_Ct_Chng_Q4_Q1 + Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Total_Revolving_Bal,
    data = train_set,
    method = "glm",
    trControl = control
  )
fit_glm_train_pred <-
  confusionMatrix(predict(fit_glm, newdata = train_set), train_set$Attrition_Flag)

fit_glm_test_pred <-
  confusionMatrix(predict(fit_glm, newdata = test_set), test_set$Attrition_Flag)

## CART
fit_cart <-
  train(
    Attrition_Flag ~ Total_Trans_Amt + Total_Ct_Chng_Q4_Q1 + Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Total_Revolving_Bal,
    data = train_set,
    method = "rpart",
    trControl = control
  )
fit_cart_train_pred <-
  confusionMatrix(predict(fit_cart, newdata = train_set), train_set$Attrition_Flag)

fit_cart_test_pred <-
  confusionMatrix(predict(fit_cart, newdata = test_set), test_set$Attrition_Flag)


## Bagging
fit_bag <-
  train(
    Attrition_Flag ~ Total_Trans_Amt + Total_Ct_Chng_Q4_Q1 + Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Total_Revolving_Bal,
    data = train_set,
    method = "treebag",
    trControl = control
  )
fit_bag_train_pred <-
  confusionMatrix(predict(fit_bag, newdata = train_set), train_set$Attrition_Flag)

fit_bag_test_pred <-
  confusionMatrix(predict(fit_bag, newdata = test_set), test_set$Attrition_Flag)


## Boosting
fit_boost <-
  train(
    Attrition_Flag ~ Total_Trans_Amt + Total_Ct_Chng_Q4_Q1 + Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Total_Revolving_Bal,
    data = train_set,
    method = "gbm",
    trControl = control
  )
fit_boost_train_pred <-
  confusionMatrix(predict(fit_boost, newdata = train_set), train_set$Attrition_Flag)

fit_boost_test_pred <-
  confusionMatrix(predict(fit_boost, newdata = test_set), test_set$Attrition_Flag)


## Random Forest
fit_rf <-
  train(
    Attrition_Flag ~ Total_Trans_Amt + Total_Ct_Chng_Q4_Q1 + Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Total_Revolving_Bal,
    data = train_set,
    method = "rf",
    trControl = control
  )
fit_rf_train_pred <-
  confusionMatrix(predict(fit_rf, newdata = train_set), train_set$Attrition_Flag)

fit_rf_test_pred <-
  confusionMatrix(predict(fit_rf, newdata = test_set), test_set$Attrition_Flag)


## KNN
fit_knn <-
  train(
    Attrition_Flag ~ Total_Trans_Amt + Total_Ct_Chng_Q4_Q1 + Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Total_Revolving_Bal,
    data = train_set,
    method = "knn",
    trControl = control
  )


fit_knn_train_pred <-
  confusionMatrix(predict(fit_knn, newdata = train_set), train_set$Attrition_Flag)

fit_knn_test_pred <-
  confusionMatrix(predict(fit_knn, newdata = test_set), test_set$Attrition_Flag)


## Naive Bayes
fit_nb <-
  train(
    Attrition_Flag ~ Total_Trans_Amt + Total_Ct_Chng_Q4_Q1 + Total_Relationship_Count + Total_Amt_Chng_Q4_Q1 + Total_Revolving_Bal,
    data = train_set,
    method = "nb",
    trControl = control
  )
fit_nb_train_pred <-
  confusionMatrix(predict(fit_nb, newdata = train_set), train_set$Attrition_Flag)

fit_nb_test_pred <-
  confusionMatrix(predict(fit_nb, newdata = test_set), test_set$Attrition_Flag)