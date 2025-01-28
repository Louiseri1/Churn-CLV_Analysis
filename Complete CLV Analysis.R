# Following packages are needed to run the whole script:
install.packages("tidyverse")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("psych")
install.packages("jtools")
install.packages("pROC")
install.packages("magrittr")
library(tidyverse)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(psych)
library(jtools)
library(pROC)
library(magrittr)

#########################################################################
######################          TASK 1        ###########################
#########################################################################


#####################           Part 1            #######################
#########################################################################

# Read data
arima_data <- readRDS("~/Documents/Echange M2/COURS/Customer Analytics in a Digital World/Final project/datasets/arima_data.RDS")

# number of columns (variables)
ncol(arima_data)

# number of observations
nrow(arima_data)

# names of columns (variables)
colnames(arima_data)

# Look at the data, prints out the 6th top rows
head(arima_data, n = 6)

# Check summary of dataframe
summary(arima_data)

# Check if there are any NAs in the data
any(is.na(arima_data)) # False 
sum(is.na(arima_data)) # 0 
colSums(is.na(arima_data)) # 0 in all columns
# No NAs (missing values in the data

# Controlling for duplicates 
any(duplicated(arima_data)) # False 
sum(duplicated(arima_data)) # 0 
# No duplicates in the data


# Check the length for the variables: 
lenf = function(x) length(table(x)) 
# Above: Function for length for each variable in a dataframe
length_dis_arima_data = sapply(arima_data,lenf) # Show length for each variable
length_dis_arima_data
# Print the names of the variables with length less than 15
low_length_vars <- print(names(length_dis_arima_data[length_dis_arima_data < 15]))

# Checking the class for each variables: 
sapply(arima_data, class)
# COMMENT
# We notice that the columns churmD, DM, HHSize holds integer values 
# (such as 0 / 1 etc.) have the datatype <integer>. 
# We convert these columns to factors
# Additionally, we notice that the column Gender holds character values 
# (such as Female / Male) have the datatype <chr>. 
# Hence, we must convert gender to factors. 

# Convert character columns to factors (in this case gender)
arima_data <- arima_data %>% 
  mutate_if(is.character, as.factor)
# Convert churnD, HHSize, DM to factors:
#arima_data$churnD <- as.factor(arima_data$churnD)
arima_data$DM <- as.factor(arima_data$DM)
# arima_data$HHSize <- as.factor(arima_data$HHSize) this needs to be numerical to calculate average HHSize


###### Should we convert Age, Year, Month to factor variables as well??? ######
# Here I`m Converting Year and Month to factor variables, but I do not change
# Age, so I let Age be converted to numeric variable below: 
#arima_data$Year <- as.factor(arima_data$Year)
#arima_data$Month <- as.factor(arima_data$Month)
#########################################################################

# Converting following integer columns
# ID, Age, HHIncome, RelationshipL, Web_Visits, Facebook_Page_Views, Publicity,
# to numerical columns:   
arima_data <- arima_data %>% 
  mutate_if(is.integer, as.numeric)

# Control that we successfully converted the variables to our desirable class 
sapply(arima_data, class)
# Looks good

# Check summary of the modified dataframe
summary(arima_data)
# COMMENT
# The summary show the minimum, 1st quartile, median, mean, 3rd wuarile and max
# values of the numeric and integer columns. 
# For the categorical column gender, 
# we see the number of times each gender is counted


# The number of donations per household saved in dataframe called numOfDonations: 
numOfDonations <- aggregate(arima_data$ID,
                            by = list(arima_data$ID),
                            FUN = length)
# changing column names
colnames(numOfDonations) <- c("ID", "NumberofDonations")
# print the first six rows of this data frame to the console
head(numOfDonations, 6)
# average number of donations per households
mean(numOfDonations$NumberofDonations) # 14.69099

# obtain a list of unique household IDs
listOfUniqueID <- unique(arima_data$ID)
# number of unique IDs in the data set before removing any observations, 
# due to outliers, abnormal values etc. 
uniqueID_before_removal = length(listOfUniqueID)
uniqueID_before_removal # 2000

# exclude all repeat information about hhSize
HHSize_per_Don <- aggregate(arima_data$HHSize,
                            by = list(arima_data$ID),
                            FUN = unique)
# change column names
colnames(HHSize_per_Don) <- c("ID", "HHSize")
# print the first six rows
head(HHSize_per_Don)
# average household size
mean(HHSize_per_Don$HHSize) # 2.417715


# Creating a dataset called customers_grouped, 
# where we group each customer to calculate the percentage of lost customers
customers_grouped <- arima_data %>%
  group_by(ID) %>% 
  filter(if (any(churnD == 1)) row_number() == which(churnD == 1)[1] else
    row_number() == which.max(churnD == 0))
number_lost = sum(customers_grouped$churnD == 1)
number_lost # prints the number of lost customers
proportion_lost = 100*(number_lost / nrow(customers_grouped))
cat("Proportion of lost customers is", proportion_lost, "%\n")

# Plot of whether an customer decided to stay with the program throughout the 
# whole period (Succesfull Customer) or if the donor churnD at any time (Lost Customer)
plot_lost_donors <- arima_data %>%
  group_by(ID) %>% #  filter on unique ID
  filter(if (any(churnD == 1)) row_number() == which(churnD == 1)[1] else
         row_number() == which.max(churnD == 0)) %>%
  ggplot(aes(churnD, fill = churnD)) +
  geom_histogram(stat = "count", colour = "black", 
                 aes(fill = factor(churnD)), position = "stack") +
  scale_fill_manual(values = c("deepskyblue2", "brown1"), 
                    labels = c("Succesfull Customers", "Lost Customers")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  ggtitle("Distribution of Succesfull vs. Lost Customers")
plot_lost_donors
# COMMENT
# The plot displays a large number of lost customers, compared 
# to successful customers, which confirms the high customer churn rate. 


# Further look at a correlation heatmap of the numeric variables

# Calculate correlation between each pairwise combination of variables
cor_df <- round(cor(arima_data %>% select(where(is.numeric))), 2)

# Melt the data frame
melted_cormat <- melt(cor_df)

# Plot heatmap
ggplot(data = melted_cormat, aes(x  = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(Var1, Var2, label = value), size = 2) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name = "Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
# COMMENT 
# We notice that RealationshipL and year are highly correlated (corr = 0.97)
# and should not be included in the same linear models due to issues with 
# multicolinearity.  
# We also see that TV_Adv and Month have an relatively high correlation 
# (corr = 0.46) with each other. 


# Run describe on multiple variables at once before removing outliers 
describe(arima_data[, c("HHIncome", "TV_Adv", "Publicity", "Web_Visits", 
                        "Facebook_Adv","Facebook_Page_Views", "RelationshipL")])
# We see that "TV_Adv" have some abnormally high values 

# Histogram of TV_Adv
ggplot(arima_data, aes(TV_Adv)) +
  geom_histogram(binwidth = 200, color = "black", fill = "blue") +
  ggtitle("Histogram of TV_Adv") +
  xlab("X") +
  ylab("Count")

# Histogram of HHIncome
ggplot(arima_data, aes(HHIncome)) +
  geom_histogram(binwidth = 200, color = "black", fill = "blue") +
  ggtitle("Histogram of HHIncome") +
  xlab("X") +
  ylab("Count")




####################################################
##############   Removing outliers   ###############  
####################################################

########## Method 1 - IQR method with Boxplot #############

dim(arima_data)[1] # number of observations before removing outliers: 31929

## Box plot of "HHIncome"
arima_data %>%
  ggplot(aes(HHIncome)) +
  geom_boxplot() +
  theme_bw() 
# COMMENT
# We notice several outliers (the dots), we update the dateframe
# after removing them

# Identify outliers in HHIncome using the boxplot.stats() function
outliers_HHIncome <- boxplot.stats(arima_data$HHIncome)$out
outliers_HHIncome
# Remove outliers from HHIncome
arima_data <- arima_data[!(arima_data$HHIncome %in% outliers_HHIncome), ]
dim(arima_data)[1] # number obs: 31819


## Box plot of "TV_Adv"
arima_data %>%
  ggplot(aes(TV_Adv)) +
  geom_boxplot() +
  theme_bw() 
# COMMENT
# We notice some outliers (the dots), we update the dateframe
# after removing them

# Identify outliers in TV_Adv using the boxplot.stats() function
outliers_TV_Adv <- boxplot.stats(arima_data$TV_Adv)$out
outliers_TV_Adv
# Remove outliers from TV_Adv
arima_data <- arima_data[!(arima_data$TV_Adv %in% outliers_TV_Adv), ]
dim(arima_data)[1] # number obs: 30451


## Box plot of "Facebook_Adv"
arima_data %>%
  ggplot(aes(Facebook_Adv)) +
  geom_boxplot() +
  theme_bw() 
# COMMENT
# Looks good. We detect no outliers (dots) for Facebook_Adv. 


# Box plot of "Publicity"
arima_data %>%
  ggplot(aes(Publicity)) +
  geom_boxplot() +
  theme_bw() 
# COMMENT
# We notice outlier, we update the dateframe after removing the outlier

# Identify outliers in Publicity using the boxplot.stats() function
outliers_Publicity <- boxplot.stats(arima_data$Publicity)$out
outliers_Publicity
# Remove outliers from Publicity
arima_data <- arima_data[!(arima_data$Publicity %in% outliers_Publicity), ]
dim(arima_data)[1] # number obs: 29947


## Box plot of "Web_Visits"
arima_data %>%
  ggplot(aes(Web_Visits)) +
  geom_boxplot() +
  theme_bw()  
# COMMENT
# We notice one outliers (the dot), we update the dateframe
# after the outlier

# Identify outliers in Web_Visits using the boxplot.stats() function
outliers_Web_Visits <- boxplot.stats(arima_data$Web_Visits)$out
outliers_Web_Visits
# Remove outliers from Web_Visits
arima_data <- arima_data[!(arima_data$Web_Visits %in% outliers_Web_Visits), ]
dim(arima_data)[1] # number obs: 29647


## Box plot of "Facebook_Page_Views"
arima_data %>%
  ggplot(aes(Facebook_Page_Views)) +
  geom_boxplot() +
  theme_bw() 
# COMMENT
# We notice one outliers (the dots), we update the dateframe
# after removing the outlier

# Identify outliers in Facebook_Page_Views using the boxplot.stats() function
outliers_Facebook_Page_Views <- boxplot.stats(arima_data$Facebook_Page_Views)$out
outliers_Facebook_Page_Views
# Remove outliers from Facebook_Page_Views
arima_data <- arima_data[!(arima_data$Facebook_Page_Views %in% outliers_Facebook_Page_Views), ]

dim(arima_data)[1] # number of observations after removing outliers: 29191
############### Finished Removing Outliers ###############


# obtain a list of unique household IDs after removing observations, 
# outliers, abnormal values etc. 
listOfUniqueID_after_removal <- unique(arima_data$ID)
# number of unique IDs in the data set before removing any observations
uniqueID_after_removal = length(listOfUniqueID_after_removal) # 1987
uniqueID_after_removal 
# number of unique IDs removed from our data: 
uniqueID_removed = uniqueID_before_removal - uniqueID_after_removal 
uniqueID_removed # prints number of uniqueID_removed from the orginal dataset


# Histogram of TV_Adv
ggplot(arima_data, aes(TV_Adv)) +
  geom_histogram(binwidth = 200, color = "black", fill = "blue") +
  ggtitle("Histogram of TV_ADV") +
  xlab("X") +
  ylab("Count")
# After removing outliers, we see that we also have removed abornomal values
# for "TV_Adv"

# Run describe on multiple variables at once after removing Outliers
describe(arima_data[, c("HHIncome", "TV_Adv", "Publicity", "Web_Visits", 
                        "Facebook_Adv","Facebook_Page_Views", "RelationshipL")])

# Plot of churn distribution
arima_data %>%
  group_by(ID) %>%
  filter(if (any(churnD == 1)) row_number() == which(churnD == 1)[1] else
    row_number() == which.max(churnD == 0)) %>%
  ggplot(aes(churnD, fill = churnD)) +
  geom_histogram(stat = "count", colour = "black") +
  scale_fill_manual(values = c("deepskyblue2", "brown1"), 
                    labels = c("Not Churned", "Churned")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  ggtitle("Distribution of Churn Variable")
# This code will filter on unique ID and only display households where churnD
# is either 1 or 0, depending on whether the household ever churned or not.
# If a household churnD at any given time during all the observations, this
# code will keep only one observation where churnD = 1. If all observations for
# a specific ID, is equal to churnD == 0, then this code will keep only one
# observation where churnD = 0.


# Converting churnD to an factor: 
arima_data$churnD <- as.factor(arima_data$churnD)

# Distribution of all observations whether a donor churnD or not
arima_data %>%
  group_by(ID) %>%
  filter(any(churnD == 1)) %>%
  ggplot(aes(churnD, fill = churnD)) +
  geom_histogram(stat = "count", colour = "black") +
  scale_fill_manual(values = c("deepskyblue2", "brown1"), 
                    labels = c("Not Churned", "Churned")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  ggtitle("Distribution of Churn Variable") 

# Plot of churn distribution
arima_data  %>%
  ggplot(aes(churnD, fill = churnD)) +
  geom_histogram(stat = "count", colour = "black") +
  scale_fill_manual(values = c("deepskyblue2", "brown1"), 
                    labels = c("Not Churned", "Churned")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  ggtitle("Distribution of Churn Variable") 
# There is a very big difference in terms of observations with regard to a
# donor churned or not

# Dividing into two dataframes: 
df.churnD.1 <- arima_data[arima_data$churnD == 1, ] # One with churns
df.churnD.0 <- arima_data[arima_data$churnD == 0, ] # One without churns 
summary(df.churnD.1)
summary(df.churnD.0)
# Finding proportions for the variables for churnD = 1 and churnD = 0 
# First and foremost, it is worth noting that the two factor levels are very 
# different in terms of observations. 
# Out of the full data set on 31098 observations, only 1705 of these
# actual made an churn (churn = 1). 
# This could have an effect on the logistic regression, seeing that
# such model prefer the two levels to be equally large.

Gender1 <- prop.table(table(df.churnD.1$Gender))
Gender0 <- prop.table(table(df.churnD.0$Gender))
Gender.prop <- cbind(Gender1, Gender0) # Column bind the two values
Gender.prop
# Some differences between the genders whether a donor churned or not

HHSize1 <- prop.table(table(df.churnD.1$HHSize))
HHSize0 <- prop.table(table(df.churnD.0$HHSize))
HHSize.prop <- cbind(HHSize1, HHSize0) # Column bind the two values
HHSize.prop
# The HHSize groups are almost identical 

DM1 <- prop.table(table(df.churnD.1$DM))
DM0 <- prop.table(table(df.churnD.0$DM))
DM.prop <- cbind(DM1, DM0) # Column bind the two values
DM.prop
# Some differences in the number of the nonprofit`s direct contacts 
# with the donor

Year1 <- prop.table(table(df.churnD.1$Year))
Year0 <- prop.table(table(df.churnD.0$Year))
Year.prop <- cbind(Year1, Year0) # Column bind the two values
Year.prop
# Quite some difference between years

Month1 <- prop.table(table(df.churnD.1$Month))
Month0 <- prop.table(table(df.churnD.0$Month))
Month.prop <- cbind(Month1, Month0) # Column bind the two values
Month.prop
# Some differences between months, but not so big as years


#########
### COMMENT 
## Maybe we should divide age into intervals??
age1 <- prop.table(table(df.churnD.1$Age))
age0 <- prop.table(table(df.churnD.0$Age))
age.prop <- cbind(age1, age0) # Column bind the two values
age.prop
## Maybe we should divide age into intervals??
#########

### Plots of integer and numerical variables vs. churnD
g1 <- ggplot(arima_data, aes(x = churnD, y = HHIncome, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) + 
  theme(legend.position = "none") + 
  ggtitle("HHIncome vs churnD") 

g2 <- ggplot(arima_data, aes(x = churnD, y = RelationshipL, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) + 
  theme(legend.position = "none") + 
  ggtitle("RelationshipL vs churnD") 

g3 <- ggplot(arima_data, aes(x = churnD, y = TV_Adv, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.position = "none") + 
  ggtitle("TV_Adv vs churnD") 

g4 <- ggplot(arima_data, aes(x = churnD, y = Facebook_Adv, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.position = "none") + 
  ggtitle("Facebook_Adv vs churnD") 

g5 <- ggplot(arima_data, aes(x = churnD, y = Publicity, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.position = "none") + 
  ggtitle("Publicity vs churnD") 

g6 <- ggplot(arima_data, aes(x = churnD, y = Web_Visits, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.position = "none") + 
  ggtitle("Web_Visits vs churnD") 

g7 <- ggplot(arima_data, aes(x = churnD, y = Facebook_Page_Views, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.position = "none") + 
  ggtitle("Facebook_Page_Views vs churnD") 

g8 <- ggplot(arima_data, aes(x = churnD, y = Age, fill = churnD)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("deepskyblue2", "brown1")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.position = "none") + 
  ggtitle("Age vs churnD") 

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2) 
grid.arrange(g5, g6, g7, g8, ncol = 2, nrow = 2)

# Based on these boxplots, RelationshipL have high variation between 
# wheter a donor churned or not. 
# Hence RelationshipL seems to be highly relevant predictor.
# There is some variation between Publicity and churnD, 
# Facebook_Page_Views and churnD, but this variation is not high. 
# Looking at the other boxplots, the means are pretty much centered 
# around for all variables at all levels.

# Hence, the most promising predictors seems to be: 
# RelationshipL, gender?, years?, .....

# However, drawback of such pairwise comparisons is that we do not observe how each 
# variable affect other ones within a model. 
# We also can not measure of the variables together affect the response variable,
# seeing that some variables might be somewhat correlated. 
# Hence, we can not observe collinearity between the variables we implement into our model.
# Thereby, since we discovered that years is highly correlated with RelationhipL, 
# it should not be included in the same models with RelationshipL 
# due to issues with multicolinearity. The other variables mentioned above
# will be carried over to the next task. 



#########################################################################
#################       Part 2 - Churn Analysis     #####################
#########################################################################

##################################################################
###################     2A) and 2B)    ###########################
##################################################################

#################################################
############### MODEL ESTIMATION ################
#################################################

# Create binomial logistic regression model in 2a) by using the 
# the whole dataset (no splitting between train and test data)
logit_model_2a <- glm(churnD ~ DM + TV_Adv + Facebook_Adv + Publicity, 
                     family = binomial(link = "logit"),
                     arima_data)

# prints summary output from 
summary(logit_model_2a)

# obtain nicely edited output of regression model
summ(logit_model_2a , digits = 4) # we use the results of our glm function as input

# Interpreting the coefficients: 
# printing out the coefficients
coef(logit_model_2a) 
# compute the exponential of the coefficients (exp(B)) to interpret the effect sizesEXPb 
expB <- exp(coef(logit_model_2a)) # this give us the odds ratio of all predictors in our model
expB
# compute the the range of the numerical variables
range(arima_data["TV_Adv"])
range(arima_data["Facebook_Adv"])
range(arima_data["Publicity"])


########## 
# Calculating the correlation between the variables Facebook_Adv, TV_Adv, Publicity
# Select the variables of interest
selected_vars <- c("TV_Adv", "Facebook_Adv", "Publicity")
arima_data_selected <- arima_data %>% select(selected_vars)
# Calculate the correlation matrix and round the values
cor_df <- round(cor(arima_data_selected), 2)
cor_df # Print the correlation matrix
##########



####################################################################
#########################     2 C)    ##############################
####################################################################

#################################################
############### MODEL VALIDATION ################
#################################################


# Since each household often had several donations, we randomly select 75% of 
# the total number of households for the estimation sample and the rest for 
# the validation sample. This way we are able to retain all donations from
# a chosen household.

# create a list of all household IDs in the original data set
# we use the unique() function to remove duplicates
don_list <- unique(arima_data$ID)
# using the sample() function,
# we randomly select 70% of this list for estimation sample (indices = 1)
# and 30% for validation sample (indices = 2)
# see Tutorial lab session 4 for more details about the sample() function

# Set seed for reproducibilty
set.seed(123)
indices <- sample(1:2, # the group index (1 vs. 2)
                  length(don_list), # for how many customers do we need the index
                  replace = T, # reusing the two groups for all customers
                  prob = c(0.75, 0.25)) # probabilities of being in groups 1 and 2
# using the index above, create a list of customers for estimation sample
don_list_train <- don_list[indices == 1]
# using the index above, create a list of customers for estimation sample
don_list_test <- don_list[indices == 2]

# using the donation lists to select donors
# Train data
traindata <- arima_data[arima_data$ID %in% don_list_train, ]
# number of donors in the train data
length(unique(traindata$ID)) # 1498

# Test data
testdata <- arima_data[arima_data$ID %in% don_list_test, ]
# number of donors in the test data
length(unique(testdata$ID)) # 489


# Create binomial logistic regression model (logit-model) from 2a with the traindata
logit_model_2a <- glm(churnD ~ DM + TV_Adv + Facebook_Adv + Publicity, 
                      family = binomial(link = "logit"),
                      traindata)
# prints the 
summary(logit_model_2a)

# obtain nicely edited output of regression model
summ(logit_model_2a , digits = 4) # we use the results of our glm function as input


################################################################
##### Experimenting with different threshold on train data #####

## (Experimenting on the train data to avoid overfitting) 

traindata$pred_prob <-  predict(logit_model_2a, 
                                newdata = traindata, 
                                type = "response") 

# if predicted probability is larger than 0.5, then churnD = 1
# if predicted probability is smaller than or equal to 0.5, then churnD = 0
traindata$pred_churnD = ifelse(traindata$pred_prob > 0.5, 1, 0)

# Create a classification table using the table function.
# save the classification table in a object named class_tab
class_tab <- table(traindata$pred_churnD, traindata$churnD)
# print the results
class_tab

# Calculate error rate
err_rate <- 100*(class_tab[2] / sum(class_tab))
cat("The overall error rate is",round(err_rate, 3),"%\n")

# Notice we get a classification table where all predictions are 
# equivalent to not a single donor churned. 
# Thus, we see that our logistic regression misses all the people that 
# actually churned. 
# So while the overall error rate is low (5.692%), 
# the error rate of people who actually churned is very high 
# (in this case 100%), where all donors who churned are predicted to not churn.
# Since we are mainly interested in predicting actual churned frequency, we 
# want to adjust our model, so we could achieve an higher accuracy rate 
# with regard to predicting donors that actually churned. 


# Finding the range of our predicted probability column, 
# to get an indication of which level we should set our threshold 
# for deciding whether a donor churned or not
range(traindata$pred_prob)
range(traindata$pred_prob)[2] # 0.1309171
# COMMENT 
# We see that the highest predicted value is only 0.1309171 (13.09%)  
# So when setting a threshold equal to 0.50 if a donor churned or not, 
# then no donors is assigned as churned in our predictions. 
# Since the highest probability value is 0.1309 (13.09%), 
# we try a threshold of 0.1 (10%). 

# First we create a new data frame for testing different thresholds with the traindata: 
traindata_th_test <- traindata

# if predicted probability is larger than 0.1, then churnD = 1
# if predicted probability is smaller than or equal to 0.1, then churnedD = 0
traindata_th_test$pred_churnD_0.1 <- ifelse(traindata_th_test$pred_prob > 0.1, 1, 0)

# Create a classification table using the table function.
# save the classification table in a object named class_tab
class_tab_0.1 <- table(traindata_th_test$pred_churnD_0.1, traindata_th_test$churnD)
class_tab_0.1 # print the results

# We create a function for calculating precision (donors who actually churned)
precision_func <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

# Calculate overall error rate
overall_err_rate = 1-sum(diag(class_tab_0.1))/nrow(traindata_th_test)
cat("The overall error rate is", 100*(round(overall_err_rate, 4)),"%\n")
# The overall error rate is ok. 

# Calculate precision rate (donors who actually churned)
precision_rate = precision_func(class_tab_0.1) 
cat("The precision rate is", 100*(round(precision_rate, 4)),"%\n")
# The precision rate is still very low, therefore we try to set an lower 
# threshold value


# if predicted probability is larger than 0.05, then churnD = 1
# if predicted probability is smaller than or equal to 0.05, then churnedD = 0
traindata_th_test$pred_churnD_0.05  <- ifelse(traindata_th_test$pred_prob > 0.05, 1, 0)
# Create a classification table using the table function.
# save the classification table in a object named class_tab
class_tab_0.05 <- table(traindata_th_test$pred_churnD_0.05, 
                        traindata_th_test$churnD)
class_tab_0.05 # print the results

# Calculate overall error rate
overall_err_rate = 1-sum(diag(class_tab_0.05))/nrow(traindata_th_test)
cat("The overall error rate is", 100*(round(overall_err_rate, 4)),"%\n")
# The overall error rate is now very high, only 52.86 %. 

# Calculate precision rate (donors who actually churned)
precision_rate = precision_func(class_tab_0.05) 
cat("The precision rate is", 100*(round(precision_rate, 4)),"%\n")
# The precision rate is now 71.87 %, which is quite good. However, 
# the overall error rate as mentioned above is only 52.86 % so we will try 
# to increase the threshold. 


# if predicted probability is larger than 0.08, then churnD = 1
# if predicted probability is smaller than or equal to 0.08, then churnedD = 0
traindata_th_test$pred_churnD_0.08 <- ifelse(traindata_th_test$pred_prob > 0.08, 1, 0)
# Create a classification table using the table function.
# save the classification table in a object named class_tab
class_tab_0.08 <- table(traindata_th_test$pred_churnD_0.08, 
                        traindata_th_test$churnD)
class_tab_0.08 # print the results

# Calculate (overall) error rate 
overall_err_rate = 1-sum(diag(class_tab_0.08))/nrow(traindata_th_test)
cat("The overall error rate is", 100*(round(overall_err_rate, 4)),"%\n")
# The (overall) error rate have decreased drastically (from 52.86% to 26.31%) 
# when increasing the threshold (from 0.05 to 0.08). 

# Calculate precision rate (donors who actually churned)
precision_rate = precision_func(class_tab_0.08) 
cat("The precision rate is", 100*(round(precision_rate, 4)),"%\n")
# However, the precision rate is now only 52.06%, which is not very good. 
# Thus, we try an threeshold which are a little bit lower than 0.08


# if predicted probability is larger than 0.07, then churnD = 1
# if predicted probability is smaller than or equal to 0.07, then churnedD = 0
traindata_th_test$pred_churnD_0.07 <- ifelse(traindata_th_test$pred_prob > 0.07, 1, 0)
# Create a classification table using the table function.
# save the classification table in a object named class_tab
class_tab_0.07 <- table(traindata_th_test$pred_churnD_0.07, 
                        traindata_th_test$churnD)
class_tab_0.07 # print the results

# Calculate (overall) error rate 
overall_err_rate = 1-sum(diag(class_tab_0.07))/nrow(traindata_th_test)
cat("The overall error rate is", 100*(round(overall_err_rate, 4)),"%\n")
# The (overall) error rate have increasedd (from 26.31% to 33.2%) 
# when decreasing the threeshold (from 0.08 to 0.07). 

# Calculate precision rate (donors who actually churned)
precision_rate = precision_func(class_tab_0.07) 
cat("The precision rate is", 100*(round(precision_rate, 4)),"%\n")
# The precision rate is now 60.3%, which is quite decent. Hence this threeshold 
# seems more reasonable, due to (relatively) low decrease in (overall) error 
# rate and high increase in the precision rate (donors who actually churned). 
# We will still try to lower the threshold a little bit to see how big 
# increase we get in precision rate at the cost of increasing overall error rate. 


# if predicted probability is larger than 0.065, then churnD = 1
# if predicted probability is smaller than or equal to 0.065, then churnedD = 0
traindata_th_test$pred_churnD_0.065 <- ifelse(traindata_th_test$pred_prob > 0.065, 1, 0)
# Create a classification table using the table function.
# save the classification table in a object named class_tab
class_tab_0.065 <- table(traindata_th_test$pred_churnD_0.065, 
                         traindata_th_test$churnD)
class_tab_0.065 # print the results

# Calculate (overall) error rate 
overall_err_rate = 1-sum(diag(class_tab_0.065))/nrow(traindata_th_test)
cat("The overall error rate is", 100*(round(overall_err_rate, 4)),"%\n")
# The (overall) error rate have increased (from 33.2% to 38.39%) 
# when decreasing the threeshold (from 0.07 to 0.065). 

# Calculate precision rate (donors who actually churned)
precision_rate = precision_func(class_tab_0.065) 
cat("The precision rate is", 100*(round(precision_rate, 4)),"%\n")
# The precision rate have increased from 60.3% to 61.73%, 
# which is a slightly improvement.

# After, trying several different thresholds levels above, 
# both thresholds 0.065 and 0.07 seems reasonable to use. 
# Moving forward we will use the threshold of 0.07 as a baseline 
# when evaluating all other models. 




######################################################
########### Prediction on the test data ##############

# We create a function for calculating precision (donors who actually churned)
precision_func <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

# estimate the predicted probabilities on new (unseen) data
testdata$pred_prob <- predict(logit_model_2a, 
                              newdata = testdata, 
                              type = "response")
# select a threshold value of 0.07 
# if predicted probability is larger than 0.07, then churnD = 1
# if predicted probability is smaller than or equal to 0.07, then churnD = 0
testdata$pred_churnD <- ifelse(testdata$pred_prob > 0.07, 1, 0)

# create a classification table using the table function.
class_tab_test <- table(testdata$pred_churnD, testdata$churnD)
class_tab_test # print the results

# compute hit rate 
hitRate <- mean(testdata$pred_churnD == testdata$churnD)
cat("The hit rate is", 100*(round(hitRate, 4)), "%\n")

# compute error rate
overall_err_rate = 1-sum(diag(class_tab_test))/nrow(testdata)
cat("The overall error rate is", 100*(round(overall_err_rate, 4)),"%\n")

# Calculate precision rate (donors who actually churned, i.e. sensitivity to the model)
precision_rate = precision_func(class_tab_test) 
cat("The precision rate is", 100*(round(precision_rate, 4)),"%\n")


#####################################################################
#####################################################################

#########################################
########### Creating Models #############
#########################################

# Create binomial logistic regression model (logit-model) from 2c)
# where we do not include either DM and Publicity
logit_model_2c_1 <- glm(churnD ~ TV_Adv + Facebook_Adv, 
                        family = binomial(link = "logit"),
                        traindata)
summary(logit_model_2c_1) # prints the results
# obtain output of regression model
summ(logit_model_2c_1 , digits = 4) # using results from glm() function above


# Create binomial logistic regression model (logit-model) from 2c)
# where we include DM and not Publicity
logit_model_2c_2 <- glm(churnD ~ DM + TV_Adv + Facebook_Adv, 
                        family = binomial(link = "logit"),
                        traindata)
summary(logit_model_2c_2)
summ(logit_model_2c_2, digits = 4)

# Create binomial logistic regression model (logit-model) from 2c)
# where we include Publicity and not DM
logit_model_2c_3 <- glm(churnD ~ TV_Adv + Facebook_Adv + Publicity, 
                        family = binomial(link = "logit"),
                        traindata)
summary(logit_model_2c_3)
summ(logit_model_2c_3, digits = 4)

####################################################
######## Performance Evaluation of Models ##########
####################################################

# Store the models in a list, including the model name
models <- list("Logistic Regression Model 2a" = logit_model_2a, 
               "Logistic Regression Model 2c_1 without_DM_Publicity" = logit_model_2c_1, 
               "Logistic Regression Model 2c_2 without_Publicity" = logit_model_2c_2, 
               "Logistic Regression Model 2c_3 without_DM" = logit_model_2c_3)

# Initialize an empty list to store the results
results <- list()

# Iterate over the list of models
for (model_name in names(models)) {
  # Get the current model from the list
  current_model <- models[[model_name]]
  
  # Estimate the predicted probabilities on new (unseen) data
  testdata$pred_prob <- predict(current_model, 
                                newdata = testdata, 
                                type = "response")
  
  # Select a threshold value of 0.07 
  # If predicted probability is larger than 0.07, then churnD = 1
  # If predicted probability is smaller than or equal to 0.07, then churnD = 0
  testdata$pred_churnD <- ifelse(testdata$pred_prob > 0.07, 1, 0)
  
  # Create a classification table using the table function
  class_tab_test <- table(testdata$pred_churnD, testdata$churnD)
  
  # Compute hit rate 
  hitRate <- mean(testdata$pred_churnD == testdata$churnD)
  
  # Compute error rate
  overall_err_rate <- 1-sum(diag(class_tab_test))/nrow(testdata)
  
  # Calculate precision rate (donors who actually churned, i.e. sensitivity to the model)
  precision_rate <- precision_func(class_tab_test) 
  
  # Store the results in the list
  results[[model_name]] <- list(class_tab_test = class_tab_test,
                                hitRate = hitRate,
                                overall_err_rate = overall_err_rate,
                                precision_rate = precision_rate)
  
  # Print the results with the exact model name
  cat("Results for", model_name, "\n")
  print(class_tab_test)
  cat("The hit rate is", 100*(round(hitRate, 4)), "%\n")
  cat("The overall error rate is", 100*(round(overall_err_rate, 4)),"%\n")
  cat("The precision rate is", 100*(round(precision_rate, 4)),"%\n\n")
}
# print(results)


#####################################################
#############  Lift Charts & Lift Curve #############
#####################################################

# number of groups
numGroup <- 10 

### Crating a function which create a lift chart for each of the models 
create_lift_chart <- function(model_name) {
  
  # estimate the predicted probabilities on new (unseen) data
  pred_prob <- predict(model_name, 
                       newdata = testdata, 
                       type = "response")
  # Create a rank based on predicted probabilities
  rank <- rank(-pred_prob,
               ties.method = "first")
  
  # divide the donors into 10 equally sized groups
  lift_group <- cut(rank,
                    breaks = numGroup, 
                    labels = 1:numGroup)
  
  ## Need to convert to churnD to numeric in order to calculate the mean
  # Convert the factor variable to a numeric variable while maintaining the values 0 and 1
  churnD <- as.numeric(testdata$churnD) - 1
  
  # compute the mean of the donor rate for each group
  lift_chart <- aggregate(churnD,
                          by = list(lift_group),
                          FUN = mean)
  colnames(lift_chart) <- c("Decile", "Donor_Rate")
  
  # compute average donor rate across the entire test sample
  average_donor_rate <- mean(churnD)
  
  # compute the lift score
  lift_chart$Lift <- lift_chart$Donor_Rate / average_donor_rate
  
  # compute cumulative lift (%), used for the lift curve
  lift_chart$Cumulative_Lift <- (cumsum(lift_chart$Lift) / numGroup) * 100
  
  # round the results for Cumulative_Lift and Lift to 3 decimal places
  lift_chart$Cumulative_Lift <- round(lift_chart$Cumulative_Lift, digits = 3)
  lift_chart$Lift <- round(lift_chart$Lift, digits = 3)
  
  return(lift_chart)
}

# Store the models in a list
models <- list(logit_model_2a, logit_model_2c_1, logit_model_2c_2, logit_model_2c_3)

# Create a vector with all the models names
model_names <- c("logit_model_2a", "logit_model_2c_1", "logit_model_2c_2", "logit_model_2c_3")

# Create an empty list to store the lift_charts for each model
lift_charts <- list()

# Create a foor loop 
for (i in 1:length(models)) {
  lift_chart <- create_lift_chart(models[[i]])
  lift_charts[[model_names[i]]] <- lift_chart
  cat("\n", model_names[i], ":\n")
  print(lift_chart) # prints out lift_chart for each model
}

lift_charts # prints the lift_chart for all models 


## Creating the Lift Curve 
# cumulative percentage of the total customers by 10 deciles
percent_Cust <- ((1:numGroup)/numGroup)*100
# draw the lift curve of our model 2a, 2c_1, 2c_2, 2c_3:
plot(x = c(0, percent_Cust), # x coordinates
     y = c(0, lift_charts$logit_model_2a$Cumulative_Lift), # y coordinates, Model 2a
     type = "l", # type of plot, l is for lines
     lwd = "2", # line width
     col = "red", # line color
     xlab = "Cumulative % of Donors", # label for the X asis
     ylab = "Cumulative % of Donor-Rate", # label for the Y axis
     xlim = c(0, 100), # the limitations for the values on the X axis
     ylim = c(0, 100), # the limitations for the values on the Y axis
     yaxs = "i", # we want to plot from Y = 0
     xaxs = "i", # we want to plot from X = 0
     main = "") 
# adding the main title with the desired size and bold font
title(main = "Cumulative Lift Curve", 
      cex.main = 1.5, # increase the title size
      font.main = 2) # bold font 
# here we add the random selection line
# you must run the whole code including the part above
lines(x = c(0, percent_Cust),
      y = c(0, percent_Cust),
      type = "l",
      lwd = "2",
      col = "darkgreen")
# here we add another lift curve based on Model 2c_1
lines(x = c(0, percent_Cust), # x coordinates
      y = c(0, lift_charts$logit_model_2c_1$Cumulative_Lift), # y coordinates
      type = "l", # type of plot, l is for lines
      lwd = "2", # line width
      col = "blue") # line color
# here we add another lift curve based on Model 2c_2
lines(x = c(0, percent_Cust), # x coordinates
      y = c(0, lift_charts$logit_model_2c_2$Cumulative_Lift), # y coordinates
      type = "l", # type of plot, l is for lines
      lwd = "2", # line width
      col = "darkorange") # line color
# here we add another lift curve based on Model 2c_3
lines(x = c(0, percent_Cust), # x coordinates
      y = c(0, lift_charts$logit_model_2c_3$Cumulative_Lift), # y coordinates
      type = "l", # type of plot, l is for lines
      lwd = "2", # line width
      col = "purple") # line color
# add the legend
legend(x = "topleft", # position of the legend (topleft corner)
       legend = c("Model 2a", "Model 2c_1", "Model 2c_2", "Model 2c_3", "Random Selection"), 
       col = c("red", "blue", "darkorange", "purple", "darkgreen"), # line colors
       lwd = 2, # line width
       cex = 0.9) # legend text size



#####################################################
#############          ROC Curve        #############
#####################################################

# Store the models in a list, including the model name
models <- list(logit_model_2a = logit_model_2a, 
               logit_model_2c_1 = logit_model_2c_1, 
               logit_model_2c_2 = logit_model_2c_2, 
               logit_model_2c_3 = logit_model_2c_3)

# Initialize an empty list to store the roc score
res_roc_score <- list()

# Iterate over the list of models
for (model_name in names(models)) {
  # Get the current model from the list
  current_model <- models[[model_name]]
  
  # Estimate the predicted probabilities on new (unseen) data
  testdata$pred_prob <- predict(current_model, 
                                newdata = testdata, 
                                type = "response")
  
  # Select a threshold value of 0.07 
  # If predicted probability is larger than 0.07, then churnD = 1
  # If predicted probability is smaller than or equal to 0.07, then churnD = 0
  testdata$pred_churnD <- ifelse(testdata$pred_prob > 0.07, 1, 0)
  
  testdata$churnD <- as.numeric(testdata$churnD) - 1
  
  # build a ROC object
  roc_score <- roc(response = testdata$churnD, # the observed choices
                   predictor = testdata$pred_churnD) # the predicted choices

  # Store the roc score in the list
  res_roc_score[[model_name]] <- list(roc_score = roc_score)
} 

# Creating the ROC CURVE for all the models 
plot(res_roc_score$logit_model_2a$roc_score, # ROC object
     col = "red", # color of the curve
     lwd = "2",  # the line width of the curve
     yaxs = "i", # we want to plot from Y = 0
     xaxs = "i", # we want to plot from X = 0
     main = "") 
# adding the main title with the desired size and bold font
title(main = "ROC Curves", 
      cex.main = 1.3, # increase the title size
      font.main = 2)  # bold font 
# here we add another lift curve based on Model 2c_1
lines(res_roc_score$logit_model_2c_1$roc_score, 
      type = "l", lwd = "2", col = "blue") 
# here we add another lift curve based on Model 2c_2
lines(res_roc_score$logit_model_2c_2$roc_score, 
      type = "l", lwd = "2", col = "darkorange") 
# here we add another lift curve based on Model 2c_3
lines(res_roc_score$logit_model_2c_3$roc_score, 
      type = "l", lwd = "2", col = "green") 
# add the legend to the plot 
legend(x = "topleft", # position of the legend (topleft corner)
       legend = c("2a", "2c_1", "2c_2", "2c_3"), 
       col = c("red", "blue", "darkorange", "green"), # line colors
       lwd = 2, 
       cex = 0.7, 
       inset = 0.15)


#########################################################################
#################       Part 3 - CLV ANALYSIS      ######################
#########################################################################


####################################
######### Exploring the data #######
####################################

# Read data
cf_data <- readRDS("~/Documents/Echange M2/COURS/Customer Analytics in a Digital World/Final project/datasets/CF_data.RDS")

# number of columns (variables)
ncol(cf_data)

# number of observations
nrow(cf_data)

# names of columns (variables)
colnames(cf_data)

# Look at the data, prints out the 6th top rows
head(cf_data, n = 6)

# Check summary of dataframe
summary(cf_data)

# Check if there are any NAs in the data
any(is.na(cf_data)) # False 
sum(is.na(cf_data)) # 0 
colSums(is.na(cf_data)) # 0 in all columns
# No NAs (missing values in the data

# Controlling for duplicates 
any(duplicated(cf_data)) # False 
sum(duplicated(cf_data)) # 0 
# No duplicates in the data

# Checking the class for each variables: 
sapply(cf_data, class)
# COMMENT
# We notice that the columns churmD, DM, HHSize holds integer values 
# (such as 0 / 1 etc.) have the datatype <integer>. 
# We convert these columns to factors
# Additionally, we notice that the column Gender holds character values 
# (such as Female / Male) have the datatype <chr>. 
# Hence, we must convert gender to factors. 

# Convert character columns to factors (in this case gender)
cf_data <- cf_data %>% 
  mutate_if(is.character, as.factor)
# Convert churnD and DM to factors:
#cf_data$churnD <- as.factor(cf_data$churnD)
cf_data$DM <- as.factor(cf_data$DM)
# cf_data$HHSize <- as.factor(cf_data$HHSize) this needs to be numerical to calculate average HHSize

###### Should we convert Age, Year, Month to factor variables as well??? ######
# Here I`m Converting Year and Month to factor variables, but I do not change
# Age, so I let Age be converted to numeric variable below: 
cf_data$Year <- as.numeric(cf_data$Year)
cf_data$Month <- as.numeric(cf_data$Month)

# Converting following integer columns
# ID, Age, HHIncome, RelationshipL, Web_Visits, Facebook_Page_Views, Publicity,
# to numerical columns:   
cf_data <- cf_data %>% 
  mutate_if(is.integer, as.numeric)

# Control that we successfully converted the variables to our desirable class 
sapply(cf_data, class)
# Looks good

# Check summary of the modified dataframe
summary(cf_data)


####################################################################
#########################     3 a)    ##############################
####################################################################

# The same binomial logistic regression model used in 2a) 
logit_model_2a <- glm(as.factor(churnD) ~ as.factor(DM) + TV_Adv + 
                                          Facebook_Adv + Publicity, 
                                          family = binomial(link = "logit"),
                                          arima_data)
summary(logit_model_2a) # prints summary of the model

# storing the coefficients from logit_model_2a 
coefficients <- coef(logit_model_2a)
coefficients

# filtering the CF data for February 2014 
filtered_data <- subset(cf_data, Month == 2 & Year == 2014)
print(filtered_data)

# removing columns from the observation February 2014 to ID 10569, 
# that was not used in the model we build in parts 2.a and 2.b 
filtered_data_cleaned <- filtered_data %>% 
  select(-c(ID, Month, Year, Age, Gender, HHIncome, HHSize, 
            RelationshipL, Web_Visits, Facebook_Page_Views))
print(filtered_data_cleaned)


# converting the dummy variable to an numeric variable, 
# so we can multiply this variable further down 
variable_sizes <- filtered_data_cleaned %>% 
  mutate(DM = as.numeric(DM) -1)

# multiplying the coefficients from logit_model_2a with the variable sizes, 
# for the donor 10569 in Februrary 2014:
sum_model = sum(coefficients[1] +
                coefficients[2]*variable_sizes[1] + 
                coefficients[4]*variable_sizes[2] + 
                coefficients[5]*variable_sizes[3] + 
                coefficients[6]*variable_sizes[4])
sum_model # printing the sum of all estimated coefficients and their sizes

# Using the formula presented in class:
pred_prob_churn_compmanually = exp(sum_model)/(1+exp(sum_model))
pred_prob_churn_compmanually
cat("Predicted probability for the customer to churn in February 2014\
based on manually cacluation of model params was", 
100*(pred_prob_churn_compmanually), "%\n")
# Note, that this is below our threshold of 0.07 so we would not 
# assign this customer to churn in February 2014. However, if we were to set the 
# threshold to 0.065, we would then assign this customer to churn this month. 


######### Used for testing
# Filter arima_data on ID = 10569, Year = 2014, Month = 2
filtered_data <- subset(arima_data, ID == 10569)
print(filtered_data)

# Filter arima_data on ID = 10569, Month = 2, and Year = 2014
filtered_data <- arima_data %>%
  filter(ID == 10569 & Month == 2 & Year == 2014)
print(filtered_data) # Display all observations with these filters
######### Used for testing


####################################################################
#########################     3 B)    ##############################
####################################################################

####### 
# COMMENT 
# Not relevant here I believe? We need to use the old model, that we fitted on that whole arima_data
# Then we run it again
# Using same binomial logistic regression model as in 2a) with counterfactual dataset
#logit_model_3a <- glm(churnD ~ DM + TV_Adv + Facebook_Adv + Publicity, 
#                      family = binomial(link = "logit"), 
#                      cf_data)
#######


#######
# Filter arima_data on ID = 10569
filtered_arima_data <- subset(arima_data, ID == 10569)
print(filtered_arima_data) 
# COMMENT 
# we see two observations for ID 10569 here, should these be removed, 
# if so, then in the logit_model_2a where included these in arima_data 
# or in the expression below where we make new predictions in these periods,
# since we have two overlapping observations?
#######

# estimate the predicted probabilities on the new dataset, counterfactual (CF) dataset
cf_data$pred_prob <- predict(logit_model_2a, 
                             newdata = cf_data, 
                             type = "response")
head(cf_data)
range(cf_data$pred_prob)
summary(cf_data$pred_prob)
cf_data

### Additional, is this redundant?:  
# Exploring how many times we predict this customer to default 
# based on our previous selected threshold of 0.07 (we could choose 0.065 as well)
# Select a threshold value of 0.07 
# If predicted probability is larger than 0.07, then churnD = 1
# If predicted probability is smaller than or equal to 0.07, then churnD = 0
cf_data$pred_churnD <- ifelse(cf_data$pred_prob > 0.07, 1, 0)
table(cf_data$pred_churnD) 
prop.table(table(cf_data$pred_churnD))*100 # in %
# COMMENT 
# We that the customer 10569 who decided to stay with the company, is predicted 
# to not stay with the organization 42 times (equivalent to 71.19% of the time) 
# and to churn on its donor 17 times (equilvalent to 28.81%), 
# based on the model we created in parts 2a) and 2b). 
###


### CONTROL (Comparison with out results from 3A)
# Comment: Is this part redundant?
# Comparing the answer we obtained by using the predict() function on the 
# new dataset compared to the result we obtained by calculating this 
# manually, based on the model parameters for logit_model_2a in 3a) 
filtered_res <- subset(cf_data, Month == 2 & Year == 2014)
pred_prob_churn_withfunc <- filtered_res$pred_prob
cat("Predicted probability for the customer to churn in February 2014\
based on the predicted function was", 100*(pred_prob_churn_withfunc), "%\n")

# Computing difference between this predicted probability and the 
# predicted probability we got manually: 
diff_prob_churn = (pred_prob_churn_withfunc - pred_prob_churn_compmanually) 
cat("Difference in the predicted outcomes are", 100*(diff_prob_churn), "%\n")
# Is this difference acceptable or is something wrong? 
# Could the difference be due to that I modified the Facebook param, so 
# where I divided the sum on the proportion of our customer to the whole data?
### CONTROL (Comparison with out results from 3A)



####################################################################
#########################     3 C)    ##############################
####################################################################


## think this can be removed? only relevant if there is multiple donations

# extract inferior about predicted probability
pred_prob_churn <- cf_data$pred_prob
pred_prob_not_churn <- 1- cf_data$pred_prob

# create a data frame with pred_prob_churn and pred_prob_not_churn as columns
df <- data.frame(pred_prob_churn, 
                 pred_prob_not_churn)
# rename columns
colnames(df) <- c("churn_probability", "not_churn_probability")

# print the first few rows of the dataframe
head(df)


##############################################
# we are given the information that the nonprofit has a total of 100.000 donors
# and about 65% of them are active on social media (Facebook), and 
# donor 10569 are one of these. So we divide the total facebook costs 
# with the costs that is assign to this donor:
variable_sizes <- filtered_data_cleaned %>%
  mutate(Facebook_Adv = sum(Facebook_Adv/(100000*0.65)))
variable_sizes
#############################################









































































