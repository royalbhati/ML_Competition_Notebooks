library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots 

setwd('Bigmart/')


#using fread() function of data.table package to read the datasets.


train = fread("Train_UWu5bXk.csv") 
test = fread("Test_u94Q5KV.csv")
submission = fread("SampleSubmission_TmnO39y.csv")

#Initially we should understand our raw data thoroughly, i.e.,
#we should explore the no. of features/columns and rows, datatype of the features, 
#feature names and so on. It helps in working with the data in the next stages.

#Dimensions of Data
dim(train)
dim(test)

#checking columns
names(train)


#Structure of Data
#In R, we have a pretty handy function called str(). 
#It gives a short summary of all the features present in a dataframe.
#Let’s apply it on train and test data.


str(train)


#Combine Train and Test to save time and effort

test[,Item_Outlet_Sales := NA]
combi = rbind(train, test) # combining train and test datasets
dim(combi)

#EDA


ggplot(train) +geom_histogram(aes(train$Item_Outlet_Sales),binwidth = 100)
  xlab("Item_Outlet_Sales")
  
#since data is right skewed we need to apply some transformations

#Independent Variables (numeric variables)
  
ggplot(combi)+geom_histogram(aes(combi$Item_Weight),binwidth = 0.5)

#no insights from this plot

ggplot(combi)+geom_histogram(aes(combi$Item_Visibility),binwidth = 0.01)
# data is right skewed we need to transform it

ggplot(combi)+geom_histogram(aes(combi$Item_MRP),binwidth = 0.5)

#We can clearly see 4 different distributions for Item_MRP. It is an interesting insight.


ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
  
ggplot(combi)+geom_bar(aes(combi$Item_Fat_Content),fill="coral1")


combi$Item_Fat_Content[combi$Item_Fat_Content=='LF']='Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi)+geom_bar(aes(combi$Item_Fat_Content),fill="coral1")

# other categorical values

# plot for Item_Type
# plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)


ggplot(combi)+geom_bar(aes(combi$Item_Type))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

#In Outlet_Size’s plot, for 4016 observations, Outlet_Size is blank or missing.
#We will check for this in the bivariate analysis to substitute the missing values 
#in the Outlet_Size.


p7=ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count=n()))+
  geom_bar(aes(factor(Outlet_Establishment_Year),Count),stat="identity",fill="coral1")+
  geom_label(aes(factor(Outlet_Establishment_Year),Count,label=Count),vjust=0.5)+
  theme(axis.text.x = element_text(size=8.5))

p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

# ploting both plots together
plot_grid(p7, p8, ncol = 2)

str(train)


#After looking at every feature individually, let’s now do some bivariate analysis.
# Here we’ll explore the independent variables with respect to the target variable. 
# The objective is to discover hidden relationships between the independent variable 
# and the target variable and use those findings in missing data imputation and feature engineering
# in the next module.  



# We will make use of scatter plots for the continuous or numeric variables 
# and violin plots for the categorical variables.

train = combi[1:nrow(train)] # extracting train data from the combined data

# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + 
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)


# Observations
# 
# Item_Outlet_Sales is spread well across the entire range of the Item_Weight without any obvious pattern.

# In Item_Visibility vs Item_Outlet_Sales, there is a string of points at Item_Visibility = 0.0 
# which seems strange as item visibility cannot be completely zero. We will take note of this issue 
# and deal with it in the later stages.

# In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 4 segments of prices that can
#be used in feature engineering to create a new variable.


# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + 
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + 
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + 
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)


# Observations

# Distribution of Item_Outlet_Sales across the categories of Item_Type is
# not very distinct and same is the case with Item_Fat_Content.
# 
# The distribution for OUT010 and OUT019 categories of Outlet_Identifier are 
# quite similar and very much different from the rest of the categories of Outlet_Identifier.

  
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")


# we can see that distribution of small looks like missing dist so we impute that with small
