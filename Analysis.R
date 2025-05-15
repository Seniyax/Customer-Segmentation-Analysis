#Import Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#Import data set
Data <- online_retail
dim(Data)
head(Data)
unique(Data$Country)

#Remove quantities and prices with negative and null values
Data <- Data %>%
  filter(!is.na(CustomerID),Quantity > 0,UnitPrice > 0)

#Total spend per transaction
Data <- Data %>%
  mutate(TotalSpend = Quantity * UnitPrice)

#Exploratory Data Analysis
#Summery Stats
Summery_stats <- Data %>%
  summarise(
    TotalTransactions = n(),
    UniqueCustomers = n_distinct(CustomerID),
    AvgSpend = mean(TotalSpend,na.rm = TRUE),
    MedianSpend = median(TotalSpend,na.rm = TRUE)
    
  )
print(Summery_stats)

#Distribution of Total Spend
ggplot(Data,aes(x=TotalSpend)) + 
  geom_histogram(bins = 50,fill = "blue",alpha = 0.7) +
  scale_x_log10()+
  labs(title = "Distribution of Total Spend per Transactions",x = "Total Spend(Log Scale)",y = "Count") +
  theme_minimal()

#Total Spend Countries 
TotalSpendCountries <- Data %>%
  group_by(Country) %>%
  summarise(TotalSpend = sum(TotalSpend),Transactions = n()) %>%
  arrange(desc(Transactions)) %>%
  slice_head(n=10)

ggplot
  
  



