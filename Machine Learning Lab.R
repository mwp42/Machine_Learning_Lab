## Part I: Preprocessing and EDA
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

orders <- read.csv(file = "./Orders.csv", stringsAsFactors =FALSE)
class(orders)
head(orders) 
dim(orders) #51290 rows by 24 columns
colnames(orders)

#Basic EDA
summary(orders)
sapply(orders, sd)
cor(orders)  #Error: 'x' must be numeric


### Problem 1: Dataset Import & Cleaning
class(orders$Profit)  ##factor and has $ dollar sign
class(orders$Sales)  ##factor and has $ dollar sign
orders$Profit <- gsub(",","",orders$Profit, fixed=TRUE)    #gets rid of commas
orders$Profit <- as.numeric(gsub("$","",orders$Profit, fixed=TRUE)) #gets rid of dollar sign and converts to numeric
orders$Sales <- gsub(",","",orders$Sales, fixed=TRUE)
orders$Sales <- as.numeric(gsub("$","",orders$Sales, fixed=TRUE))

#changes to numeric and gets rid of "," commas and $" dollar sign. 

orders$Order.Date
class(orders$Order.Date)
orders$Order.Date<-as.Date(orders$Order.Date, format = "%m/%d/%y") #lower case y
orders$Order.Date
### Problem 2: Inventory Management
#Part 2.1
#plot by time
orders_q<-orders%>%
  group_by(.,Order.Date)%>%
  summarise(.,Quantity=sum(Quantity))

ggplot(data = orders_q , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  ggtitle("Relationship between Order.Date and Quantity")

#plot by month
orders_q_monthly <- orders %>%
  group_by(.,Order.Date= strftime(Order.Date,"%y/%m")) %>%
  summarize(.,Quantity = sum(Quantity))

ggplot(data = orders_q_monthly , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  ggtitle("Relationship between Order.Date and Quantity")


#Part 2.2
#plot by time and facet for category
orders_c<-orders%>%
  group_by(.,Order.Date, Category)%>%
  summarise(.,Quantity=sum(Quantity))

ggplot(data = orders_c , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  facet_wrap(~Category)+
  ggtitle("Relationship between Month and Quantity")


#plot by month and facet for category
orders_c_monthly <- orders %>%
  group_by(.,Order.Date= strftime(Order.Date,"%y/%m"), Category) %>%
  summarize(.,Quantity = sum(Quantity))

ggplot(data = orders_c_monthly , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  facet_wrap(~Category)+
  ggtitle("Relationship between Month and Quantity")


### Problem 3: Why did customers make returns?
#3.1
# For which customers returned things
ordersandreturns_custret <- ordersandreturns %>%
  group_by(Customer.Name) %>%
  filter(Returned == "Yes") %>%
  summarize(Returns = n())

#3.2
# How many customers returned 1,2,...etc times
ret_count <- ordersandreturns_custret %>%
  group_by(Returns) %>%
  summarize(Customers = n())

#3.2
#Regions most likely to return orders
#Generate total returns by region
or_region_returns <- ordersandreturns %>%
  group_by(Region.x) %>%
  filter(Returned == "Yes") %>%
  summarize(Returns = n())

#Generate total orders by region
or_region_quantity = ordersandreturns %>%
  group_by(Region.x) %>%
  summarize(total = sum(Quantity))

#Merge both data frames 
r_d_q = merge(or_region_returns,or_region_quantity, by= "Region.x", all = TRUE)

#Divide total returns by total orders to get proportion or likelyhood
r_d_q$proportion = r_d_q$Returns/r_d_q$total

#sort by greatest proportion of likelyhood
r_d_q[order(r_d_q$proportion, decreasing = TRUE),]

#3.3
#Products most likely to be returned
#Generate total returns by sub-category
or_subs <- ordersandreturns %>%
  group_by(Sub.Category) %>%
  filter(Returned == "Yes") %>%
  summarize(Returns = n())

#Generate total orders by sub-category
or_subs_orders = ordersandreturns %>%
  group_by(Sub.Category) %>%
  summarize(total = sum(Quantity))

##Merge both data frames 
rdq = merge(or_subs_orders,or_subs, by= "Sub.Category", all = TRUE)

#Divide total returns by total orders to get proportion or likelyhood
rdq$proportion = rdq$Returns/rdq$total

#sort by greatest proportion of likelyhood
rdq[order(rdq$proportion, decreasing = TRUE),]
