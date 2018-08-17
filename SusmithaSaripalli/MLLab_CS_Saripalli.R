### Machine Learning Lab ###
###  Confidence Squared  ###
###  Susmitha Saripalli  ###

library(dplyr)
library(ggplot2)
library(caret)

o <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/Machine_Learning_Lab/data/orders.csv", stringsAsFactors = FALSE)
r <- read.csv("/Users/susmithasaripalli/Documents/NYCDSA/Machine_Learning_Lab/data/returns.csv", stringsAsFactors = FALSE)
summary(o)
## Part 1 ##

# Problem 1

o$Profit <- gsub(",","",o$Profit, fixed=TRUE)
o$Profit <- as.numeric(gsub("$","",o$Profit, fixed=TRUE))
o$Sales <- gsub(",","",o$Sales, fixed=TRUE)
o$Sales <- as.numeric(gsub("$","",o$Sales, fixed=TRUE))

# Problem 2

o$Order.Date <- as.Date(o$Order.Date, "%m/%d/%y")
o$Ship.Date <- as.Date(o$Ship.Date, "%m/%d/%y")

oMonthly <- o %>%
  group_by(date= strftime(Order.Date,"%y/%m")) %>%
  summarize(sold = sum(Quantity))
o_Monthly <- ggplot(data = oMonthly,aes(date,sold))
o_Monthly + geom_point()
# General trend of increased sales toward the end of the year, around holiday season

o_cat<-o%>%
  group_by(.,date= strftime(Order.Date,"%y/%m"), Category)%>%
  summarise(.,Quantity=sum(Quantity))

ggplot(data = o_cat , aes(x = date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  facet_wrap(~Category)
# Trend is the same for each category but especially relevant in for office supplies

# Problem 3
# 1
or <- merge(o,r,by="Order.ID", all = TRUE)
or[is.na(or)] <- "No"

or_loss_returned <- or %>%
  group_by(year = strftime(Order.Date,"%y")) %>%
  filter(Returned == "Yes") %>%
  summarize(loss = sum(Profit))

or_loss_negative <- or %>% 
  group_by(year = strftime(Order.Date, "%y")) %>%
  filter(Returned == "Yes",Profit <0) %>%
  summarize(loss = sum(Profit))

# 2
# For which customers returned things
or_custret <- or %>%
  group_by(Customer.Name) %>%
  filter(Returned == "Yes") %>%
  summarize(Returns = sum(Quantity))
##summarize(Returns = n())

# How many customers returned 1,2,...etc times
ret_count <- or_custret %>%
  group_by(Returns) %>%
  summarize(Customers = n())


ret_plot <- ggplot(ret_count, aes(x = Returns, y = Customers))
ret_plot + geom_bar(stat = "identity") +
  labs(title = "Number of Customers Returning Repeatedly", x = "Returns", y = "Customers")

# 3
regional_returns <- or %>%
  group_by(Region = Region.x) %>%
  filter(Returned == "Yes") %>%
  summarize(Returns = sum(Quantity))
##summarize(Returns = n())

regional_sales = or %>%
  group_by(Region = Region.x) %>%
  summarize(total = sum(Quantity))
##summarize(total = n())
regional = merge(regional_returns,regional_sales, by= "Region", all = TRUE)
regional_loss <- regional %>%
  mutate(PercentReturned = Returns/total) 
regional_loss <- regional_loss[order(regional_loss$PercentReturned, decreasing = TRUE),]

reg_plot <- ggplot(regional_loss, aes (x = reorder(Region,-PercentReturned), y=PercentReturned))

reg_plot + geom_bar(stat = "identity") +
  labs(title = "Regional returns/sales", x = "Region", y = "Returns %")

# 4
cat_returns <- or %>%
  group_by(Sub.Category) %>%
  filter(Returned == "Yes") %>%
  summarize(Returns = sum(Quantity))
##summarize(Returns = n())
cat_sales = or %>%
  group_by(Sub.Category) %>%
  summarize(total = sum(Quantity))
##summarize(total = n())
subCat <- merge(cat_returns,cat_sales, by = "Sub.Category", all = TRUE)
cat_loss <- subCat %>%
  mutate(PercentReturned = Returns/total)
cat_loss <- cat_loss[order(cat_loss$PercentReturned, decreasing = TRUE),]

cat_plot <- ggplot(cat_loss, aes (x = reorder(Sub.Category,-PercentReturned), y=PercentReturned))

cat_plot + geom_bar(stat = "identity") +
  labs(title = "Categorical returns/sales", x = "SubCategory", y = "Returns %")

## Part 2 ##

# Problem 4
# Part 1 & 2
or <- or %>%
  mutate(Ret_bin = as.numeric(Returned == "Yes"),
         Process.Time = Ship.Date - Order.Date)
# Part 3
ret_before <- or %>%
  group_by(Product.ID) %>%
  summarize(retbefore = sum(Ret_bin))
or = merge(or,ret_before, by = "Product.ID", all = TRUE)

# Problem 5

or_ml <- subset(or, select=-c(Order.ID,Row.ID,Customer.ID, Customer.Name,Product.ID, 
                              Product.Name,Region.y,Returned,Postal.Code, 
                              Region.y, Profit, Country))
or_ml <- or_ml %>% mutate_if(is.character,as.factor)

set.seed(123)
orIndex = createDataPartition(or_ml$Ret_bin, p=0.8, list = F, times = 1)
orTrain = or_ml[orIndex,]
orTest = or_ml[-orIndex,]

summary(or_ml)
# testing to make sure the ratios are correct
or_mltest <- or_ml %>%
  group_by(Ret_bin) %>%
  summarise(n())
or_mltest[2,2]/sum(or_mltest[,2])
#0.04328329
orTraintest <- orTrain %>%
  group_by(Ret_bin) %>%
  summarise(n())
orTraintest[2,2]/sum(orTraintest[,2])
#0.0438682
orTesttest <- orTest %>%
  group_by(Ret_bin) %>%
  summarise(n())
orTesttest[2,2]/sum(orTesttest[,2])
#0.04094365

# Fitting a model
# Logistic regression
#model <- glm(Ret_bin ~.,
 #            family=binomial(link='logit'),
  #           data=orTrain)



