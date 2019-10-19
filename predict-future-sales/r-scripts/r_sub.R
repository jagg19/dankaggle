library(tidyverse)
library(data.table)
library(lubridate)

setwd("/Users/swag/Github/dankaggle/predict-future-sales/data")

# LOAD ITEMS AND SHOP DATA
items_translated <- fread("items-translated.csv")
items_categories_translated <- fread("item_categories-translated.csv")
shops_translated <- fread("shops-translated.csv")


# LOAD TRAINING AND TEST DATA
sales_data <- fread("sales_train.csv")
test_csv <- fread("test.csv")


# COMBINE WITH ITEMS FOR MORE SALES DATA
sales_data_with_cat <- sales_data %>% left_join(items_translated,by = c("item_id")) %>% select(-V3)




# TOP 10 SELLING CATEGORY - QUANTITY SOLD
top_selling_cat <- sales_data_with_cat %>% 
  group_by(item_category_id) %>% 
  summarize(total_items_sold = sum(item_cnt_day)) %>% 
  ungroup() %>% 
  arrange(desc(total_items_sold))
head(top_selling_cat, 10)


# PLOT
head(top_selling_cat, 10) %>% ggplot(aes(reorder(as.factor(item_category_id),total_items_sold),total_items_sold,fill=as.factor(item_category_id))) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  labs(title = "Top Selling Item Category", x = "Quantiy Sold",y="Category ID")+
  coord_flip()
  



# TOP 10 SELLING CATEGORY - REVENUE
top_selling_revenue_cat <- sales_data_with_cat %>% 
  group_by(item_category_id) %>% 
  summarize(total_revenue = sum(item_cnt_day*item_price)) %>% 
  ungroup() %>% 
  arrange(desc(total_revenue))
head(top_selling_revenue_cat, 10)



# PLOT
head(top_selling_revenue_cat, 10) %>% ggplot(aes(reorder(as.factor(item_category_id),total_revenue),total_revenue,fill=as.factor(item_category_id))) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  labs(title = "Top Selling Item Category", x = "Total Revenue", y="Category ID") +
  coord_flip()




# MOST POPULAR ITEMS - QUANTITY SOLD
popular_items <- sales_data_with_cat %>% 
  group_by(item_id) %>% 
  summarize(total_sold = sum(item_cnt_day)) %>% 
  ungroup() %>% 
  arrange(desc(total_sold))
head(popular_items,10)



# PLOT
head(popular_items,10) %>% ggplot(aes(x = reorder(as.factor(item_id), total_sold), y = total_sold,fill=as.factor(item_id))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") +
  labs(y = 'Quantity Sold', x = 'Item ID', title = 'Top Selling Items') + 
  coord_flip()




# MOST POPULAR ITEMS - REVENUE
popular_items_revenue <- sales_data_with_cat %>% 
  group_by(item_id) %>% 
  summarize(total_revenue = sum(item_cnt_day*item_price)) %>% 
  ungroup() %>% 
  arrange(desc(total_revenue))
head(top_selling_revenue_cat, 10)


# PLOT
head(popular_items_revenue,10) %>% ggplot(aes(x = reorder(as.factor(item_id), total_revenue), y = total_revenue,fill=as.factor(item_id))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") +
  labs(y = 'Total Revenue', x = 'Item ID', title = 'Top Selling Items') + 
  coord_flip()






# MOST POPULAR ITEMS PER SHOP - REVENUE
pop_items_per_shop <- sales_data_with_cat %>%
  group_by(shop_id, item_id) %>%
  summarise(Revenue = sum(item_cnt_day*item_price)) %>%
  filter(Revenue == max(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  ungroup()
head(pop_items_per_shop)



# PLOT
ggplot(data=pop_items_per_shop, aes(x = reorder(as.factor(shop_id), Revenue), y = Revenue, fill = as.factor(item_id))) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title= "Top Selling Item Per Shop", x= "Shop ID", y = "Revenue", fill = "Item ID")






#########
# DATES #
#########



# FORMAT DATES 
sales_data_with_cat$date <- dmy(sales_data_with_cat$date)
sales_data_with_cat$year <- as.factor(year(sales_data_with_cat$date))
sales_data_with_cat$month <- as.factor(month(sales_data_with_cat$date))
sales_data_with_cat$day <- as.factor(day(sales_data_with_cat$date))
sales_data_with_cat$weekday <- as.factor(weekdays(sales_data_with_cat$date))
str(sales_data_with_cat)





# MONTHLY SALES PER SHOP - REVENUE
sales_monthly_shops <- sales_data_with_cat %>% group_by(shop_id, month) %>% summarise(total_revenue =sum(item_cnt_day*item_price)) %>% ungroup() %>% arrange(desc(total_revenue))
sales_monthly_shops %>% distinct(month,.keep_all=TRUE)




# YEARLY SALES PER SHOP - REVENUE
sales_yearly_shops <- sales_data_with_cat %>% group_by(shop_id, year) %>% summarise(total_revenue =sum(item_cnt_day*item_price)) %>% ungroup() %>% arrange(desc(total_revenue))
sales_yearly_shops %>% distinct(year,.keep_all=TRUE)





# DAY OF MONTH SALES
day_month_sales <- sales_data_with_cat %>%
  group_by(month, day) %>%
  summarise(total_sales =  sum(item_price * item_cnt_day))


# PLOT
day_month_sales %>% ggplot(aes(x = day, y = total_sales, group =  month, color =  month)) +
  geom_line() + 
  geom_point() +
  labs(title = "Day of Month Sales", x = "Day", y = "Total Revenue", fill = "Months") 
    
    
    
    
    
# NUMBER OF ITEMS SOLD ON WEEKDAYS
weekdays_items_sold <- sales_data_with_cat %>%
      group_by(weekday) %>%
      summarise(item_sold = sum(item_cnt_day)) %>%
      arrange(desc(item_sold))


# PLOT  
weekdays_items_sold %>% ggplot(aes(x = reorder(weekday, item_sold), y =  item_sold, fill = weekday))+
      geom_bar(stat = "identity") +
      labs(title = "Items Sold On Weekdays", x = "Week Days", y =  "Quantity", fill = "Week Days") +
      geom_label(stat = "identity", position = position_dodge(width = 1), hjust = "right", aes(label = item_sold)) +
      coord_flip()
    
    
    
    
# REVENUE ON WEEKDAYS
weekdays_revenue <- sales_data_with_cat %>%
      group_by(weekday) %>%
      summarise(total_revenue = sum(item_cnt_day * item_price)) %>%
      arrange(desc(total_revenue))

weekdays_revenue$total_revenue <- round(weekdays_revenue$total_revenue, 1)

    
# PLOT    
ggplot(weekdays_revenue, aes(x =reorder(weekday, total_revenue), y =  total_revenue, fill = factor(weekday)))+
      geom_bar(stat = "identity") +
      labs(title = "Revenue Per Weekday", x = "Day of Week", y =  "Total Revenue", fill = "Week Days") +
      geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "right", aes(label = total_revenue)) +
      coord_flip()
      

    
paste(weekdays_revenue$weekday[1],"had the most revenue and quantity sold out of all the days in the week. Total revenue was",weekdays_revenue$total_revenue[1], "with a total of",weekdays_items_sold$item_sold[1],"items sold.")

    


################################
# FIX CREDITED ITEMS / REFUNDS #
################################



## DROPPING CREDITS  
sales_data_refund_dropped <- sales_data_with_cat %>% filter(item_cnt_day > 0)


## REPLACING CREDITS WITH ZERO

sales_data_refund_zero <- sales_data_with_cat


idx <- which(sales_data_refund_zero$item_cnt_day < 0)
sales_data_refund_zero$item_cnt_day[idx] <- 0







##########
# MODELS #
##########

str(sales_data_with_cat)

# MAKE COPY
data <- sales_data_with_cat %>% select(shop_id,item_id,item_cnt_day,item_category_id,year,month,day,weekday)


# MONTHLY
data_monthly <- data %>% group_by(month,shop_id,item_category_id,item_id) %>% summarise(item_cnt_month=sum(item_cnt_day)) %>% ungroup()


# FACTOR CATEGORIES
data_monthly$item_category_id <- factor(data_monthly$item_category_id)
data_monthly$shop_id <- factor(data_monthly$shop_id)
str(data_monthly)


# ADD FEATURE COLUMNS TO TEST DATASET
test_data <- test_csv %>% left_join(items_translated,by = c("item_id")) %>% select(-c("V3","item_name_translated"))
test_data$month <- factor(11)
test_data$item_category_id <- factor(test_data$item_category_id)
test_data$shop_id <- factor(test_data$shop_id)
str(test_data)


# CREATE MULTI-LINEAR MODEL
linear_model <- lm(item_cnt_month ~ month+shop_id+item_category_id+item_id, data = data_monthly)
summary(linear_model)


# PREDICT
result <- predict(linear_model, test_data[,-1])



#CALCULATE RMSE
#res <- sqrt(mean((result - test_csv$Petal.Length) ^ 2))


# SUBMIT
submission <-  data.frame(ID = test_csv$ID,
                         item_cnt_month = result)



write_csv(submission,"submission1_linreg.csv")






library(gbm)
#gbm_model  <- gbm(item_cnt_day ~ shop_id + item_id,
#                  data = sales_data,
#                  shrinkage = 0.01,
#                  distribution = "gaussian",
#                  n.trees = 1000,
#                  interaction.depth = 5, 
#                  bag.fraction = 0.5,
#                  train.fraction = 0.8,
#                  # cv.folds = 5,
#                  n.cores = -1,
#                  verbose = T)



#result2 <- predict(gbm_model,newdata = test_csv[,c("shop_id","item_id")], n.trees = 1000)



#sub2 <- data.frame(ID = test_csv$ID, 
#                  item_cnt_month =  result2)



















