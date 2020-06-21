library(dplyr)
# loading data and a glimpse of data 
data <- read.csv('/Users/chenlyn/Desktop/Order Brushing/order_brush_order.csv')
head(data)
length(unique(data$shopid))
# convert event_time from string to time format
data$event_time <- as.POSIXct(as.character(data$event_time), "%Y-%m-%d %H:%M:%s")

# how many orders on each shop
shop_order <- data %>% group_by(shopid)%>%count()
# If the shop has less than 3 orders, the shop is not suspected.
no_brushing_shop <- shop_order[which(shop_order[,'n'] < 3),]
# Remain shop having more than 2 orders
brushing_shop_data <- subset(data, !(data$shopid %in% no_brushing_shop$shopid))

#sort the data by shopid, userid and event_time
brushing_shop_data <- brushing_shop_data %>% arrange(shopid, userid, event_time)

# To find users who placed multiple orders on the same shop
shop_orders_uniuser <- brushing_shop_data %>% group_by(shopid) %>% 
  summarise(orders = n(), unique_user = n_distinct(userid), total_centrate_rate = orders / unique_user ) 
# If shops' orders are from unique user, the shop is not suspected
no_brushing_shop_v2 <- shop_orders_uniuser[which(shop_orders_uniuser$total_centrate_rate ==1), ]
# Remain shop with users placed the mutiple orders
brushing_shop_data <- subset(brushing_shop_data, !(brushing_shop_data$shopid %in% no_brushing_shop_v2$shopid))

# Calculate the period between the first order and the third order placed by the same user on the same shop
brushing_shop_data <- brushing_shop_data %>% group_by(shopid, userid) %>% 
  mutate(next_2_event_time = lead(event_time, 2), time_diff = next_2_event_time - event_time)
# If the period between the first order and the third order is less than one hour, it means that the user placed >= 3 orders on the same shop within an hour.
# The concentrate rate is >= 3
spam_user <- brushing_shop_data[which(brushing_shop_data$time_diff <= 3600),]

# The shops without any spam user are not suspected.
no_brushing_shop_v3 <- subset(brushing_shop_data, !(brushing_shop_data$shopid %in% spam_user$shopid))
no_brushing_shop_v3 <- as.data.frame(unique(no_brushing_shop_v3$shopid))
colnames(no_brushing_shop_v3) <- 'shopid'

# Make the spam user lists for shops with order brushing
brushing_shop <- spam_user %>% group_by(shopid) %>%
  summarise(userid = paste(unique(userid), collapse = "&"))

# Combind all shops without order brushing, and insert 0 in the 'userid' column.
no_brushing_shop_total <- rbind(list(no_brushing_shop$shopid, no_brushing_shop_v2$shopid, no_brushing_shop_v3$shopid))
no_brushing_shop_total <- as.data.frame(unlist(no_brushing_shop_total))
no_brushing_shop_total$userid <- 0
colnames(no_brushing_shop_total) <- c('shopid', 'userid')

# Combind all shops and export
submission <- rbind(brushing_shop, no_brushing_shop_total)
write.csv(submission, '/Users/chenlyn/Desktop/Order Brushing/after_competion_final.csv', row.names = FALSE)
