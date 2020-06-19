library(dplyr)
data <- read.csv('/Users/chenlyn/Desktop/Order Brushing/order_brush_order.csv')
data <- data[order(data$event_time),]
data$event_time <- as.POSIXct(as.character(data$event_time), "%Y-%m-%d %H:%M:%s")
shop_order <- data %>% group_by(shopid)%>%count()
no_brushing_shop <- shop_order[which(shop_order[,'n'] < 3),]
brushing_shop_data <- subset(data, !(data$shopid %in% no_brushing_shop$shopid))
brushing_shop_data <- brushing_shop_data %>% arrange(shopid, userid, event_time)

shop_orders_uniuser <- brushing_shop_data %>% group_by(shopid) %>% 
  summarise(orders = n(), unique_user = n_distinct(userid), total_centrate_rate = orders / unique_user ) 
no_brushing_shop_v2 <- shop_orders_uniuser[which(shop_orders_uniuser$total_centrate_rate ==1), ]
brushing_shop_data <- subset(brushing_shop_data, !(brushing_shop_data$shopid %in% no_brushing_shop_v2$shopid))

#not_spam_user <- brushing_shop_data %>% group_by(shopid, userid) %>% count()  %>% filter(n < 3)
#brushing_shop_data <- subset(brushing_shop_data, !(brushing_shop_data$userid %in% not_spam_user$userid))

brushing_shop_data <- brushing_shop_data %>% group_by(shopid, userid) %>% 
  mutate(next_2_event_time = lead(event_time, 2), time_diff = next_2_event_time - event_time)
spam_user <- brushing_shop_data[which(brushing_shop_data$time_diff <= 3600),]

no_brushing_shop_v3 <- subset(brushing_shop_data, !(brushing_shop_data$shopid %in% spam_user$shopid))
no_brushing_shop_v3 <- as.data.frame(unique(no_brushing_shop_v3$shopid))
colnames(no_brushing_shop_v3) <- 'shopid'


brushing_shop <- spam_user %>% group_by(shopid) %>%
  summarise(userid = paste(unique(userid), collapse = "&"))


no_brushing_shop_total <- rbind(list(no_brushing_shop$shopid, no_brushing_shop_v2$shopid, no_brushing_shop_v3$shopid))
no_brushing_shop_total <- as.data.frame(unlist(no_brushing_shop_total))
no_brushing_shop_total$userid <- 0
colnames(no_brushing_shop_total) <- c('shopid', 'userid')

sumbmission <- rbind(brushing_shop, no_brushing_shop_total)
write.csv(temp, '/Users/chenlyn/Desktop/Order Brushing/submission.csv', row.names = FALSE)
