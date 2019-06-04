
# source("r/ver1/train_models.R")

glm_pred <- readRDS("data/glm_model.RDS")
gam_pred <- readRDS("data/gam_model.RDS")
rf_pred <- readRDS("data/rf_model.RDS")
rf_pred2 <- readRDS("data/rf_model2.RDS")
ctree_pred <- readRDS("data/ctree_model.RDS")
# crf_pred <- readRDS("data/crf_model.RDS")
# svmfit <- readRDS("data/svm_model.RDS")

library(tidyverse)
library(modelr)
library(mgcv)
library(RODER)
library(randomForest)
### need to work a bit to get tomorrows date but use todays or yesterdays data to predict tomorrow.
### models are trained on D+1 data


get_tomorrow <- function(){
  output <- data.frame(hour = seq(0,23,1)) %>% 
    mutate(datetime = as.POSIXct(paste0(Sys.Date()+1," ",hour,":00"),tz = "UTC")) %>% 
    select(-hour)
  return(output)
}

get_tomorrow()[1,1]


# temp_data <- readxl::read_excel("data/temperature/BHT_temp_apr.xlsx")
temp_data <- read_csv2("data/temperature/BHT_temp_apr.csv") %>% 
  mutate(Timestamp = as.POSIXct(Timestamp, format ="%d.%m.%Y %H:%M"))

start_date <- as.POSIXct(x = paste(Sys.Date()-lubridate::days(50),"00:00:00"), format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct(x = paste(Sys.Date()+lubridate::days(1),"00:00:00"), format = "%Y-%m-%d %H:%M:%S")

pred_file <- get_system_plan(query_start = start_date, query_end = end_date) %>% 
  left_join(get_NPS_price(start_date,end_date)[,c(1,3)], by = "datetime") %>% 
  select(1,2,3,4,5,9,7)


colnames(pred_file) = gsub("real","plan",colnames(pred_file))

pred_data <- pred_file %>% 
  mutate(year = lubridate::year(datetime),
         month = lubridate::month(datetime),
         weekday = lubridate::wday(datetime),
         week = lubridate::week(datetime),
         day = lubridate::day(datetime),
         hour = lubridate::hour(datetime)) %>% 
  group_by(month) %>% 
  mutate(monthly_max = max(plan.losses, na.rm =  TRUE)) %>% 
  group_by(month, week) %>% 
  mutate(weekly_mean = mean(plan.losses,na.rm = TRUE),
         weekly_max = max(plan.losses,na.rm = TRUE),
         weekly_min = min(plan.losses, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(datetime = datetime + lubridate::days(3),
         month = lubridate::month(datetime),
         weekday = lubridate::wday(datetime),
         day = lubridate::day(datetime)) %>% 
  mutate(weekend = ifelse(weekday %in% c(1,7),1,0)) %>% 
  # ungroup() %>% 
  left_join(temp_data, by = c("datetime" = "Timestamp")) %>% 
  rename("temp" = Temperatuur) %>% 
  na.omit() %>% 
  add_predictions(glm_pred, var = "glm") %>% 
  add_predictions(gam_pred, var = "gam") %>% 
  add_predictions(rf_pred, var = "rf") %>% 
  add_predictions(rf_pred2, var = "rf_wide") %>% 
  add_predictions(ctree_pred, var = "ctree") %>% 
  # add_predictions(svmfit, var = "svm") %>% 
  group_by(day) %>% 
  mutate(mean_loss = mean(gam,na.rm = TRUE)) %>% 
  mutate(gam_mod = ifelse(gam <= mean_loss,gam*0.90,gam*1.1)) %>% 
  group_by(datetime) %>% 
  mutate(frank = max(glm,gam,gam_mod),
         cap_obv = (glm+gam)/2) %>% 
  ungroup()


### wrangle data to plot easily
plot_pred <- pred_data %>% 
  filter(week >= lubridate::week(Sys.Date())-1) %>%
  select(datetime,gam,gam_mod,cap_obv,ctree,rf,rf_wide,frank,glm) %>% 
  gather(key = "type", value = "value",gam:glm)

### plot the plan.loss vs predictional loss
ggplot(plot_pred)+
  geom_line(aes(x = datetime, y = value, color = type), size = 1)+
  facet_grid(type~.)+
  scale_x_datetime(date_breaks = "1 day")


## see the sum of all days

sum_days <- pred_data %>% 
  group_by(month,day) %>% 
  summarise(sum_glm_loss = sum(glm, na.rm = TRUE),
            sum_gam_loss = sum(gam, na.rm = TRUE),
            sum_gammod_loss = sum(gam_mod, na.rm = TRUE),
            cap_obv_loss = sum(cap_obv,na.rm = TRUE),
            rf_loss = sum(rf,na.rm = TRUE),
            rf_wide_loss = sum(rf_wide,na.rm = TRUE),
            ctree_loss = sum(ctree, na.rm = TRUE))



## to develop a master function to get prediction for given day
# predict_a_day <- function(prediction_date = get_tomorrow()){
#   return(prediction_date)
# }
# 
# predict_a_day()



## make d-1 prediction
output <- get_tomorrow() %>% 
  left_join(pred_data, by = "datetime") %>% 
  mutate(month = lubridate::month(datetime)) %>% 
  add_predictions(glm_pred, var = "glm") %>% 
  add_predictions(gam_pred, var = "gam") %>% 
  group_by(day = lubridate::day(datetime)) %>% 
  filter(day == lubridate::day(Sys.Date()+lubridate::days(1))) %>% 
  mutate(mean_loss = mean(gam)) %>% 
  mutate(gam_mod = ifelse(gam <= 0.98*mean_loss,gam*0.90,gam*1.1)) %>% 
  group_by(datetime) %>% 
  mutate(frank = max(glm,gam,gam_mod)) %>% 
  ungroup()

write_csv2(output,paste0("preds/d-1_pred_",Sys.Date(),".csv"))

# xgb_test<- output %>% 
#   select(plan.system_balance,plan.production,plan.consumption,plan.losses,ee.price,temp,month,week,day,weekday,hour,weekend,monthly_max,weekly_mean,weekly_max,weekly_min)
# 
# model_matrix_test <- model.matrix(xgb_test)
# # # ?model.matrix
# ?xgb.DMatrix
# data_test <- xgb.DMatrix(as.matrix(xgb_test))
# # # 
# predictions$xgb_pred <- predict(xgb_pred,data_test)


day_mw <- output %>% 
  group_by(day) %>% 
  summarise(total_mw_glm = sum(glm),
            total_mw_gam = sum(gam),
            total_mw_gam_2 = sum(gam_mod),
            tot_frank = sum(frank),
            tot_obv = sum(cap_obv),
            tot_rf = sum(rf),
            tot_crf = sum(crf))
  

print(day_mw)
  
output_gather <- output %>% 
  mutate(gam = as.numeric(gam)) %>% 
  select(datetime,gam,glm,crf, rf,rf_wide) %>% 
  gather(key = "type", value = "value",gam:rf_wide)




p <- ggplot(output_gather)+
  geom_line(aes(x = datetime, y = value, color = type)) +
  scale_y_continuous(limits = c(0,max(output_gather$value)+10))

  
plotly::ggplotly(p)



pred_data %>%
  filter(month == 6 & day == 2) %>%
  write_csv2(path = "daily_pred.csv")

# 
# ## make d-0 prediction
# output <- get_tomorrow() %>% 
#   left_join(pred_data, by = "datetime") %>% 
#   mutate(datetime = datetime - lubridate::days(1)) %>% 
#   mutate(month = lubridate::month(datetime)) %>% 
#   add_predictions(glm_pred, var = "glm") %>% 
#   add_predictions(gam_pred, var = "gam") %>% 
#   group_by(day = lubridate::day(datetime)) %>% 
#   filter(day == lubridate::day(Sys.Date())) %>% 
#   mutate(mean_loss = mean(gam)) %>% 
#   mutate(gam_mod = ifelse(gam <= mean_loss,gam*0.90,gam*1.1)) %>% 
#   ungroup()
# 
# d0_day_mw <- output %>% 
#   group_by(day) %>% 
#   summarise(total_mw_glm = sum(glm),
#             total_mw_gam = sum(gam),
#             total_mw_gam_2 = sum(gam_mod))
# 
# 
# print(d0_day_mw)
# 
# output_gather <- output %>% 
#   mutate(gam = as.numeric(gam)) %>% 
#   select(datetime,gam,glm, gam_mod) %>% 
#   gather(key = "type", value = "value",gam:gam_mod)
# 
# 
# 
# 
# p <- ggplot(output_gather)+
#   geom_line(aes(x = datetime, y = value, color = type)) +
#   scale_y_continuous(limits = c(0,max(output_gather$value)+10))
# 
# 
# plotly::ggplotly(p)
# 
