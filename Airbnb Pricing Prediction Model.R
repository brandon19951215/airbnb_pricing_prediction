

rm(list=ls())

library(tidyverse)
library(plm)
library(MASS)
library(psych)
library(car)
library(stats)
library(ggplot2)

#read the data
dat0 <- read.csv("listings SP.csv", header = T)
#get a subset of the data
dat1 <- subset(dat0, select = c('id',	'host_since', 'host_response_time',
                                'host_response_rate',	'host_is_superhost',
                                'host_total_listings_count','host_has_profile_pic',	'host_identity_verified',
                                'neighbourhood_group_cleansed',
                                'is_location_exact',	'property_type',	'room_type',	'accommodates',	'bathrooms',	'bedrooms',
                                'beds',	'bed_type', 'price', 'security_deposit',
                                'cleaning_fee',	'guests_included',	'extra_people',	'minimum_nights',	'maximum_nights',
                                'availability_30',	'availability_60',
                                'availability_90',	'availability_365',	'number_of_reviews','last_review',	
                                'review_scores_rating',	'review_scores_accuracy',
                                'review_scores_cleanliness',	'review_scores_checkin',	'review_scores_communication',
                                'review_scores_location',
                                'review_scores_value',	'instant_bookable',	'cancellation_policy',	
                                'require_guest_profile_picture',	
                                'require_guest_phone_verification','reviews_per_month'))
#data cleaning
dat1$price2 <- as.numeric(gsub("[^0-9.]", "", dat1$price))
dat1$security_deposit2 <- as.numeric(gsub("[^0-9.]", "", dat1$security_deposit))
dat1$cleaning_fee2 <- as.numeric(gsub("[^0-9.]", "", dat1$cleaning_fee))
dat1$extra_people2 <- as.numeric(gsub("[^0-9.]", "", dat1$extra_people))
dat1$host_response_rate2 <- as.numeric(gsub("%", "", dat1$host_response_rate))
dat1$DayDiff_Reviews <- as.numeric(as.Date("2019-06-25")-as.Date(dat1$last_review))
dat1$DayDiff_Host <- as.numeric(as.Date("2019-06-25")-as.Date(dat1$host_since))
PriceQ1SP <- summary(dat1$price2)[2]
PriceQ3SP <- summary(dat1$price2)[5]
PriceIQRSP <- PriceQ3SP - PriceQ1SP
min_PriceSP = as.numeric(PriceQ1SP - 1.5*PriceIQRSP)
max_PriceSP = as.numeric(PriceQ3SP + 1.5*PriceIQRSP)
dat1 <- dat1 %>% filter(price2 < max_PriceSP)
dat2 <- dat1 %>% drop_na()
#obtain cleaned data set
dat <- subset(dat2, select = c('id', 'DayDiff_Host', 'host_response_time',
                               'host_response_rate2',	'host_is_superhost',
                               'host_total_listings_count',	'host_identity_verified',
                               'neighbourhood_group_cleansed',
                               'is_location_exact',	'property_type',	'room_type', 'accommodates', 'bathrooms', 'bedrooms',
                               'beds', 'bed_type',	'price2', 'security_deposit2',
                               'cleaning_fee2',	'guests_included',	'extra_people2', 'minimum_nights', 'maximum_nights',
                               'availability_30',	'availability_60',
                               'availability_90',	'availability_365',	'number_of_reviews','DayDiff_Reviews',	
                               'review_scores_rating', 'review_scores_accuracy',
                               'review_scores_cleanliness',	'review_scores_checkin',	'review_scores_communication',
                               'review_scores_location',
                               'review_scores_value',	'instant_bookable',	'cancellation_policy',	
                               'require_guest_profile_picture', 
                               'require_guest_phone_verification','reviews_per_month'))
dat$Tol_Price <- dat$price2 + dat$cleaning_fee2

#Final cleaned data set for Singapore, and output as csv
dat
write.csv(x = dat,file = "cleaned data SP.csv")

#read the data
data0 <- read.csv("listings NY.csv", header = T)
#get a subset of the data
data1 <- subset(data0, select = c('id',	'host_since', 'host_response_time',
                                  'host_response_rate',	'host_is_superhost',
                                  'host_total_listings_count','host_has_profile_pic',	'host_identity_verified',
                                  'neighbourhood_group_cleansed',
                                  'is_location_exact',	'property_type',	'room_type',	'accommodates',	'bathrooms',	'bedrooms',
                                  'beds',	'bed_type', 'price', 'security_deposit',
                                  'cleaning_fee',	'guests_included',	'extra_people',	'minimum_nights',	'maximum_nights', 'availability_30',	'availability_60',
                                  'availability_90',	'availability_365',	'number_of_reviews','last_review',	
                                  'review_scores_rating',	'review_scores_accuracy',
                                  'review_scores_cleanliness',	'review_scores_checkin',	'review_scores_communication',
                                  'review_scores_location',
                                  'review_scores_value',	'instant_bookable',	'cancellation_policy',	
                                  'require_guest_profile_picture',	
                                  'require_guest_phone_verification','reviews_per_month'))
#data cleaning
data1$price2 <- as.numeric(gsub("[^0-9.]", "", data1$price))
data1$security_deposit2 <- as.numeric(gsub("[^0-9.]", "", data1$security_deposit))
data1$cleaning_fee2 <- as.numeric(gsub("[^0-9.]", "", data1$cleaning_fee))
data1$extra_people2 <- as.numeric(gsub("[^0-9.]", "", data1$extra_people))
data1$host_response_rate2 <- as.numeric(gsub("%", "", data1$host_response_rate))
data1$DayDiff_Reviews <- as.numeric(as.Date("2019-07-09")-as.Date(data1$last_review))
data1$DayDiff_Host <- as.numeric(as.Date("2019-07-09")-as.Date(data1$host_since))
PriceQ1NY <- summary(data1$price2)[2]
PriceQ3NY <- summary(data1$price2)[5]
PriceIQRNY <- PriceQ3NY - PriceQ1NY
min_PriceNY = as.numeric(PriceQ1NY - 1.5*PriceIQRNY)
max_PriceNY = as.numeric(PriceQ3NY + 1.5*PriceIQRNY)
data1 <- data1 %>% filter(price2 < max_PriceNY)
data2 <- data1 %>% drop_na()
#obtain cleaned data set
data <- subset(data2, select = c('id', 'DayDiff_Host', 'host_response_time',
                                 'host_response_rate2',	'host_is_superhost',
                                 'host_total_listings_count',	'host_identity_verified',
                                 'neighbourhood_group_cleansed',
                                 'is_location_exact',	'property_type',	'room_type',	'accommodates',	'bathrooms',	'bedrooms',
                                 'beds',	'bed_type',	'price2', 'security_deposit2',
                                 'cleaning_fee2',	'guests_included',	'extra_people2',	'minimum_nights',	'maximum_nights',
                                 'availability_30',	'availability_60',
                                 'availability_90',	'availability_365',	'number_of_reviews','DayDiff_Reviews',	
                                 'review_scores_rating',	'review_scores_accuracy',
                                 'review_scores_cleanliness',	'review_scores_checkin',	'review_scores_communication',
                                 'review_scores_location',
                                 'review_scores_value',	'instant_bookable',	'cancellation_policy',	
                                 'require_guest_profile_picture',	
                                 'require_guest_phone_verification','reviews_per_month'))
data$Tol_Price <- data$price2 + data$cleaning_fee2

#Final cleaned data set for Singapore, and output as New York City
data
write.csv(x = data,file = "cleaned data NY.csv")



#Data Visualization For Singapore

#superhost and host and review ratings
singnames <- c(
  'f'="Not Superhost",
  't'="Superhost")
ggplot(dat, aes(x=review_scores_rating, y= Tol_Price, color= host_is_superhost)) + 
  geom_point() + 
  facet_wrap(~host_is_superhost, labeller= as_labeller(singnames)) + 
  labs( x = 'Review Scores Rating', y = 'Price') + 
  labs(title = "Review Scores and Total Price Singapore", subtitle = "Partitioned by Host Type") + 
  theme(legend.position = "none")

ggplot(dat, aes(x = neighbourhood_group_cleansed, y = Tol_Price, fill = neighbourhood_group_cleansed)) +
  geom_boxplot(show.legend = T) +
  theme_minimal() +
  labs(title = 'Singapore Price difference in different Neighbourhood', x = 'Neighbourhood', y = 'Price')

ggplot(dat, aes(x = room_type, y = Tol_Price, fill = room_type)) +
  geom_boxplot(show.legend = T) +
  theme_minimal() +
  labs(title = 'Singapore Price difference in different Room Type', x = 'Room Type', y = 'Price')

ggplot(dat, aes(x = bedrooms, y = Tol_Price, color = room_type)) +
  geom_point(show.legend = T) +
  theme_minimal() +
  geom_smooth(aes(group = room_type), method = 'lm', show.legend = FALSE, se = FALSE) +
  labs(title = 'Singapore Price vs Number of Bedrooms', x = 'Number of Bedrooms', y = 'Price')

ggplot(dat, aes(x = property_type, y = Tol_Price, fill = property_type)) +
  geom_boxplot(show.legend = F) +
  theme_minimal() +
  labs(title = 'Singapore Price difference in different Property Type', x = 'Property Type', y = 'Price') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(dat, aes(x = Tol_Price, alpha=.5, position="identity", color = neighbourhood_group_cleansed,fill = neighbourhood_group_cleansed)) +
  geom_histogram(show.legend = T) +
  theme_minimal() +
  labs(title = 'Singapore Price Histogram', x = 'Price')+
  geom_vline(aes(xintercept=mean(Tol_Price, na.rm=T)),color="red", linetype="dashed", size=1)

#Data Visualization For New York City

#shows the majority of bad ratings are people that are not super hosts in Singapore
nynames <- c(
  'f'="Not Superhost",
  't'="Superhost")
ggplot(data, aes(x=review_scores_rating, y= Tol_Price, color= host_is_superhost)) + 
  geom_point() + facet_wrap(~host_is_superhost, labeller= as_labeller(nynames)) + 
  labs( x = 'Review Scores Rating', y = 'Price') + 
  labs(title = "Review Scores and Total Price New York", subtitle = "Partitioned by Host Type") + 
  theme(legend.position = "none")
#the same thing can be seen in new york, so these are correlated

ggplot(data, aes(x = room_type, y = Tol_Price, fill = room_type)) +
  geom_boxplot(show.legend = T) +
  theme_minimal() +
  labs(title = 'New York Price difference in different Room Type', x = 'Room Type', y = 'Price')

ggplot(data, aes(x = neighbourhood_group_cleansed, y = Tol_Price, fill = neighbourhood_group_cleansed)) +
  geom_boxplot(show.legend = T) +
  theme_minimal() +
  labs(title = 'New York Price difference in different Neighbourhood Group', x = 'Neighbourhood Group', y = 'Price')

ggplot(data, aes(x = bedrooms, y = Tol_Price, color = room_type)) +
  geom_point(show.legend = T) +
  theme_minimal() +
  geom_smooth(aes(group = room_type), method = 'lm', show.legend = FALSE, se = FALSE) +
  labs(title = 'New York Price vs Number of Bedrooms', x = 'Number of Bedrooms', y = 'Price')

ggplot(data, aes(x = property_type, y = Tol_Price, fill = property_type)) +
  geom_boxplot(show.legend = F) +
  theme_minimal() +
  labs(title = 'New York Price difference in different Property Type', x = 'Property Type', y = 'Price') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(data, aes(x = Tol_Price, alpha=.5, position="identity", color = neighbourhood_group_cleansed, fill = neighbourhood_group_cleansed)) +
  geom_histogram(show.legend = T) +
  theme_minimal() +
  labs(title = 'New York Price Histogram', x = 'Price')+
  geom_vline(aes(xintercept=mean(Tol_Price, na.rm=T)),color="red", linetype="dashed", size=1)

#Conduct Chi-Squared Test to Check Difference between Superhost and Response Time for Both Singapore and NYC
table1 <- xtabs(~host_is_superhost + host_response_time, data=dat)
table1
prop.table(table1)
margin.table(table1,1)
chisq.test(table1)
chisq.test(table1)$expected

table2 <- xtabs(~host_is_superhost + host_response_time, data=data)
table2
prop.table(table2)
margin.table(table2,1)
chisq.test(table2)
chisq.test(table2)$expected

#stepwise regression for Singapore
step(lm(dat$Tol_Price ~ 1), dat$Tol_Price ~ dat$DayDiff_Host + dat$host_response_time + dat$host_response_rate2 +
       dat$host_is_superhost + dat$host_total_listings_count + dat$host_identity_verified +
       dat$DayDiff_Host + dat$host_response_time + dat$host_response_rate2 +
       dat$neighbourhood_group_cleansed + dat$is_location_exact + dat$property_type +
       dat$room_type + dat$accommodates + dat$bathrooms +
       dat$bedrooms + dat$beds + dat$bed_type +     
       dat$security_deposit2 + dat$guests_included +    
       dat$extra_people2 + dat$minimum_nights + dat$maximum_nights +    
       dat$availability_30 + dat$availability_60 + dat$availability_90 +   
       dat$availability_365 + dat$number_of_reviews + dat$dayDiff_Reviews +
       dat$review_scores_rating + dat$review_scores_accuracy + dat$review_scores_cleanliness +
       dat$review_scores_checkin + dat$review_scores_communication + dat$review_scores_location +
       dat$review_scores_value + dat$instant_bookable + dat$cancellation_policy +
       dat$require_guest_profile_picture + dat$require_guest_phone_verification + dat$reviews_per_month
     ,
     trace = 1, direction = "both")

fit1 <- lm(dat$Tol_Price ~ dat$room_type + dat$bedrooms + dat$extra_people2 + 
             dat$accommodates + dat$security_deposit2 + dat$host_is_superhost + 
             dat$property_type + dat$guests_included + dat$host_total_listings_count + 
             dat$review_scores_location + dat$number_of_reviews + dat$neighbourhood_group_cleansed + 
             dat$DayDiff_Reviews + dat$cancellation_policy + dat$bathrooms + 
             dat$is_location_exact + dat$review_scores_cleanliness + dat$review_scores_value + 
             dat$review_scores_rating + dat$availability_30 + dat$require_guest_profile_picture + 
             dat$require_guest_phone_verification + dat$instant_bookable + 
             dat$host_response_time + dat$maximum_nights + dat$reviews_per_month)
summary(fit1)
extractAIC(fit1)

#stepwise regression for NYC
step(lm(data$Tol_Price ~ 1), data$Tol_Price ~ data$DayDiff_Host + data$host_response_time + data$host_response_rate2 +
       data$host_is_superhost + data$host_total_listings_count + data$host_identity_verified +
       data$DayDiff_Host + data$host_response_time + data$host_response_rate2 +
       data$neighbourhood_group_cleansed + data$is_location_exact + data$property_type +
       data$room_type + data$accommodates + data$bathrooms +
       data$bedrooms + data$beds + data$bed_type +     
       data$security_deposit2 + data$guests_included +    
       data$extra_people2 + data$minimum_nights + data$maximum_nights +    
       data$availability_30 + data$availability_60 + data$availability_90 +   
       data$availability_365 + data$number_of_reviews + data$DayDiff_Reviews +
       data$review_scores_rating + data$review_scores_accuracy + data$review_scores_cleanliness +
       data$review_scores_checkin + data$review_scores_communication + data$review_scores_location +
       data$review_scores_value + data$instant_bookable + data$cancellation_policy +
       data$require_guest_profile_picture + data$require_guest_phone_verification + data$reviews_per_mont
     ,
     trace = 1, direction = "both")

fit2 <- lm(data$Tol_Price ~ data$room_type + data$neighbourhood_group_cleansed + 
             data$bedrooms + data$security_deposit2 + data$property_type + 
             data$guests_included + data$availability_60 + data$review_scores_location + 
             data$reviews_per_month + data$beds + data$cancellation_policy + 
             data$host_total_listings_count + data$review_scores_cleanliness + 
             data$review_scores_value + data$bathrooms + data$availability_365 + 
             data$minimum_nights + data$number_of_reviews + data$review_scores_checkin + 
             data$review_scores_rating + data$DayDiff_Host + data$review_scores_communication + 
             data$host_is_superhost + data$instant_bookable + data$review_scores_accuracy + 
             data$availability_90 + data$bed_type + data$is_location_exact + 
             data$availability_30 + data$DayDiff_Reviews + data$require_guest_profile_picture)
summary(fit2)
extractAIC(fit2)


#Final Model for Singapore
fit_SP <- lm(dat$Tol_Price ~ dat$room_type + dat$bedrooms + dat$extra_people2 + 
             dat$host_is_superhost + 
             dat$guests_included + dat$host_total_listings_count + 
             dat$number_of_reviews + dat$neighbourhood_group_cleansed + 
             dat$DayDiff_Reviews +
             dat$review_scores_rating + 
             dat$host_response_time)
summary(fit_SP)
extractAIC(fit_SP)

#Final Model for NYC
fit_NYC <- lm(data$Tol_Price ~ data$room_type + data$neighbourhood_group_cleansed + 
             data$bedrooms + data$property_type + 
             data$guests_included + 
             data$cancellation_policy + 
             data$host_total_listings_count + data$number_of_reviews + 
             data$review_scores_rating + 
             data$host_is_superhost + data$host_response_time + data$extra_people2)
summary(fit_NYC)
extractAIC(fit_NYC)

#Use VIF Values to Check Multicollinearity
vif(fit_SP)

vif(fit_NYC)

#Use Residuals Plots to Check If Our Models Meet the Assumptions
par(mfrow = c(2,2))
plot(fit_SP, main = 'Final Model for Singapore')
plot(fit_NYC, main = 'Final Model for New York City')
dev.off()

#Make Sure the Residuals Follow Normal Distribution
shapiro.test(fit_SP$residuals)
ks.test(fit_SP$residuals,"pnorm")
shapiro.test(fit_NYC$residuals)
ks.test(fit_NYC$residuals,"pnorm")