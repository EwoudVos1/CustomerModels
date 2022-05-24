
#### PC Nienke import and setup ####
setwd('C:/Users/nienk/OneDrive/Documenten/Master EORAS/Customer Models')
rm(list = ls())
restaurant <- read.csv("restaurants_IL.csv", sep = ";")

#### PC Ewoud import and setup ####
setwd('~/Dropbox/EORAS/2b/Customer Models/Assignments Customer Models/Assignment 1')
rm(list = ls())
restaurant <- read.csv("restaurants_IL.csv")

#### PC Roel import and setup ####
setwd("~/Documents/Documenten van Roel/EOR + ECO MSc/EORAS Year 1")
rm(list = ls())
restaurant <- read.csv("restaurants_CM.csv")


#### Loading packages #####
library(stargazer)
library(dplyr)
library(ggplot2)
library(xtable)
library(MASS)
library(mlogit)
library(lmtest)
library(tidyverse)
library(sf)
library(mapview)
library(data.table) 

#### Review data ####
#three missing postal codes
summary(restaurant$postal_code)

#remove restaurants with missing postal codes
restaurant1 <- restaurant[!is.na(restaurant$postal_code),]
nrow(restaurant1)

#remove duplicate restaurants 
#remove index column 'X', rest of the row should be the same
restaurant2 <- restaurant1[,-1]
restaurant3 <- restaurant1[!duplicated(restaurant2),]

data <- restaurant3

#### data formatting #####

#make stars to integers 
table(data$stars)
stars_original <- data$stars
data$stars <- ceiling(data$stars)
table(data$stars)
table(stars_original)

data$stars <- ordered(data$stars)

data$state <- as.factor(data$state)
data$categories <- as.factor(data$categories)
data$GoodForKids <- as.logical(data$GoodForKids == "True")

data$RestaurantsPriceRange2 <- ordered(data$RestaurantsPriceRange2, levels = c("1", "2", "3", "4"))
data$RestaurantsReservations <- as.logical(data$RestaurantsReservations == "True")
data$Caters <- as.logical(data$Caters == "True")
data$NoiseLevel <- ordered(data$NoiseLevel, levels = c("quiet", "average", "loud", "very_loud"))

#sequence of variables that need to be turned into binary (true/false)
for (i in c(14, 16, 19:31)) {
  data[,i] <- as.logical(data[,i] == "True")
}

data$Open7days <- as.logical(data$Open7days)
data$OpenSundays <- as.logical(data$OpenSundays)
data$checkedin2016 <- as.logical(data$checkedin2016)
data$checkedin2017 <- as.logical(data$checkedin2017)
data$checkedin2018 <- as.logical(data$checkedin2018)
data$checkedin100 <- as.logical(data$checkedin100)

data$WiFi <- as.logical(data$WiFi == "free")
data$RestaurantsAttire <- as.factor(data$RestaurantsAttire)

data$parking <- as.logical(data$garage + data$street + data$validated + data$lot + data$valet)

#check how many missing values per variable
colSums(is.na(data))

#remove index, category dummies, parking type dummies, review types
data.final <- data[,-c(1,21:31, 34:41)]

head(data.final)


#### area plots ######
# a <- st_as_sf(data.final, coords = c("longitude", "latitude"),  crs = 4326) all the data
data.WI <- st_as_sf(data.final[data.final$state %like% "WI",], coords = c("longitude", "latitude"),  crs = 4326)
mapview(data.WI, map.types = "Stamen.Toner")
data.NC <- st_as_sf(data.final[data.final$state %like% "NC",], coords = c("longitude", "latitude"),  crs = 4326)
mapview(data.NC, map.types = "Stamen.Toner")
data.IL <- st_as_sf(data.final[data.final$state %like% "IL",], coords = c("longitude", "latitude"),  crs = 4326)
mapview(data.IL, map.types = "Stamen.Toner")

#### question 2 ####
model2 <- glm(checkedin100~
                state+
                stars+
                review_count+
                # categories+
                # GoodForKids+
                RestaurantsReservations   +                 
                # RestaurantsTakeOut    +     
                RestaurantsPriceRange2    +
                OutdoorSeating        +     
                WiFi         +             
                RestaurantsAttire    +     
                RestaurantsDelivery   +     
                # BusinessAcceptsCreditCards + 
                parking +
                OpenSundays + Open7days +
                checkedin2016  + checkedin2017 + checkedin2018
              ,family=binomial(link="logit"),data=data.final)

summary(model2)

data.final3 <- data.final
data.final3$stars <- factor(ceiling(restaurant3$stars))
data.final3$stars <- relevel(data.final3$stars, ref = "3")


data.final3$RestaurantsPriceRange2 <- factor(restaurant3$RestaurantsPriceRange2)

model3 <- glm(checkedin100~
                state+
                stars+
                review_count+
                # categories+
                # GoodForKids+
                RestaurantsReservations   +                 
                # RestaurantsTakeOut    +     
                RestaurantsPriceRange2    +
                OutdoorSeating        +     
                WiFi         +             
                RestaurantsAttire    +     
                RestaurantsDelivery   +     
                # BusinessAcceptsCreditCards + 
                parking +
                OpenSundays + Open7days +
                checkedin2016  + checkedin2017 + checkedin2018
              ,family=binomial(link="logit"),data=data.final3)

summary(model3)

model4 <- glm(checkedin100~
                state+
                stars+
                review_count+
                # categories+
                # GoodForKids+
                RestaurantsReservations   +                 
                # RestaurantsTakeOut    +     
                RestaurantsPriceRange2    +
                OutdoorSeating        +     
                WiFi         +             
                RestaurantsAttire    +     
                RestaurantsDelivery   +     
                # BusinessAcceptsCreditCards + 
                parking +
                OpenSundays + Open7days +
                checkedin2016  + checkedin2017 + checkedin2018
              ,family=binomial(link="probit"),data=data.final3)

summary(model4)
#entire model: AIC 1213.5 1212.9
#1. remove categories; AIC decreases to 1209.5 1209
#2. remove stars; not wise. AIC increases to 1232.7 1231. leave in model.
#3. remove acceptance credit cards: AIC decreases to 1207.5 1207.
#4. remove pricerange2: not wise. AIC increases to 1208.8 1208.4 . leave in model.
#5. remove option for take out: AIC decreases to 1206.9 1206.3. Was insignificant so may be wise to exclude it, although it is some sort of service
#6. remove good for kids: AIC decreases to 1205 1204.4. Theory would suggest to include it, reality differs (mention in report)

stargazer(model2, model3, model4)

model2.me <- logitmfx(checkedin100~OpenSundays+
                        state+
                        stars+
                        review_count+
                        #categories+
                        #GoodForKids+ 
                        RestaurantsReservations   +                 
                        #RestaurantsTakeOut    +     
                        RestaurantsPriceRange2    +
                        OutdoorSeating        +     
                        WiFi         +             
                        RestaurantsAttire    +     
                        RestaurantsDelivery   +     
                        #BusinessAcceptsCreditCards + 
                        parking +
                        OpenSundays + Open7days +
                        checkedin2016  + checkedin2017 + checkedin2018,atmean=FALSE,data=data.final)
#almost all variables are insignificant when using MEM 
#stargazer(model2.me$fit) --> same as stargazer(model2). You can't transfer logitmx to table in latex.



ggplot(data = data.final) + 
  geom_bar(mapping = aes(x= categories, fill=categories)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 10))

ggplot(data = data.final) + 
  geom_bar(mapping = aes(x= categories, fill=state)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 10))

##### question 3 #####


data_mlogit <- mlogit.data(data.final,choice='stars',shape='wide')
head(data_mlogit)

# multinomial logit. Inclusion of many variables not supported by software. Not in report.
m1=mlogit(stars~0| #OpenSundays+
            # state+
            review_count+
            #categories+
            #GoodForKids+ 
            # RestaurantsReservations   +                 
            # RestaurantsTakeOut    +     
            # RestaurantsPriceRange2    +
            # OutdoorSeating        +     
            # WiFi         +             
            # #RestaurantsAttire    +     
            # RestaurantsDelivery   +     
            #BusinessAcceptsCreditCards + 
            parking +
            Open7days, # +
            #checkedin2016  + checkedin2017 + checkedin2018 + checkedin100, 
          data=data_mlogit,reflevel = "3")
summary(m1)
#odds
exp(coef(m1))



#Ordered logit
data.final4 <- data.final3
data.final4$stars <- ordered(data.final4$stars, levels = c("1", "2", "3", "4", "5"))
data.final4$NoiseLevel <- factor(data.final4$NoiseLevel, ordered = F)

# data.final$stars <- ordered(data.final$stars)
m1 <- polr(stars ~   
             state+
            review_count+
              categories+
             GoodForKids+
             RestaurantsReservations   +                 
              RestaurantsTakeOut    +     
             RestaurantsPriceRange2    +
             OutdoorSeating        +     
            WiFi         +             
             RestaurantsAttire    +     
             RestaurantsDelivery   +     
              BusinessAcceptsCreditCards + 
             NoiseLevel +
             parking +
            OpenSundays +
            Open7days,
              data = data.final4,
           Hess = TRUE)
summary(m1)
coeftest(m1)

m1excl <- polr(stars ~   
              # state+
             review_count+
             categories+
             GoodForKids+
             RestaurantsReservations   +                 
             RestaurantsTakeOut    +     
             # RestaurantsPriceRange2    +
             OutdoorSeating        +     
             WiFi         +             
             # RestaurantsAttire    +     
             RestaurantsDelivery   +     
             BusinessAcceptsCreditCards + 
             NoiseLevel +
             parking +
             # OpenSundays +
             Open7days,
           data = data.final4,
           Hess = TRUE)
summary(m1excl)
coeftest(m1excl)

#AIC: 3081.213   full model
#zonder categories: AIC: 3092.074 , dus hier categories er wel in 
#zonder state: #AIC: 3078.50 , lager dus state eruit?
#zonder price range: AIC: 3074.778, lager
#ook zonder open sundays: AIC: 3073.365


exp(coef(m1))
# library(brant)
# #error due to singularity
# brant(m1)




##### Question 4 ####
# renew ordering of stars

#low: 1, 1.5, 2, 2.5
#middle: 3, 3.5
#high: 4, 4.5, 5

data.final4$group <- NA


data.final4$group[which(stars_original %in% c(1, 1.5, 2, 2.5))] <- "low"
data.final4$group[which(stars_original %in% c(3, 3.5))] <- "mid"
data.final4$group[which(stars_original %in% c(4, 4.5, 5))] <- "high"

data.final4$group <- ordered(data.final4$group, levels=c("low", "mid", "high"))

#Ordered logit
# data.final$stars <- ordered(data.final$stars)
m2 <- polr(group ~    
             state+
             review_count+
             categories+
             GoodForKids+
             RestaurantsReservations   +                 
             RestaurantsTakeOut    +     
            RestaurantsPriceRange2    +
             OutdoorSeating        +     
            WiFi         +             
            RestaurantsAttire    +     
             RestaurantsDelivery   +     
             BusinessAcceptsCreditCards + 
             NoiseLevel +
             parking +
             OpenSundays +
             Open7days,
           data = data.final4,
           Hess = TRUE)
summary(m2)
coeftest(m2)

m2excl <- polr(group ~    
             # state+
             review_count+
             categories+
             GoodForKids+
             RestaurantsReservations   +
             RestaurantsTakeOut    +
             RestaurantsPriceRange2    +
             OutdoorSeating        +     
             WiFi         +             
             # RestaurantsAttire    +
             RestaurantsDelivery   +     
             BusinessAcceptsCreditCards + 
             NoiseLevel +
             parking +
             # OpenSundays +
             Open7days,
           data = data.final4,
           Hess = TRUE)
summary(m2excl)

m2excl1 <- polr(group ~    
                 # state+
                 review_count+
                 categories+
                 GoodForKids+
                 RestaurantsReservations   +
                 RestaurantsTakeOut    +
                 # RestaurantsPriceRange2    +
                 OutdoorSeating        +     
                 WiFi         +             
                 # RestaurantsAttire    +
                 RestaurantsDelivery   +     
                 BusinessAcceptsCreditCards + 
                 NoiseLevel +
                 parking +
                 # OpenSundays +
                 Open7days,
               data = data.final4,
               Hess = TRUE)
summary(m2excl1)
#AIC gaat nu wel omlaag als price range ingevoegd wordt, en restaurant attire eruit

stargazer(m1, m1excl, m2, m2excl, m2excl1)
m1$aic


# exp(coef(m2))
library(brant)
#error due to singularity
brant(m2)
table(data.final$group)
table(data.final$stars)
