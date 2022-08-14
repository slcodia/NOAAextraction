
library(tidyverse)
library(readr)
library(ggplot2)
library(ggthemes)
library(latex2exp)
library(ggpubr)
library(reshape2)

# DATA PREPARATION
source("C:/Users/Siegfred Codia/Documents/Acads/MS Stat/Thesis/Data/read_NOAA.R")
path0 <- "C:/Users/Siegfred Codia/Documents/Acads/MS Stat/Thesis/Data/NOAA/daily extracted"
legaspi <- read_NOAA(station = 984440,path = path0,start = 1973, end = 2021, verbose = TRUE)
legaspi.load <- legaspi
legaspi <- legaspi.load


View(legaspi)

sum(is.na(legaspi$TEMP))
sum(is.na(legaspi$MIN))
sum(is.na(legaspi$MAX))

sum(is.na(legaspi$PRCP))
sum(is.na(legaspi$VISIB))




    


# Temperature =====================
    
    
    # removing invalid temperatures (based on PAGASA extremes)
    legaspi$MIN  <- replace(legaspi$MIN,  legaspi$MIN  < 13.9, NA)
    legaspi$TEMP  <- replace(legaspi$TEMP,  legaspi$TEMP  < 13.9, NA)
    legaspi$MAX  <- replace(legaspi$TEMP,  legaspi$MAX  > 37.7, NA)
    
    ## imputing mean temperature
    legaspi$TEMP <- imputeTS::na_interpolation(legaspi$TEMP)
    
    ## obtaining the differences
    mean.min <- imputeTS::na_interpolation(legaspi$TEMP - legaspi$MIN)
    max.mean <- imputeTS::na_interpolation(legaspi$MAX - legaspi$TEMP)
    
    ## calculation of max and mean
    legaspi$MIN <- legaspi$TEMP - mean.min
    legaspi$MAX <- legaspi$TEMP + max.mean    
    
    
    View(legaspi)
    
    
# Rainfall ========================
    ## exploring relationships 
    
    ### PRCP ~ VISIB 
    ggplot(legaspi, aes(x = VISIB, y = PRCP))+
        geom_point(shape = 1)+geom_smooth(method = "lm")
    
    ### PRCP ~ SLP
    ggplot(legaspi,aes(x = SLP, y = PRCP))+
        geom_point(shape = 1)+geom_smooth(method = "lm")
    
    ### PRCP ~WDSP
    ggplot(legaspi,aes(x = WDSP, y = PRCP))+
        geom_point(shape = 1)+geom_smooth(method = "lm")
    
    ## model
    subset(legaspi, !is.na(PRCP))%>%nrow()
    
    mod.precip <- lm(PRCP~VISIB:SLP:MONTH, data = subset(legaspi, !is.na(PRCP)))
    summary(mod.precip)
    
    fit <-  predict(mod.precip, data = legaspi)
    length(fit)
    
    nrow(legaspi)
    
    ## linear interpolation
    legaspi$PRCP <- imputeTS::na_interpolation(legaspi$PRCP)


# Dew point, Sea Level Pressure, Visibility, and Windspeed ==============
    legaspi$DEWP  <- imputeTS::na_interpolation(legaspi$DEWP)
    legaspi$SLP   <- imputeTS::na_interpolation(legaspi$SLP)
    legaspi$VISIB <- imputeTS::na_interpolation(legaspi$VISIB)
    legaspi$DEWP  <- imputeTS::na_interpolation(legaspi$DEWP)
    legaspi$WDSP  <- imputeTS::na_interpolation(legaspi$WDSP)

# Solar Radiation Data ====
# Source: SOLCAST

 ## Data extraction ----

## Reading Data
radiance <- read_csv("C:/Users/Siegfred Codia/Documents/Work/UPRI/Downscaling/Dataset/13.133_123.733_Solcast_PT60M.csv")%>%
    group_by(DATE = lubridate::date(PeriodEnd))%>%
    summarise(RAD = mean(Ghi)*0.0864)%>%
    mutate(YEAR = lubridate::year(DATE),
           MONTH = lubridate::month(DATE),
           DAY = lubridate::day(DATE))
## Merging
legaspi <- merge(x = legaspi,
                 y = radiance[,c("DATE", "RAD")],
                 by = 'DATE',
                 all = TRUE)

View(legaspi)
## Radiance Visualizations ----
radiance%>%
    ggplot(aes(x = DATE, y = RAD))+ geom_line()

# Yearly
year.rad <- radiance %>%
    group_by(YEAR)%>%
    summarise(RAD = mean(RAD))
    
ggplot(year.rad,aes(x = YEAR, y = RAD)) +
        geom_line()+theme_bw()+
        ylab(TeX("Average Solar Radiation $\\left(\\frac{MJ}{m^2day}\\right)$"))

Kendall::MannKendall(year.rad$RAD)

# monthly
month.rad <- radiance %>%
    group_by(MONTH)%>%
    summarise(RAD = mean(RAD))%>%
    mutate(date = as.Date(paste("1900",MONTH,"01",sep = "-")))
## line chart
ggplot(month.rad, aes(x = date, y = RAD)) +
    geom_line()+theme_bw()+
    ylab(TeX("Average Solar Radiation $\\left(\\frac{MJ}{m^2day}\\right)$"))+
    scale_x_date(date_labels = "%b")+
    xlab("MONTH")
## boxplot
ggplot(radiance, aes(x = as.factor(MONTH), y = RAD))+
    geom_boxplot()+xlab("MONTH")+ylab("RADIATION")+theme_bw()


# year and month
radiance %>%
    group_by(YEAR, MONTH)%>%
    summarise(RAD = mean(RAD))%>%
    mutate(date = as.Date(paste(YEAR,MONTH,"01",sep = "-")))%>%
    ggplot(aes(x = date, y = RAD))+
        geom_line()+theme_bw()+
        ylab(TeX("Average Solar Radiation $\\left(\\frac{MJ}{m^2day}\\right)$"))+
        scale_x_date(date_labels = "%Y-%b")+
        xlab("YEAR-MONTH")
View(radiance)

View(legaspi)
## Radiance Relationships ----

# MEAN temp vs Radiation
ggplot(legaspi, aes(x = MEAN, y = RAD))+
    geom_point()+geom_smooth(method ="lm", se=F)+theme_bw()+
    ylab(TeX("Daily Solar Radiation $\\left(\\frac{MJ}{m^2day}\\right)$"))+
    xlab(expression(paste("Daily Mean Temperature ",(degree*C))))

# MIN temp vs radiation in December
ggplot(subset(legaspi, MONTH==12), aes(x = MIN, y = RAD))+
    geom_point()+geom_smooth(method ="lm", se=F)+theme_bw()+
    ylab(TeX("Daily Solar Radiation $\\left(\\frac{MJ}{m^2day}\\right)$"))+
    xlab(expression(paste("Daily Temperature ",(degree*C))))

# Temp range vs radiation
legaspi <- legaspi %>% mutate(temprange = MAX - MIN)

    ggplot(legaspi, aes(x = temprange, y = RAD))+
    geom_point()+geom_smooth(method ="lm", se=F)+theme_bw()+
    ylab(TeX("Daily Solar Radiation $\\left(\\frac{MJ}{m^2day}\\right)$"))+
    xlab(expression(paste("Daily Temperature Range",(degree*C))))

## Model Estimation ----

# split data
train <- subset(legaspi, YEAR %in% c(2007:2021))
test <- subset(legaspi, YEAR %in% c(1973:2006))
View(train)

# MODEL 1
View(legaspi)
mod1 <- lm(RAD~temprange+ TEMP + MONTH +
               temprange:TEMP+ TEMP:MONTH, data = train)
summary(mod1)
aov(mod1)

# MODEL 2
# note that radiance is bounded (RAD >=0), so we do a regression with log link
mod2 <- lm(log(RAD)~temprange+ TEMP + MONTH +
               temprange:TEMP+ TEMP:MONTH, 
           data = train)
summary(mod2)

# MODEL 3
# note that radiance is bounded (RAD >=0), so we do a regression with log link
mod3 <- glm(RAD~temprange+ TEMP + MONTH +
               temprange:TEMP+ TEMP:MONTH, 
           data = train, family = gaussian(link = "log"))
summary(mod3)




## Diagnostics ====

# model 1
fit1   <- exp(predict(mod1, train))
resid1 <- fit1-train$RAD

hist(resid1)
range(resid1)

range(train$RAD)
range(fit1)

# model 2
fit2   <- exp(predict(mod2, train))
resid2 <- fit2-train$RAD

hist(resid2)
range(resid2)

range(RAD)
range(fit2)

# model 3
fit3   <- exp(predict(mod3, train))
resid3 <- fit3-train$RAD

hist(resid3)
range(resid3)

range(train$RAD)
range(fit3)

# residual vs fitted plot
ggplot(train, aes(x=fit2, y=resid2, colour = MONTH))+
    geom_point()

# residual histogram
ggplot(train, aes(x=resid2))+
    geom_histogram(aes(y =..density..),
                   #breaks = seq(-50, 50, by = 10), 
                   colour = "black", 
                   fill = "white")+
    stat_function(fun = dnorm, args = list(mean = mean(train$resid), sd = sd(train$resid)))

# residual histogram by month
                   
histogram.month <- tapply(train$resid, train$MONTH, hist)

for (i in 1:12){
    histogram.month[[i]]%>%
        plot(main = paste("Residual Histogram for", month.name[i]), 
             xlab = "Residual")
}

# standard deviation per month
train%>%
    group_by(MONTH)%>%
    summarize(sd = sd(resid))%>%
    ggplot(aes(x=MONTH, y = sd)) + geom_point() + ylim(0,4)

# tests for normality

qqnorm(resid(mod2))
qqline(resid(mod2), col = "steelblue")


## Predictions using the model =======

test$RAD <- exp(predict(mod3, test))
View(test)

range(test$RAD)
range(legaspi$RAD, na.rm = T)


# combining data
legaspi <- rbind(test,train)
View(legaspi)


## Visualization of prediction----
legaspi.month <- legaspi%>%
    group_by(YEAR, MONTH)%>%
    summarise(RAD = mean(RAD))%>%
    mutate(date = as.Date(paste(YEAR,MONTH,"01",sep = "-")))
View(legaspi.month)

ggplot(legaspi2.month, aes(x=date,y=RAD))+
    geom_line()+theme_bw()+
    ylab(TeX("Average Solar Radiation $\\left(\\frac{MJ}{m^2day}\\right)$"))+
    scale_x_date(date_labels = "%Y-%b")+
    xlab("Year-Month")+
    geom_vline(xintercept=as.numeric(legaspi2.month$date[408]),
           linetype=2, colour="red", size =1)


legaspi2$DAY <- lubridate::day(legaspi2$DATE)
View(legaspi2)


# WRITING DATA
Legaspi <- legaspi[,c("YEAR", "MONTH","DAY","MAX","MIN","PRCP", "RAD")]
Legaspi$MONTH <- as.numeric(Legaspi2$MONTH)
Legaspi_stationary <- subset(Legaspi, YEAR %in% c(1991:2020))
library(multiplex)
dat.path <- "C:/Users/Siegfred Codia/Documents/Work/UPRI/Downscaling/LARSWG6/Data"
write.dat(Legaspi, dat.path)
write.dat(Legaspi_stationary, dat.path)

# CHECKING IF DATA IS STATIONARY----
library(tidyverse)
legaspi.year <- legaspi %>% group_by(YEAR)%>%
    summarise(PRCP.total = sum (PRCP),
              MAX.mean = mean(MAX),
              TEMP.mean = mean(TEMP),
              MIN.mean = mean(MIN))

View(legaspi.year)

# precipitation
ggplot(legaspi.year, aes(x = YEAR, y = PRCP.total))+
    geom_point(shape = 1, size = 3) + theme_bw()+
    ylab("Total Annual Precipitation Amount (mm)")+
    geom_smooth(method = "lm", se = F)
Kendall::MannKendall(legaspi.year$PRCP.total)



# cutting the data: starting 1991

legaspi.year.1991 <- subset(legaspi.year, YEAR %in% c(1991:2020))
ggplot(legaspi.year.1991, aes(x = YEAR, y = PRCP.total))+
    geom_point(shape = 1, size = 3) + theme_bw()+
    ylab("Total Annual Precipitation Amount (mm)")
Kendall::MannKendall(legaspi.year.1991$PRCP.total)

ggplot(legaspi.year.1991, aes(x = YEAR, y = TEMP.mean))+
    geom_point(shape = 1, size = 3) + theme_bw()+
    ylab("Total Annual Precipitation Amount (mm)")

Kendall::MannKendall(legaspi.year.1991$TEMP.mean)

