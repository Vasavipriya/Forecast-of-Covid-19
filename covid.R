library(readxl)
library(leaflet)
library(tidyverse)
library(ggplot)
library(dplyr)
data_ind=read_excel(file.choose())

#Analysing 
data_ind<-data_ind%>%select(-1)
data_in<-data.frame(data_ind)
class(data_in)
str(data_in)
data_in[ ,"total.cases"] = data_in[ ,2] + data_in[ ,3]
totalcases<-data_in[ ,"total.cases"]
sum(totalcases)

#number of active cases
data_in[ ,"Total active cases"]=data_in[ ,"total.cases"]-(data_in[ ,4]+data_in[ ,5])
total.activecases<-data_in[ ,"Total active cases"]
sum(total.activecases)
ggplot(data_in,aes(x=Name.of.State...UT,y=total.cases),group=Name.of.State...UT)+geom_point()+ylim(0,50)

#covid clear data
df = read.csv(file.choose()) 
df<-df%>%rename("Country" = "Country.Region")
df<-data.frame(df)
str(df)

head(df)
df2<-df %>% 
  group_by(Date,Country,Province.State) %>% 
  summarise(confirmed=sum(Confirmed, na.rm = TRUE),deaths=sum(Deaths),recovered=sum(Recovered))

df2<-df2%>%group_by(Date)
df_india<-subset(df2,Country == 'India')

#world wide
df_all<-df%>%group_by(Date)%>%summarise_if(is.numeric,sum)

confirmed=df%>%group_by(Date)%>%summarise(confirmed=sum(Confirmed))
deaths=df%>%group_by(Date)%>%summarise(deaths=sum(Deaths))
recovered=df%>%group_by(Date)%>%summarise(recovered=sum(Recovered))

confirmed<-confirmed%>%rename("ds"="Date","y"="confirmed")
confirmed$ds <- as.Date(as.character(confirmed$ds), "%m/%d/%y")
confirmed$y=log(confirmed$y)
format(confirmed$ds, format="%y-%m-%d")
confirmed

deaths<-deaths%>%rename("ds"="Date","y"="deaths")
deaths$ds <- as.Date(as.character(deaths$ds), "%m/%d/%y")
format(deaths$ds, format="%y-%m-%d")
deaths

library(installr)
install.RTools()

#install.Rtools(choose_version = TRUE, check = FALSE, GUI = TRUE,page_with_download_url = "https://cran.r-project.org/bin/windows/Rtools/")

#Forecasting Future

library(prophet)

#FORECAST CONFIRMED

m<-prophet(confirmed,interval.width=0.95,daily.seasonality = "TRUE")

future<-make_future_dataframe(m,periods = 30)
tail(future)

forecast<-predict(m,future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)
prophet_plot_components(m, forecast)


#Forecast Deaths

s<-prophet(deaths,interval.width=0.95,yearly.seasonality = "FALSE",
  weekly.seasonality = "FALSE",daily.seasonality = "TRUE")

future<-make_future_dataframe(s,periods = 30)
tail(future)

forecast<-predict(s,future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(s, forecast)
prophet_plot_components(s, forecast)








