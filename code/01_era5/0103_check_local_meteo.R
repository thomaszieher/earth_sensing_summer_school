#####################################################
##                                                 ##
##      ######      #########  ##          ##      ##
##      #######     ########   ##          ##      ##
##      ##    ##    ##         ##          ##      ##
##      ##   ##     ##         ##          ##      ##
##      ######      ######     ##          ##      ##
##      #######     #####      ##          ##      ##
##      ##    ##    ##         ##    ##    ##      ##
##      ##     ##   ##         ##   ####   ##      ##
##      ##     ##   ##         ##  ##  ##  ##      ##
##      ########    ##          ####    ####       ##
##      #######     ##           ##      ##        ##
##                                                 ##
#####################################################
##                                                 ##
##  Author: Thomas Zieher                          ##
##  Institute: Nat Haz, 6.3                        ##
##  Date: 2023-01-23                               ##
##  License: GNU GPL V.3 or later                  ##
##                                                 ##
#####################################################
##  Description: ADD HERE
##  Environment: ADD HERE
#####################################################

setwd("C:/Users/thomas.zieher/Documents/summer_school_2025")


library(imputeTS)
library(data.table)

##repair 2021
# data=read.table("year_2021.txt",sep="\t",skip=1)
# head(data)
# 
# 
# data["date"]=as.Date(sprintf("%0.3i",data$V3),format="%j",origin = "2021-01-01")
# tail(data)
# head(data)
# data$V2=as.numeric(format(data$date,format="%m"))
# tail(as.numeric(format(data$date,format="%m")))
# write.table(data[,1:(ncol(data)-1)],"year_2021_fmt.txt",sep="\t",quote=F,row.names=F)


##raw data
data=read.table("MeteoCR2019-2024_rep.txt",sep="\t",header=T)#,skip=1)

##add date
data["date"]=as.Date(sprintf("%i-%0.3i",as.numeric(data$Year),as.numeric(data$Day)),format="%Y-%j")


##check nas
aggregate(is.na(data$Rain),by=list(format(data$date,format="%m")),FUN="sum")
aggregate(is.na(data$Rain),by=list(format(data$date,format="%Y")),FUN="sum")
aggregate(is.na(data$Rain),by=list(format(data$date,format="%j")),FUN="sum")

barplot(table(data$Month))
barplot(table(data$Year))



##aggregate variables
data_tmean=aggregate(data$T,by=list(data$date),FUN="mean",na.rm=T)
names(data_tmean)=c("date","tmean")
data_tmin=aggregate(data$T,by=list(data$date),FUN="min",na.rm=T)
names(data_tmin)=c("date","tmin")
data_tmax=aggregate(data$T,by=list(data$date),FUN="max",na.rm=T)
names(data_tmax)=c("date","tmax")
data_prec=aggregate(data$Rain,by=list(data$date),FUN="sum",na.rm=T)
names(data_prec)=c("date","prec")
data_globrad=aggregate(data$Glob.Rad.W.mq,by=list(data$date),FUN="mean",na.rm=T)
names(data_globrad)=c("date","globrad")
data_wind=aggregate(data$Wind.vel..m.s,by=list(data$date),FUN="mean",na.rm=T)
names(data_wind)=c("date","wind")
data_rh=aggregate(data$RH,by=list(data$date),FUN="mean",na.rm=T)
names(data_rh)=c("date","rh")

##merge to correct dates
data_daily=data.frame(date=seq(as.Date("2019-01-01"),as.Date("2024-12-31"),by="day"))
data_daily=merge(data_daily,data_tmean,by.x="date",by.y="date")
data_daily=merge(data_daily,data_tmin,by.x="date",by.y="date")
data_daily=merge(data_daily,data_tmax,by.x="date",by.y="date")
data_daily=merge(data_daily,data_prec,by.x="date",by.y="date")
data_daily=merge(data_daily,data_globrad,by.x="date",by.y="date")
data_daily=merge(data_daily,data_wind,by.x="date",by.y="date")
data_daily=merge(data_daily,data_rh,by.x="date",by.y="date")

barplot(table(format(data_daily$date,format="%Y")))
barplot(table(format(data_daily$date,format="%m")))



##fill gaps
data_daily_filled=data.frame(date=data_daily$date)

##kalman filter
data_daily_filled$tmean=imputeTS::na_kalman(data_daily$tmean)
data_daily_filled$tmin=imputeTS::na_kalman(data_daily$tmin)
data_daily_filled$tmax=imputeTS::na_kalman(data_daily$tmax)
data_daily_filled$prec=imputeTS::na_kalman(data_daily$prec)
data_daily_filled$globrad=imputeTS::na_kalman(data_daily$globrad)
data_daily_filled$wind=imputeTS::na_kalman(data_daily$wind)
data_daily_filled$rh=imputeTS::na_kalman(data_daily$rh)

# ##interpolation
# data_daily_filled$tmean=imputeTS::na_interpolation(data_daily$tmean)
# data_daily_filled$tmin=imputeTS::na_interpolation(data_daily$tmin)
# data_daily_filled$tmax=imputeTS::na_interpolation(data_daily$tmax)
# data_daily_filled$prec=imputeTS::na_interpolation(data_daily$prec)
# data_daily_filled$globrad=imputeTS::na_interpolation(data_daily$globrad)
# data_daily_filled$wind=imputeTS::na_interpolation(data_daily$wind)
# data_daily_filled$rh=imputeTS::na_interpolation(data_daily$rh)


any(is.na(data_daily_filled))
barplot(table(format(data_daily_filled$date,format="%Y")))
barplot(table(format(data_daily_filled$date,format="%m")))



##function for saturation vapour pressure (Tetens formula)
es=function(T){
  0.6108*exp((17.27*T)/(T+237.3))
}

##actual vapour pressure from T and RH
ea_from_T_RH=function(T,RH){
  es(T)*(RH/100)
}


##construct meteo data
meteo=data.table(dates=data_daily_filled$date)
meteo$tmean=data_daily_filled$tmean
meteo$tmin=data_daily_filled$tmin
meteo$tmax=data_daily_filled$tmax
meteo$prec=data_daily_filled$prec
meteo$globrad=data_daily_filled$globrad/(1000000/86400)
meteo$vappres=ea_from_T_RH(T=data_daily_filled$tmean,RH=data_daily_filled$rh)
meteo$windspeed=data_daily_filled$wind


barplot(table(format(meteo$date,format="%Y")))
barplot(table(format(meteo$date,format="%m")))


##Walther-Lieth diagram
start_date=as.Date("2019-01-01")
end_date=as.Date("2024-12-31")
##aggregate to monthly values
tmin=setDT(meteo)[year(dates)%in%year(start_date):year(end_date), .(tmin=min(tmin)),by=.(yr=year(dates),mon=month(dates))]
tmax=setDT(meteo)[year(dates)%in%year(start_date):year(end_date), .(tmax=max(tmax)),by=.(yr=year(dates),mon=month(dates))]
psum=setDT(meteo)[year(dates)%in%year(start_date):year(end_date), .(psum=sum(prec)),by=.(yr=year(dates),mon=month(dates))]

##aggregate to mean monthly values
meteo_monthly=aggregate(data.frame(psum=psum$psum,
                                   tmax=tmax$tmax,
                                   tmin=tmin$tmin,
                                   tabsmin=abs(tmin$tmin)),
                        by=list(tmin$mon),FUN="mean")

t(meteo_monthly[,2:5])


par(mfrow=c(1,1))
diagwl(t(meteo_monthly[,2:5]), est="Location",alt="XXX",mlab="en",cols=NULL)


write.table(meteo,"meteo_local.csv",sep=";",row.names=F,quote=F)


