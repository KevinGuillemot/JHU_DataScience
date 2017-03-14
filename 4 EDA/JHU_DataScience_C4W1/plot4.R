# ########################################################################################
# JHU Data Science - Course 4 Week 1 project - Plot 4
# EDA
# 
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()

# ########################################################################################
# Libraries
# ########################################################################################

library(data.table)

###########################################################################################
# Data import and cleaning
###########################################################################################

#Set file location
directory<-"exdata%2Fdata%2Fhousehold_power_consumption"
fileName<-"household_power_consumption.txt"
fileLocation<-paste(directory,"/",fileName,sep="")

#Validate file presence
if (!file.exists(fileLocation)){ 
  stop(paste("The following file must be present in the working directory",fileLocation)) 
}

#Read data file
rawData<-fread(fileLocation,header=TRUE,sep=";")

#Subset Date 1/2/2007 2/2/007
rawData<-rawData[Date=="1/2/2007" | Date=="2/2/2007",]

#Normalize data Class
rawData[,Date:=as.Date(Date,format="%d/%m/%Y")]
rawData[,Time:=strptime(paste(Date,Time,sep=" "),format="%Y-%m-%d %H:%M:%S")]
rawData[,Global_active_power:=as.numeric(Global_active_power)]
rawData[,Global_reactive_power:=as.numeric(Global_reactive_power)]
rawData[,Voltage:=as.numeric(Voltage)]
rawData[,Global_intensity:=as.numeric(Global_intensity)]
rawData[,Sub_metering_1:=as.numeric(Sub_metering_1)]
rawData[,Sub_metering_2:=as.numeric(Sub_metering_2)]

#Check for missing values
DF<-as.data.frame(rawData)
cleanData<-DF[complete.cases(DF),]



###########################################################################################
# EDA
###########################################################################################

#Open Png file
png(
  "plot4.png",
  width     = 480,
  height    = 480,
  units =  "px"
)

#Plot
par(mfrow=c(2,2))
plot(cleanData$Time,cleanData$Global_active_power,type="l",xlab='',ylab="Global Active Power")

plot(cleanData$Time,cleanData$Voltage,type="l",xlab="datetime",ylab="Voltage")

plot(cleanData$Time,cleanData$Sub_metering_1,type="l",xlab='',ylab="Energy sub metering")
lines(cleanData$Time,cleanData$Sub_metering_2,col="red")
lines(cleanData$Time,cleanData$Sub_metering_3,col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1),col=c("black","red","blue"))

plot(cleanData$Time,cleanData$Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power")

#Close file
dev.off()
