# set working directory
setwd("C:/Users/Selcuk Fidan/Desktop/0_Programming_Assignment/Exploratory_DAta_Analysis/Assignment_2")
if(!file.exists("./dataStore")){dir.create("./dataStore")}
# activity monitoring data
get.data.project <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(get.data.project,destfile="./dataStore/exdata-data-NEI_data.zip",method="auto")  

# make sure the site is live, if it is not live stop function terminate the program
check.url <- file(get.data.project,"r")
if (!isOpen(check.url)) {
  stop(paste("There's a problem with the data:",geterrmessage()))
}
# zipfile.data is the variable to keep the *.zip file
zipfile.data = "exdata-data-NEI_data.zip"

# make sure the data in the working directory if not download the zip file into the to zipfile.data and unzip the zipfile.data
if(!file.exists(zipfile.data)) {        
  # download.file(get.data.project,zipfile.data)
  unzip(zipfile="./dataStore/exdata-data-NEI_data.zip",exdir="./dataStore")
} 
path_rf <- file.path("./dataStore" , "exdata-data-NEI_data")
files<-list.files(path_rf, recursive=TRUE)
files

# Read data files
# read national emissions data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
# str(NEI)
# dim(NEI)
# head(NEI)
#read source code classification data
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
# str(SCC)
# dim(SCC)
# head(SCC)
# visualization
number.add.width<-800                             # width length to make the changes faster
number.add.height<-800                             # height length to make the changes faster

require(dplyr)

# png("plot1.png", width=number.add.width, height=number.add.height)
# Group total NEI emissions per year:
total.emissions <- summarise(group_by(NEI, year), Emissions=sum(Emissions))
clrs <- c("red", "green", "blue", "yellow")
x1<-barplot(height=total.emissions$Emissions/1000, names.arg=total.emissions$year,
            xlab="years", ylab=expression('total PM'[2.5]*' emission in kilotons'),ylim=c(0,8000),
            main=expression('Total PM'[2.5]*' emissions at various years in kilotons'),col=clrs)

## Add text at top of bars
text(x = x1, y = round(total.emissions$Emissions/1000,2), label = round(total.emissions$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")


baltcitymary.emissions<-summarise(group_by(filter(NEI, fips == "24510"), year), Emissions=sum(Emissions))
clrs <- c("red", "green", "blue", "yellow")
x2<-barplot(height=baltcitymary.emissions$Emissions/1000, names.arg=baltcitymary.emissions$year,
            xlab="years", ylab=expression('total PM'[2.5]*' emission in kilotons'),ylim=c(0,4),
            main=expression('Total PM'[2.5]*' emissions in Baltimore City-MD in kilotons'),col=clrs)

## Add text at top of bars
text(x = x2, y = round(baltcitymary.emissions$Emissions/1000,2), label = round(baltcitymary.emissions$Emissions/1000,2), pos = 3, cex = 0.8, col = "black")

require(dplyr)
#png("plot3.png", width=number.add.width, height=number.add.height)
# Group total NEI emissions per year:
baltcitymary.emissions.byyear<-summarise(group_by(filter(NEI, fips == "24510"), year,type), Emissions=sum(Emissions))
# clrs <- c("red", "green", "blue", "yellow")
ggplot(baltcitymary.emissions.byyear, aes(x=factor(year), y=Emissions, fill=type,label = round(Emissions,2))) +
  geom_bar(stat="identity") +
  #geom_bar(position = 'dodge')+
  facet_grid(. ~ type) +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emission in tons")) +
  ggtitle(expression("PM"[2.5]*paste(" emissions in Baltimore ",
                                     "City by various source types", sep="")))+
  geom_label(aes(fill = type), colour = "white", fontface = "bold")

combustion.coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
combustion.coal.sources <- SCC[combustion.coal,]

# Find emissions from coal combustion-related sources
emissions.coal.combustion <- NEI[(NEI$SCC %in% combustion.coal.sources$SCC), ]
require(dplyr)
emissions.coal.related <- summarise(group_by(emissions.coal.combustion, year), Emissions=sum(Emissions))
require(ggplot2)
ggplot(emissions.coal.related, aes(x=factor(year), y=Emissions/1000,fill=year, label = round(Emissions/1000,2))) +
  geom_bar(stat="identity") +
  #geom_bar(position = 'dodge')+
  # facet_grid(. ~ year) +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emissions in kilotons")) +
  ggtitle("Emissions from coal combustion-related sources in kilotons")+
  geom_label(aes(fill = year),colour = "white", fontface = "bold")
baltcitymary.emissions<-NEI[(NEI$fips=="24510") & (NEI$type=="ON-ROAD"),]
require(dplyr)
baltcitymary.emissions.byyear <- summarise(group_by(baltcitymary.emissions, year), Emissions=sum(Emissions))
require(ggplot2)
ggplot(baltcitymary.emissions.byyear, aes(x=factor(year), y=Emissions,fill=year, label = round(Emissions,2))) +
  geom_bar(stat="identity") +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emissions in tons")) +
  ggtitle("Emissions from motor vehicle sources in Baltimore City")+
  geom_label(aes(fill = year),colour = "white", fontface = "bold")

require(dplyr)
baltcitymary.emissions<-summarise(group_by(filter(NEI, fips == "24510"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))
losangelscal.emissions<-summarise(group_by(filter(NEI, fips == "06037"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))

baltcitymary.emissions$County <- "Baltimore City, MD"
losangelscal.emissions$County <- "Los Angeles County, CA"
both.emissions <- rbind(baltcitymary.emissions, losangelscal.emissions)

require(ggplot2)
ggplot(both.emissions, aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions,2))) +
  geom_bar(stat="identity") + 
  facet_grid(County~., scales="free") +
  ylab(expression("total PM"[2.5]*" emissions in tons")) + 
  xlab("year") +
  ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles in tons"))+
  geom_label(aes(fill = County),colour = "white", fontface = "bold")


