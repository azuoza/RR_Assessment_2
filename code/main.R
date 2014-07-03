
library(psych)
library(tools)
library(ggplot2)
library(plyr)
library(reshape2)

install.packages("xtable")
library(xtable)

# reading data

con <- file(description = "C:/Users/az/Desktop/old/repdata_data_StormData.csv", open = "r")
storm <- read.csv(con, stringsAsFactors=FALSE)
close(con)

print(object.size(storm), units="Mb")

# bz2 reading data.

download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                 destfile="./data/repdata_data_StormData.csv.bz2",method="auto")
fl <- bzfile("./data/repdata_data_StormData.csv.bz2", open="r")

#Load data and assign it to variable "data"
storm <- read.csv(fl, stringsAsFactors=FALSE)

close(fl)


md5sum("./data/repdata_data_StormData.csv.bz2") == "df4aa61fff89427db6b7f7b1113b5553"

temp <- as.data.frame(table(storm$EVTYPE))
dimnames(temp)

dimnames(temp[max(temp)])
plot(storm$EVTYPE)

# removing columns that isn't needed. 

# column of interest 
colnm <- c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")

# small storm data set
sstorm <- storm[, colnm]
# only 50 US state and DC
sstorm <- sstorm[sstorm$STATE %in% c(state.abb, "DC"), ]
dim(sstorm)
print(object.size(sstorm), units="Mb")

storm.healt <- subset(x = sstorm, subset = (sstorm$INJURIES!=0 | sstorm$FATALITIES!=0))
print(object.size(storm.healt), units="Mb")

storm.healt$BGN_DATE <- as.Date(storm.healt$BGN_DATE, "%m/%d/%Y")
rownames(storm.healt) <- NULL

# grafikas apie datu pasiskirstyma ----
temp <- as.data.frame(table(storm.healt$BGN_DATE))
temp$Var1 <- as.Date(as.character(temp$Var1), "%Y-%m-%d")

plot(x = temp$Var1, y = temp$Freq, type="l")

qplot(x = Var1, y = Freq, data = subset(x = temp, subset = temp$Var1 > "2011-01-01"),geom = "line")
qplot(x = Var1, 
      y = Freq, 
      data = temp, 
      geom = "line", 
      main = "Weather events in US from 1950-01-01 till 2011-11-30", 
      xlab = "", 
      ylab = "frequency"
      )

# surakite tris maksimumus ir sudekime juos ant grafiko. 
head(
    temp[order(-temp$Freq), ]
    )

fig1 <- ggplot(data = temp, aes(x = Var1, y = Freq)) + geom_line()
fig1 <- fig1 + labs(list(
                    title = "Weather events in US from 1950-01-01 till 2011-11-30", 
                    x = "", 
                    y = "frequency")
                )
fig1 <- fig1 + annotate(geom = "text", x = as.Date("1972-01-01"), y = 130, label = "134")
fig1 <- fig1 + annotate(geom = "text", x = as.Date("2009-11-01"), y = 120, label = "120")
fig1 <- fig1 + annotate(geom = "text", x = as.Date("1964-01-01"), y = 55, label = "54")
fig1


# Multiplot ----

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

# suzeisti ir zuvusieji ----

# fatalities and injuries 
dim(storm.healt)
str(storm.healt)
describe(storm.healt)

# Mano kategorijos 

kate <- c("TORNADO", "WIND", "HAIL", "FLOOD", "COLD", "HURRICANE", "FOG",
          "RIP CURRENT", "WIND", "LIGHTNING", "HEAT", "RAIN", "COLD", 
          "FLOOD", "FLOOD", "COLD", "WIND", "WIND", "WIND", "HIGH SEAS", "WIND", 
          "STORM", "SLEET", "FLOOD", "WIND", "HEAT", "WIND", "SURF", 
          "FIRE", "WIND", "WIND", "STORM", "WIND", "FLOOD", "SNOW", 
          "FOG", "WIND", "WIND", "FLOOD", "WIND", "ICE", "HEAT", "HEAT", 
          "WIND", "BILZZARD", "TORNADO", "HURRICANE", "TORNADO", "WIND", 
          "STORM", "FLOOD", "HURRICANE", "TORNADO", "STORM", "STORM", "LIGHTNING", 
          "STORM", "OTHER", "WIND", "FLOOD", "SNOW", "RAIN", "SNOW", 
          "FLOOD", "HEAT", "WIND", "RAIN", "FOG", "SNOW", "WIND", "ICE", 
          "SNOW", "FLOOD", "COLD", "LIGHTNING", "HURRICANE", "WIND", "COLD", "TORNADO", 
          "FLOOD", "ICE", "AVALANCE", "SNOW", "RIP CURRENT", "COLD", 
          "FOG", "SURF", "ICE", "HEAT", "TORNADO", "RIP CURRENT", 
          "HURRICANE", "HURRICANE", "STORM", "SNOW", "SNOW", "RAIN", 
          "HEAT", "SNOW", "HEAT", "HEAT", "COLD", "HEAT", "WIND", 
          "TSUNAMI", "COLD", "COLD", "WIND", "COLD", "COLD", "WIND", 
          "COLD", "FLOOD", "RAIN", "SNOW", "WIND", "WIND", "COLD", 
          "FLOOD", "RAIN", "STORM", "FLOOD", "ICE", "OTHER", "WIND", 
          "FLOOD", "FLOOD", "HEAT", "OTHER", "FLOOD", "WIND", "WIND", 
          "FLOOD", "SURF", "FIRE", "FOG", "WIND", "SURF", "STORM", 
          "HURRICANE", "SURF", "COLD", "RAIN", "HURRICANE", "STORM", 
          "FLOOD", "WIND", "COLD", "WIND", "WIND", "SNOW", "OTHER", 
          "COLD", "WIND", "MUDSLIDES", "MUDSLIDES", "COLD", "FLOOD", 
          "COLD", "HEAT", "SNOW", "COLD", "RAIN", "SNOW", "COLD", 
          "COLD", "ICE", "STORM", "WIND", "SNOW", "COLD", "SNOW", 
          "WIND", "WIND", "HAIL", "FLOOD", "WIND", "WIND", "COLD", 
          "COLD", "WIND", "SNOW", "ICE", "WIND", "WIND", "WIND", "HEAT",
          "WIND", "FLOOD", "SNOW", "COLD", "WIND", "ICE", "WIND", 
          "OTHER", "FIRE", "SURF", "ICE", "RAIN", "WIND", "FIRE", "SURF",
          "COLD", "HURRICANE", "FLOOD", "FLOOD", "WIND", "STORM", 
          "TSUNAMI")

kategorijos <- data.frame(orig=unique(storm.healt$EVTYPE), 
                          new=kate)






storm.healt$NEVTYPE <- NA

for(i in 1:length(storm.healt$NEVTYPE)){
    event <- paste("^", storm.healt$EVTYPE[i], "$", sep="")
    if(grepl(pattern = ".*TSTM WIND.*", x=event)){
        event <- "TSTM WIND"
    }
    if(grepl(pattern = ".*THUNDERSTORM WIND*", x=event)){
        event <- "THUNDERSTORM WIND"
    }
    storm.healt$NEVTYPE[i] <- as.character(kategorijos$new[grep(pattern = event, x = kategorijos$orig)])
    
}


storm.healt$NEVTYPE <- as.factor(storm.healt$NEVTYPE)

# Fatalities and Injuries cost column
storm.healt$FICOST <- (7000000*storm.healt$FATALITIES)+(10000*storm.healt$INJURIES)

# grafikas apie ivykiu pasiskirstyma laike
    fig2 <- ggplot(data=storm.healt, aes(x=BGN_DATE, y=NEVTYPE)) + geom_point()
    fig2

# grafikas apie 

FI.df <- aggregate(storm.healt[, c(4, 5, 11)], by=list(storm.healt$NEVTYPE), FUN=sum)
FI.df <- FI.df[order(-FI.df$FICOST), ]
FI.df$FICOST <- FI.df$FICOST/1000


    fig3 <- ggplot(data=FI.df, aes(x=Group.1)) + 
            geom_point(aes(y=FATALITIES), color = "red") + 
            geom_point(aes(y=INJURIES), color = "blue")
    fig3 <- fig3 + xlab("") + ylab("event count") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

    fig3
    


FI.melt <- melt(FI.df[, 1:3], id.vars="Group.1", variable.name = "Victoms", 
                    value.name = "Rate")
FI.melt$Victoms <- as.factor(FI.melt$Victoms)

    fig3.1 <- ggplot(data=FI.melt, aes(x=Group.1, y=Rate, colour=Victoms))
    fig3.1 <- fig3.1 + geom_point() + xlab("") + 
                ylab("Number of Fatalieties and Injuries") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

    fig3.1

# calculating upper point of the bar
    upper <- FI.df$FICOST + 20000

    fig4 <- ggplot(data=FI.df, 
                   aes(x=reorder(Group.1, FICOST), y=FICOST)) 
    fig4 <- fig4 + geom_bar(stat="identity")
    fig4 <- fig4 + xlab("") + ylab("Fatalities and Injuries cost, k$") + 
    geom_text(aes(label = FICOST, y = upper), size = 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

   multiplot(fig3.1, fig4, layout=matrix(c(1,2), nrow=1, byrow=TRUE)) 

print(xtable(FI.df[1:10, ]), type="html")

storm.healt$cost.total <- NA
for(i in 1:dim(storm.healt)[1]){
    property <- NA
    if(storm.healt$PROPDMGEXP[i]=="H"){
        property <- storm.healt$PROPDMG[i]*100
    } else if(storm.healt$PROPDMGEXP[i]=="K"){
        property <- storm.healt$PROPDMG[i]*1000
    } else if((storm.healt$PROPDMGEXP[i]=="m") | (storm.healt$PROPDMGEXP[i]=="M")){
        property <- storm.healt$PROPDMG[i]*1000000
    } else if(storm.healt$PROPDMGEXP[i]=="B"){
        property <- storm.healt$PROPDMG[i]*1000000000
    } else {
        property <- storm.healt$PROPDMG[i]
    }
    
    crop <- NA
    if(storm.healt$CROPDMGEXP[i]=="B"){
        crop <- storm.healt$CROPDMG[i] * 1000000000
    } else if (storm.healt$CROPDMGEXP[i]=="M") {
        crop <- storm.healt$CROPDMG[i] * 1000000
    } else if (storm.healt$CROPDMGEXP[i]=="K") {
        crop <- storm.healt$CROPDMG[i] * 1000
    } else {
        crop <- storm.healt$CROPDMG[i]
    }
    
    storm.healt$cost.total[i] <- crop + property
}

cost.df <- aggregate(storm.healt[, 12], by=list(storm.healt$NEVTYPE), FUN=sum)
colnames(cost.df) <- c("Event", "Cost")

fig5 <- ggplot(data=cost.df, aes(x=reorder(Event, Cost), y=Cost)) + 
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))+
    xlab("")

fig5

# US map ----
install.packages("XML")

library(maps)
library(XML)

dim(sstorm)
all_states <- map_data("state")

unemp <-
    readHTMLTable('http://www.bls.gov/web/laus/laumstrk.htm',
                  colClasses = c('character', 'character', 'numeric'))[[2]]

names(unemp) <- c('rank', 'region', 'rate')
unemp$region <- tolower(unemp$region)

us_state_map <- map_data('state')
map_data <- merge(unemp, us_state_map, by = 'region')

map_data <- arrange(map_data, order)

states <- data.frame(state.center, state.abb)

p1 <- ggplot(data = map_data, aes(x = long, y = lat, group = group))
p1 <- p1 + geom_polygon(aes(fill = cut_number(rate, 5)))
p1 <- p1 + geom_path(colour = 'gray', linestyle = 2)
p1 <- p1 + scale_fill_brewer('Unemployment Rate (Jan 2011)', palette  = 'PuRd')
p1 <- p1 + coord_map()
p1 <- p1 + geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 2)
p1 <- p1 + theme_bw()
p1


# state.name visi statu pavadinimai

length(table(sstorm$STATE))

# combinig data
sstorm <- subset(sstorm, sstorm$EVTYPE == "TORNADO")
sstorm$BGN_DATE <- as.Date(sstorm$BGN_DATE, "%m/%d/%Y")
sstorm.agr <- aggregate(sstorm$INJURIES, by=list(sstorm$STATE), FUN=sum)
colnames(sstorm.agr) <- c("abb", "Injuries")

states.all <- data.frame(abb = state.abb, name = state.name)
sstorm.agr <- merge(sstorm.agr, states.all, by="abb")
sstorm.agr$name <- tolower(sstorm.agr$name)
colnames(sstorm.agr) <- c("abb", "Injuries", "region")

us_state_map1 <- map_data('state')
states1 <- data.frame(state.center, state.abb)
map_data1 <- merge(sstorm.agr, us_state_map1, by = 'region')
map_data1 <- arrange(map_data1, order)


p1 <- ggplot(data = map_data1, aes(x = long, y = lat, group = group))
p1 <- p1 + geom_polygon(aes(fill = cut_number(Injuries, 5)))
p1 <- p1 + geom_path(colour = 'gray', linestyle = 2)
p1 <- p1 + scale_fill_brewer('Unemployment Rate (Jan 2011)', palette  = 'PuRd')
#p1 <- p1 + coord_map()
p1 <- p1 + geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 2)
p1 <- p1 + theme_bw()
p1
