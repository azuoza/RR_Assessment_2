
# loading library
library(foreach)
library(doParallel)

#Download file to data dircetory and unzip it (.bz2)
bz2=download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                  destfile="./data/repdata_data_StormData.csv.bz2",method="curl")
file=bzfile("./data/repdata_data_StormData.csv.bz2", open="r")

#Load data and assign it to variable "data"
data=read.csv(file)

close(file)


# read 20 lines of data

#setup parallel backend to use 8 processors
cl<-makeCluster(2)
registerDoParallel(cl)

strt<-Sys.time()
storm <- foreach(1) %dopar% {
    temp <- read.csv("C:/Users/az/Desktop/old/repdata_data_StormData.csv")
    return(temp)    
}
print(Sys.time()-strt)
stopCluster(cl)


strt<-Sys.time()
con <- file(description = "C:/Users/az/Desktop/old/repdata_data_StormData.csv", open = "r")
temp <- read.csv(con)
close(con)
print(Sys.time()-strt)

str(temp)
table(temp$STATE)
table(temp$F)
str(temp$BGN_DATE)
temp$BGN_DATE[1:10]
temp2 <- as.character(temp$BGN_DATE)
temp2 <- as.Date(temp2)
