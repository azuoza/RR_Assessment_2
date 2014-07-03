library(RCurl)

# testing loading data from bz2 from internet

url1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

# smaller data set and zip file
url2 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

con <- url(description = url2, open = "r", blocking = T)
down
close(con)
