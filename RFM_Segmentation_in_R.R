#######################################################################################
##                                                                                   ##
## RFM segmentation in Retail industry                                               ##
##                                                                                   ##
#######################################################################################
##
setwd("/Volumes/Misc/Retail Analytics")
df <- read.table("sample_data.txt",header=F)
head(df)

#construct data frame using customer ID, transaction date, and 
#amount spent by a customer per transaction
df <- as.data.frame(cbind(df[,1],df[,2],df[,4]))
names <- c("Cust ID","Trx Date","Amount Spent per trx")
names(df) <- names
#format date column
df[,2] <- as.Date(as.character(df[,2]),"%Y%m%d")
head(df)

####
#segment the customers into RFM cells by the three dimensions of Recency, Frequencey, and Monetary.
#Customers are rated on 1 to 5 scale,5 being the best.
#
#meaning of "543" score: customer have 5 points in Recency, 
#4 points in Frequency, and 3 points in Monetary.
####
#set the start_date and end_date as per your analysis date range.
start_date <- as.Date("19990101","%Y%m%d")
end_date <- as.Date("19991231","%Y%m%d")
df <- df[order(df[,2],decreasing = TRUE),]
df <- df[df[,2]>=start_date,]
df <- df[df[,2]<=end_date,]

#remove duplicate customer IDs
newdf <- df[!duplicated(df[,1]),]

# calculate Recency(in days) to the end_date, smaller days value means more recent
Recency<-as.numeric(difftime(end_date,newdf[,2],units="days"))
newdf <-cbind(newdf,Recency)

# calculate frequency
newdf <- newdf[order(newdf[,1]),]
fre <- as.data.frame(table(df[,1]))
Frequency <- fre[,2]
newdf <- cbind(newdf,Frequency)

#calculate spent per trx
#newdf <- newdf[order(newdf[,1]),]
m <- as.data.frame(tapply(df[,3],df[,1],sum))
Monetary <- m[,1]/Frequency
newdf <- cbind(newdf,Monetary)

newdf_bkp <- newdf
head(newdf_bkp)

####
#scoring based on business rule (important - should be meaningful)
####
##
##based on the distribution of your data set ranges (for R, F, M) as stated in below example.
##use table(), hist() to break ranges.
##

# set Recency ranges as 0-30 days, 30-90 days, 90-180 days, 180-270 days and more than 270 days.
r <-c(30,90,180,270)
# set the Frequency ranges as 0-3 times, 3-5 times, 5-10 times, 10-12 times, and more than 12 times.
f <-c(3,5,10,12)
# set the Monetary ranges as 0-100 dollars, 100-200 dollars, 200-400 dollars, 400-500 dollars and more than 500.
m <-c(100,200,400,500)

## scoring the Recency
i=1
while(i <= length(newdf[,1])){
  j=1
  while(j <= length(r)){
   if(newdf$Recency[i] < r[j]){
     newdf$R_Score[i] <-  length(r)+2-j
     j <- length(r)+1
   }
   else
     j<-j+1
  }
  if(newdf$Recency[i] >= r[j-1])
    newdf$R_Score[i] <-  length(r)+2-j
  i<- i+1
}

## scoring the Frequency
i=1
while(i <= length(newdf[,1])){
  j=1
  while(j <= length(f)){
    if(newdf$Frequency[i] < f[j]){
      newdf$F_Score[i] <-  j
      j <- length(f)+1
    }
    else
      j<-j+1
  }
  if(newdf$Frequency[i] >= f[j-1])
    newdf$F_Score[i] <-  j
  i<- i+1
}

## scoring the Monetary
i=1
while(i <= length(newdf[,1])){
  j=1
  while(j <= length(m)){
    if(newdf$Monetary[i] < m[j]){
      newdf$M_Score[i] <-  j
      j <- length(m)+1
    }
    else
      j<-j+1
  }
  if(newdf$Monetary[i] >= m[j-1])
    newdf$M_Score[i] <-  j
  i<- i+1
}

#order the dataframe by R_Score, F_Score, and M_Score desc
newdf <- newdf[order(-newdf$R_Score,-newdf$F_Score,-newdf$M_Score),]
  
# caculate the total score
Total_Score <- c(100*newdf$R_Score + 10*newdf$F_Score+newdf$M_Score)  
newdf <- cbind(newdf,Total_Score)
head(newdf)

final <- newdf[,!names(newdf)%in%c("Trx Date","Amount Spent per trx")]
head(final)

######
##See how many customers have a total score of more than the 500,400 or the figure given by business
######
imp_cust <- final[final$Total_Score>500,]  
