# Data Set has been taken from "https://www.kaggle.com/datasnaek/youtube-new#INvideos.csv"
# Used XLSX file as data 
library("readxl") # As we are using Excel file we are using this readxl library 
install.packages("readxl")# Installing readxl file
INV<- read.csv(file="INVideos.csv", header=TRUE)# Assigned QA variable to that excel sheet
View(INV)# to view the data 
##Direct Package available 
library("pastecs") # As we need descriptive stats ofour data using this pastecs library 
install.packages("pastecs")# Installing pastecs package
stat.desc(INV) #Entire Descriptive statistics of the Data
options(scipen = 100,digits = 2) # changing format to have easy understanding
stat.desc(INV,basic = F) #Basic Descriptive statistics of the Data
## Traditional Methods to find the Descriptive Stats for quantitative variables
mean(INV$likes) # Mean 
median(INV$likes) # Median
min(INV$likes) # Min
max(INV$likes) # Max
range(INV$likes) # Range
quantile(INV$likes) # Sample Quantiles
IQR(INV$likes) # Interquartile Range
var(INV$likes) # Variance
sd(INV$likes) # Standard Deviation
mad(INV$likes)  # Median Absolute Deviation
summary(INV$likes) # Sumamry stats of one variable: mean, median, 25th and 75th quartiles, min and max
#Mode : To Get mode we can utilise Modeest Package with genefilter in it from Biocmanager.
#(Using this because we can find mode of any column direclty via mfv function)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("genefilter") # Installing Genefilter
mfv(INV$category_id) #Mode Vectors
#Descriptive Stats for quantitative categorical variables
mfv(INV$channel_title) #Mode Character
table(INV$channel_title) #Count of Channel titles
Prp <- table(INV$category_id) #Proportions
prop.table(Prp) # Proportons of above category_id
# Transformations
lgV <- log(INV$views) # Log Transformation
lgV # View Log Transformation performed
Sqv <- sqrt(INV$views) # Square root Transformation
Sqv # View Square root Transformation
# Conversions
Dt <- as.Date(INV$trending_date, "%y.%d.%m") # Converting to Date format
class(Dt) # Verifying the format by Class
Year <- format(Dt,"%Y") # Pulling Year field from the Date
Year
UYr <- factor(c(Year)) # Finding Unique Year Values
UYr
# Charts
# stacked Bar Chart
Stkb <- table(UYr,Cty) 
Stkb
Clr =c("Orange","Yellow") # Assigning Colors
barplot(Stkb,ylim = c(0,15000),las = 1,col = Clr,legend.text = TRUE,beside = TRUE,
        main = "Stacked-Bar Plotting of Likes Per Category",
        ylab = "Likes",xlab = "Category Id") # Plotting Stacked Bar chart
# Bar Chart
Cty <-(factor(c(INV$category_id))) # Assigned Variable to find Unique charater Values of Category_id
Cty
Group <- tapply(INV$likes,Cty,sum) # To make a consoslidation of Category_id,Likes and Summing
Group
barplot(Group,main = "Bar Plotting of Likes Per Category",
                ylab = "Likes",xlab = "Category Id",ylim =c(0,500000000)) # Bar graph plotting
#Scatter Plot
# aaa <- plot(INV$comment_count,INV$likes,main = "Scatter Plotting of Likes Per Category",
#      ylab = "Likes",xlab = "Category Id",ylim =c(10,75),xlim=c(10,50)) # Plotting on Likes and Category_id
plot(Group,ylim = c(0,30000000),xlim = c(0,25),ylab = "Likes",xlab = "Category Id",main = "Scatter Plotting of Likes Per Category")
abline(lm(INV$comment_count ~ INV$likes),col='red')
#END