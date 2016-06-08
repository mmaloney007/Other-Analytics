############################################## 
# Standard Response/Propensity Model demo code
# uses new CustData XDF
# Illustrates customer analysis to optimize
# marketing spend
##############################################

# set proper directory

setwd("~/Documents")						

dataFile<-"CustData.csv"

mydata = read.csv(dataFile, header=TRUE, fill=TRUE, stringsAsFactors=TRUE)  # read csv file 
#mydata 

#####################################################
## 
## Exploratory data analysis
##
#####################################################
read.csv
## get general information about the file and the variables in it
names(mydata)

## a summary
#rxSummary(~RESPONSE + AVG_PURCH + NUMPROM + NUMPRM12 + NUMPURCH_LIFE, data=xdfFile)
summary(mydata)

#mydata["AVG_PURCH"]
## we can inspect them visually, too. A histogram of average purchasae, for example:
#rxHistogram(~AVG_PURCH, data=xdfFile)

hist(mydata["AVG_PURCH"], freq=FALSE, xlab="Average Purchase", main="Average Purchase", col="lightgreen")
curve(dnorm(mydata, mean=mean(AVG_PURCH), sd=sd(AVG_PURCH)), add=TRUE, col="darkblue", lwd=2)

rxHistogram(~AVG_PURCH|State, data=xdfFile)

#################################################
## More visual analysis. A scatter plot
## use this analysis to better understand your customers

rxLinePlot(NUMPURCH_LIFE~NUMPROM, type="p", data=xdfFile)

#################################################
# perform cross tabs
# look for top buyers by age and city

rxCube(formula = PURCHASE~F(AGE):City, data = xdfFile)

#################################################
# analyze customers geographically
#
# use ScaleR to process large data and pass results to OSR routines
# use ggplot2 to plot customers on a map of the US
#

# get purchases by City
counts<-rxCube(formula = PURCHASE~City, returnDataFrame = TRUE, data = xdfFile)
counts

# read location file - unique by City and sorted
cities<-read.delim("US Cities location-unq_sort.txt",header=TRUE)
cities

# append geographic fields to rxCube results
# includes calculation of a Spend column
counts$State<-cities$State
counts$Lat<-cities$Lat
counts$Long<-cities$Long
counts$Spend<-counts$PURCHASE*counts$Counts

counts

require(ggplot2)
require(maps)

states <- data.frame(map("state", plot=FALSE)[c("x","y")])
colnames(states) <- c("Lon","Lat")

p <- ggplot(states, aes(x=Lon, y=Lat)) + geom_path() 
s<-counts$Spend/691800   # scale Spend for plotting
p <- p + geom_point(data = counts, aes(x= -Long, y = Lat),alpha=0.2,size=s*70, color = "red")
p<-p + labs(title = "Spend Concentration")
p

#####################################################
## 
## build models - predict likely buyers to better allocate
## marketing expenditures
##
#####################################################

#############################################
# Decision Tree
#############################################

system.time(tr1 <- rxDTree(formula=RESPONSE~AVG_PURCH + REC_PURCH_AMT + NUMPURCH_LIFE,maxDepth = 3,data=xdfFile))

#summary(tr1)

tr1

########### picture the tree

library(RevoTreeView)

plot(createTreeView(tr1))

##################################################
##################################################

########################################################
## kMeans clustering - behavioral analysis
## Are there groups of customers who fall into
## natural segments with similar buying behavior?
# If so, they may respond to similar marketing messages
########################################################

# create an output file with cluster membership number
clustFile <- "custClusters.xdf"
if(file.exists(clustFile)) file.remove(clustFile)	

system.time(k1 <- rxKmeans(formula=~AVG_PURCH + AGE + NUMPROM + NUMPRM12 + NUMPURCH_LIFE, 
	data=xdfFile, outFile=clustFile,writeModelVars=TRUE,overwrite=TRUE, numClusters=5))


rxGetInfo(data=clustFile, getVarInfo=TRUE,numRows=3)

#############################################
### Different ways to visualize the clusters
#############################################

# cluster analysis plot - 1 panel for each cluster
rxLinePlot(AVG_PURCH~AGE | .rxCluster, data=clustFile, type="p", title="Behavioral Cluster Analysis")

# cluster analysis plot - 1 panel with each data point in color by cluster
rxLinePlot(AVG_PURCH~AGE, groups=.rxCluster, data=clustFile, type="p", title="Behavioral Cluster Analysis")

# cluster analysis plot - 1 panel for each NUMPRM12 with each data point in color by cluster
# use rowSelection to limit the number of panels displayed - easier to read 
rxLinePlot(AVG_PURCH~AGE | NUMPRM12, groups=.rxCluster, data=clustFile, type="p",rowSelection=(NUMPRM12<15), title="Behavioral Cluster Analysis")


#####################################################
####### Model Deployment
###
### use the model to generate customer scores
### these scores will be used for customer selectiom
### and segmentation for marketing promotions
###
### scores may get loaded into a customer DB to be 
### used in a campaign management tool
#####################################################

pred1<-rxPredict(tr1,data=xdfFile)

# analyze prediction (scores)

rxSummary(~RESPONSE_Pred, data=xdfFile)

rxHistogram(~RESPONSE_Pred, data=xdfFile)

#############################################
#############################################
#### clear prediction variable for next time
#############################################

tempFile<-"temp.xdf"

rxDataStep(inData = xdfFile, outFile = tempFile,
	varsToDrop = c("RESPONSE_Pred"))

rxDataStep(inData = tempFile, outFile = xdfFile,overwrite=TRUE)

rxGetInfo(data=xdfFile, getVarInfo=TRUE,numRows=3)

if(file.exists(tempFile)) file.remove(tempFile)	

#############################################
####### Supplemental examples
#############################################

## a linear regression:
system.time(reg1 <- rxLinMod(formula=RESPONSE~AVG_PURCH + REC_PURCH_AMT + NUMPURCH_LIFE, data=xdfFile))

summary(reg1)

## a logistic regression:
system.time(log1 <- rxLogit(formula=RESPONSE~AVG_PURCH + REC_PURCH_AMT + NUMPURCH_LIFE, data=xdfFile))

summary(log1)

### use the logistic model to make a prediction

pred1<-rxPredict(log1,data=xdfFile)

rxSummary(~RESPONSE_Pred, data=xdfFile)

# analyze prediction (scores)

rxSummary(~RESPONSE_Pred, data=xdfFile)

rxHistogram(~RESPONSE_Pred, data=xdfFile)

#############################################




