
## Date of code: 24/05/23
library(caret)
library(randomForest)
library(raster)
library(openxlsx)
library(Metrics)
library(rgdal)
library(dplyr)

file <- '1764_GEDI_2021.shp'
GEDI_data <- readOGR(file)

# Read the Sentinel imagery 
im_data <-'S2_1764_2021_allbands.tiff'
#names<-list.files(pattern = '*.tiff')
img_data<-stack(im_data)
# im_data <-raster('Input/18 05 23 - Sentinel/S2_1764_2021_allbands.tiff')
#extracting image values at plot locations
img_val<-extract(img_data,GEDI_data)
ht_data<-as.data.frame(cbind(GEDI_data,img_val))
htData<-ht_data[,,-c((ncol(ht_data)-1):ncol(ht_data))]#removing the last two columns of coordinates

#select only one height data (eg. rh98) and image data columns
htData<-select(htData,rh98, colnames(img_val))
#Following step can only be used when there is sufficient data for dividing into two parts
frac<-createDataPartition(htData$rh98, p = .65, list = FALSE, group = 10)

mean_y <- mean(frac[, 1])
sd_y <- sd(frac[, 1])

# Set a threshold for outlier detection (e.g., 3 times the standard deviation)
threshold <- 0.5 * sd_y

# Identify the indices of the anomalous points
anomalous_indices <- which(abs(frac[, 1] - mean_y) > threshold)

# Remove the anomalous points from the data frame
clean_frac <- frac[-anomalous_indices, ]

# Plot the cleaned data points
plot(frac[, 1])

training<-htData[frac,]
testing<-htData[-frac,]

#regression modelling
fitControl <- trainControl(method = "cv",
                           number = 5,
                           savePredictions = TRUE)

rfGrid<- expand.grid(mtry = c(1:6))

set.seed(1)
rfFit1 <- train(rh98~.,data = training,
                method = "rf",
                trControl = fitControl,
                verbose = FALSE,
                importance=TRUE,
                tuneGrid = rfGrid)

rfFit1

rfImp <- varImp(rfFit1, scale=FALSE)
plot(rfImp, main = par(cex.axis=2))

test<-testing[,-1]
predictedHt <- predict(rfFit1,test)
result<-lm(predictedHt~testing[,1])
summary(result)

rmse(testing[,1],predictedHt)
nRMSE<-sqrt( mean( (predictedHt-testing[,1])^2) ) / ( max(testing[,1])-min(testing[,1]) )
nRMSE	 
plot(testing[,1],predictedHt,pch = 16,cex.axis = 1.5, cex.lab = 1.5, col = "blue",main = "GEDI vs Model Predicted Height\n RF Regression\n", xlim = c(0,25), ylim = c(0,25), xlab = "GEDI height (m)", ylab = "Model height (m)")			
abline(1.123e-15, 1)

ht<- predict(img_data,rfFit1)
writeRaster(ht,'1764_ht_21.tif', overwrite = TRUE)
#mean height from the map
cellStats(ht, mean)