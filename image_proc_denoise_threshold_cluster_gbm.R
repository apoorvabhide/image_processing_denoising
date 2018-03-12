#In the output of linear regression, we still had some creases left.
#We need to capture the fact that the writing is  darker than the background.
#Using thresholding -> pixels can only be black or white, no grey shades. Must decide where the
#cut-off for black vs white is, though.
library(png)
library(raster)
library(gbm)
library(data.table)

image <- readPNG("~/R Work/image_proc_denoise_kaggle/train/6.png")
#Turn the image into a vector
img_to_vec <- function(image)
{
  return(matrix(image, nrow(image) * ncol(image), 1))
}
#Let's see a histogram
hist(img_to_vec(image))
#The histogram is tri-modal, which means two threshold choices: ~0.3 and ~0.65
#Setting a threshold at 0.3
image0.3 <- image
image0.3[image0.3 <= 0.3] = 0 
image0.3[image0.3 > 0.3] = 1
plot(raster(image0.3))
#No false positives, but the text isn't really clear.

#Setting a threshold at 0.65
image0.65 <- image
image0.65[image0.65 <= 0.65] = 0 
image0.65[image0.65 > 0.65] = 1
plot(raster(image0.65))
#Much better than 0.3
#But, can we automate this?

#Can cluster the image around 3 brightnesses, and use the splits for thresholds.

#Fitting 3 clusters
vec <- img_to_vec(image)
km <- kmeans(vec, 3)
#Clusters will be randomly ordered, so need to order them properly
oc <- order(km$centers)

#The lower threshold is the point halfway between the bottom two clusters
lower_thresh <- 0.5 * (max(vec[km$cluster == oc[1]]) + min(vec[km$cluster == oc[2]]))
high_thresh <- 0.5 * (max(vec[km$cluster == oc[2]]) + min(vec[km$cluster == oc[3]]))

#Using the lower threshold
image_lo <- image
image_lo[image_lo <= lower_thresh] <- 0
image_lo[image_lo > lower_thresh] <- 1
plot(raster(image_lo))

#Using the higher threshold
image_hi <- image
image_hi[image_hi <= high_thresh] <- 0
image_hi[image_hi > high_thresh] <- 1
plot(raster(image_hi))
#Again, the higher threshold works much better

#Now then, let's combine this with a linear-ish model (GBM)
#Writing a function to do k-means with thresholding
kmeans_thresh <- function(image)
{
 #Fit the three clusters
  vec <- img_to_vec(image)
  km <- kmeans(vec, 3)
  #Order the clusters, set and choose the higher threshold
  oc <- order(km$centers)
  high_thresh <- 0.5 * (max(vec[km$cluster == oc[2]]) + min(vec[km$cluster == oc[3]]))
  image_hi <- vec
  image_hi[image_hi <= high_thresh] <- 0
  image_hi[image_hi > high_thresh] <- 1
  return(image_hi)
}

#This part is copied from the linear file, setting the folders and stuff
#Let's set up the folders
dirty_folder <- "~/R Work/image_proc_denoise_kaggle/train/"
clean_folder <- "~/R Work/image_proc_denoise_kaggle/train_cleaned/"
out_folder <- "~/R Work/image_proc_denoise_kaggle/train_predicted/"

out_path <- file.path(out_folder, "trainingdata.csv")
filenames <- list.files(dirty_folder)

#Now, we read the files one by one, x is matrix of dirty brightness values, y is a vector of 
#the cleaned up brightness values.
for(f in filenames)
{
  print(f)
  image_x <- readPNG(file.path(dirty_folder, f))
  image_y <- readPNG(file.path(clean_folder, f))
  
  #Turn the images into vectors
  x <- matrix(image_x, nrow(image_x) * ncol(image_x), 1)
  y <- matrix(image_y, nrow(image_y) * ncol(image_y), 1)
  
  #Threshold the image
  x2 <- kmeans_thresh(image_x)
  
  #Rest of it again copied from 'linear' with modifications
  dat1 <- data.table(cbind(y, x, x2))
  setnames(dat1, c("y", "x", "thresholded"))
  write.table(dat1, file=out_path, append=(f != filenames[1]), sep=",", row.names=FALSE, col.names=(f == filenames[1]), quote=FALSE)
}  

#View the data
dat1 <- read.csv(out_path)
rows <- sample(nrow(dat1), 10000)
d1 <- dat1[rows,]
plot(d1$x[dat1$thresholded == 0], d1$y[dat1$thresholded == 0], col = 'blue')
lines(d1$x[dat1$thresholded == 1], d1$y[dat1$thresholded == 1], col = 'red', type = 'p')

#Fit gbm model to a subset of the data
rows <- sample(nrow(dat1), 100000)
gbm_model <- gbm(y ~ x + thresholded, data = dat1[rows,], n.trees = 5000, cv.folds = 10, train.fraction = 0.5)
best_iter <- gbm.perf(gbm_model, method = "cv")

#Score on the training data
#DON'T RUN THIS ALL THE TIME: SYSTEM WILL HANG!
y_predict <- predict(gbm_model, newdata = dat1, n.trees = best_iter)
rmse <- sqrt(mean((y_predict - dat1$y) ^ 2))
print(rmse)

#Let's look at the result for a sample image
image <- readPNG("~/R Work/image_proc_denoise_kaggle/train/6.png")
x <- data.table(matrix(image, nrow(image) * ncol(image), 1), kmeans_thresh(image))
setnames(x, c("raw", "thresholded"))
y_predict <- predict(gbm_model, newdata = x, n.trees = best_iter)
image_out <- matrix(y_predict, nrow(image), ncol(image))
writePNG(image_out, )