#The kaggle competition is about denoising document images to remove stains, creases etc. and read
#just the text.

#Here, I'm going to try a simple linear regression model.
library(png)
library(raster)
library(data.table)

#Let's look at one image
sample_image <- readPNG("~/R Work/image_proc_denoise_kaggle/train/6.png")
head(sample_image)
plot(raster(sample_image))
#All the brightness values are between 0 to 1, 0 = black, 1 = white

#Let's set up the folders
dirty_folder <- "~/R Work/image_proc_denoise_kaggle/train/"
clean_folder <- "~/R Work/image_proc_denoise_kaggle/train_cleaned/"
out_folder <- "~/R Work/image_proc_denoise_kaggle/train_predicted/"

out_path <- file.path(out_folder, "trainingdata.csv")
filenames <- list.files(dirty_folder)
#read the files one by one, x is matrix of dirty brightness values, y is a vector of 
#the cleaned up brightness values.
for(f in filenames)
{
  print(f)
  image_x <- readPNG(file.path(dirty_folder, f))
  image_y <- readPNG(file.path(clean_folder, f))
  
  #Turn the images into vectors
  x <- matrix(image_x, nrow(image_x) * ncol(image_x), 1)
  y <- matrix(image_y, nrow(image_y) * ncol(image_y), 1)
  
  dat1 <- data.table(cbind(y, x))
  setnames(dat1, c("y", "x"))
  write.table(data, file=out_path, append=(f != filenames[1]), sep=",", row.names=FALSE, col.names=(f == filenames[1]), quote=FALSE)
}

#Now, let's view the data
dat1 <- read.csv(out_path)
head(dat1)
rows <- sample(nrow(dat1), 10000)
plot(dat1$x[rows], dat1$y[rows])

#The plot looks like a fairly linear relationship, except at the ends
#Let's fit a linear model
linear_model <- lm(y ~ x, data = dat1[dat1$y > 0.05 & dat1$y < 0.95,])
summary(linear_model)
dat1$predicted <- sapply(predict(linear_model, newdata = dat1), function(x) max(min(x,1),0))
plot(dat1$predicted[rows], dat1$y[rows])
rmse1 <- sqrt(mean( (dat1$y - dat1$x) ^ 2))
rmse2 <- sqrt(mean( (dat1$predicted - dat1$y) ^ 2))
c(rmse1, rmse2)
#The RMS error has gone down from 11.7% to 1.9% by applying the linear model on this image!

#Let's see the result for an image
image <- readPNG("~/R Work/image_proc_denoise_kaggle/train/6.png")
x <- data.table(matrix(image, nrow(image) * ncol(image), 1))
setnames(x, "x")
y_image <- sapply(predict(linear_model, newdata = x), function(x) max(min(x,1), 0))
image_out <- matrix(y_image, nrow(image), ncol(image))
writePNG(image_out, "~/R Work/image_proc_denoise_kaggle/6.png")
plot(raster(image_out))