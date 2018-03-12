# image_processing_denoising

This is an attempt to learn how to process image data.
I'm using Colin Priest's tutorial on a kaggle dataset to denoise images.

1. The image_proc_denoise_linear file uses a linear regression model to clean the image.
As you can see, it gets pretty good results. However, better results are possible.

2. The image_proc_denoise_threshold_cluster_gbm file, as the name suggests, first uses k-means clustering to cluster the pixels based on brightness, then chooses the midpoint of the higher two clusters as a threshold (everything higher than that is white, lower is black.) Then it uses this to build a gbm model, similar to the linear regression model from file 1. The results are even better than the first case.
