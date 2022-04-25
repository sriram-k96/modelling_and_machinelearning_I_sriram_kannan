Homework - 7
================
Sriram Kannan

1.  Work through the “Image Classification” tutorial on the RStudio
    Keras website.

``` r
library(keras)
```

    ## Warning: package 'keras' was built under R version 4.1.3

``` r
fashion_mnist <- dataset_fashion_mnist()
```

    ## Loaded Tensorflow version 2.8.0

``` r
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

dim(train_images)
```

    ## [1] 60000    28    28

``` r
dim(train_labels)
```

    ## [1] 60000

``` r
train_labels[1:20]
```

    ##  [1] 9 0 0 3 0 2 7 2 5 5 0 9 5 5 7 9 1 0 6 4

``` r
dim(test_images)
```

    ## [1] 10000    28    28

``` r
dim(test_labels)
```

    ## [1] 10000

``` r
library(tidyr)
library(ggplot2)

image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")
```

![](homework7_machinelearning_SriramK_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
train_images <- train_images / 255
test_images <- test_images / 255
```

``` r
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}
```

![](homework7_machinelearning_SriramK_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)
model %>% fit(train_images, train_labels, epochs = 5, verbose = 2)
```

``` r
score <- model %>% evaluate(test_images, test_labels, verbose = 0)
score
```

    ##     loss accuracy 
    ## 0.362046 0.870900

``` r
predictions <- model %>% predict(test_images)
predictions[1, ]
```

    ##  [1] 2.097862e-05 1.837528e-08 4.476495e-08 2.166520e-09 1.943991e-07
    ##  [6] 7.960267e-04 5.096706e-06 1.104418e-02 4.013616e-06 9.881294e-01

``` r
which.max(predictions[1, ])
```

    ## [1] 10

``` r
test_labels[1]
```

    ## [1] 9

``` r
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}
```

![](homework7_machinelearning_SriramK_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

2.  Use the Keras library to re-implement the simple neural network
    discussed during lecture for the mixture data (see nnet.R). Use a
    single 10-node hidden layer; fully connected.

``` r
library('rgl')
```

    ## Warning: package 'rgl' was built under R version 4.1.3

``` r
library('ElemStatLearn')
library('nnet')
library('dplyr')
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data("mixture.example")
dat <- mixture.example

## create 3D plot of mixture data
plot_mixture_data <- function(dat=mixture.example, showtruth=FALSE) {
  ## create 3D graphic, rotate to view 2D x1/x2 projection
  par3d(FOV=1,userMatrix=diag(4))
  plot3d(dat$xnew[,1], dat$xnew[,2], dat$prob, type="n",
         xlab="x1", ylab="x2", zlab="",
         axes=FALSE, box=TRUE, aspect=1)
  ## plot points and bounding box
  x1r <- range(dat$px1)
  x2r <- range(dat$px2)
  pts <- plot3d(dat$x[,1], dat$x[,2], 1,
                type="p", radius=0.5, add=TRUE,
                col=ifelse(dat$y, "orange", "blue"))
  lns <- lines3d(x1r[c(1,2,2,1,1)], x2r[c(1,1,2,2,1)], 1)
  
  if(showtruth) {
    ## draw Bayes (True) classification boundary
    probm <- matrix(dat$prob, length(dat$px1), length(dat$px2))
    cls <- contourLines(dat$px1, dat$px2, probm, levels=0.5)
    pls <- lapply(cls, function(p) 
      lines3d(p$x, p$y, z=1, col='purple', lwd=3))
    ## plot marginal probability surface and decision plane
    sfc <- surface3d(dat$px1, dat$px2, dat$prob, alpha=1.0,
      color="gray", specular="gray")
    qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
      color="gray", lit=FALSE)
  }
}

## compute and plot predictions
plot_nnet_predictions <- function(fit, dat=mixture.example) {
  
  ## create figure
  plot_mixture_data()

  ## compute predictions from nnet
  preds <- predict(fit, dat$xnew, type="class")
  probs <- predict(fit, dat$xnew, type="raw")[,1]
  probm <- matrix(probs, length(dat$px1), length(dat$px2))
  cls <- contourLines(dat$px1, dat$px2, probm, levels=0.5)

  ## plot classification boundary
  pls <- lapply(cls, function(p) 
    lines3d(p$x, p$y, z=1, col='purple', lwd=2))
  
  ## plot probability surface and decision plane
  sfc <- surface3d(dat$px1, dat$px2, probs, alpha=1.0,
                   color="gray", specular="gray")
  #qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
   #              color="gray", lit=FALSE)
}
```

``` r
## plot data and 'true' probability surface
plot_mixture_data(showtruth=TRUE)
```

``` r
## fit single hidden layer, fully connected NN 
## 10 hidden nodes
fit <- nnet(x=dat$x, y=dat$y, size=10, entropy=TRUE, decay=0) 
```

    ## # weights:  41
    ## initial  value 280.464184 
    ## iter  10 value 95.585499
    ## iter  20 value 87.521538
    ## iter  30 value 81.109828
    ## iter  40 value 73.146872
    ## iter  50 value 65.147086
    ## iter  60 value 64.282158
    ## iter  70 value 63.121310
    ## iter  80 value 62.994484
    ## iter  90 value 62.809911
    ## iter 100 value 62.590577
    ## final  value 62.590577 
    ## stopped after 100 iterations

``` r
plot_nnet_predictions(fit)
## count total parameters
## hidden layer w/10 nodes x (2 + 1) input nodes = 30 
## output layer w/1 node x (10 + 1) hidden nodes = 11
## 41 parameters total
length(fit$wts)
```

    ## [1] 41

3)Create a figure to illustrate that the predictions are (or are not)
similar using the ‘nnet’ function versus the Keras model.

4.  (optional extra credit) Convert the neural network described in the
    “Image Classification” tutorial to a network that is similar to one
    of the convolutional networks described during lecture on 4/15
    (i.e., Net-3, Net-4, or Net-5) and also described in the ESL book
    section 11.7. See the !ConvNet tutorial on the RStudio Keras
    website.

``` r
model2 <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(28,28,1)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")
summary(model2)
```

    ## Model: "sequential_1"
    ## ________________________________________________________________________________
    ##  Layer (type)                       Output Shape                    Param #     
    ## ================================================================================
    ##  conv2d_2 (Conv2D)                  (None, 26, 26, 32)              320         
    ##                                                                                 
    ##  max_pooling2d_1 (MaxPooling2D)     (None, 13, 13, 32)              0           
    ##                                                                                 
    ##  conv2d_1 (Conv2D)                  (None, 11, 11, 64)              18496       
    ##                                                                                 
    ##  max_pooling2d (MaxPooling2D)       (None, 5, 5, 64)                0           
    ##                                                                                 
    ##  conv2d (Conv2D)                    (None, 3, 3, 64)                36928       
    ##                                                                                 
    ## ================================================================================
    ## Total params: 55,744
    ## Trainable params: 55,744
    ## Non-trainable params: 0
    ## ________________________________________________________________________________

``` r
model2 %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")
model2 %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)
```

``` r
model2 %>% fit(train_images, train_labels, epochs = 5, verbose = 2)
```

``` r
score <- model2 %>% evaluate(test_images, test_labels, verbose = 0)
score
```

    ##      loss  accuracy 
    ## 0.2741347 0.9028000

``` r
predictions <- model2 %>% predict(test_images)
predictions[1, ]
```

    ##  [1] 1.872141e-07 2.016101e-08 2.275393e-07 6.843116e-09 1.356982e-07
    ##  [6] 2.371644e-03 5.514295e-07 1.131892e-03 1.113532e-06 9.964942e-01

``` r
which.max(predictions[1, ])
```

    ## [1] 10

``` r
test_labels[1]
```

    ## [1] 9

``` r
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}
```

![](homework7_machinelearning_SriramK_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
