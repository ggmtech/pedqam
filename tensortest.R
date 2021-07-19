reticulate::py_config()
which(python)


# just be aware that Tensorflow has to be installed first:
install.packages("tensorflow")
library(tensorflow)
install_tensorflow()  # Error: could not find a Python environment for /opt/anaconda3/bin/python
#install_tensorflow(version = "2.0.0")
# Create new environment r-reticulate in conda : conda create -n r-reticulate
# Install tensorflow from R-studio console with parameters : 
install_tensorflow(method = 'conda', envname = 'r-reticulate')
# Load the reticulate package 
library(reticulate)
library(tensorflow)
Sys.setenv(RETICULATE_PYTHON="/usr/local/bin/python")
# Activate the conda environment in R-studio 
use_condaenv('r-reticulate')
# Load the tensorflow libray 
library(tensorflow)
install_tensorflow()
sess <- tf$Session()
tf$constant("Hellow Tensorflow")  # Check if tensorflow is active 
# Error: Python module tensorflow was not found.


Sys.setenv(RETICULATE_PYTHON="/usr/local/bin/python")

!conda create --name tensorflow-env python=3.8 pip
conda activate tensorflow-env
pip install tensorflow


install.packages("keras")

reticulate::use_condaenv()
py_module_import(module, convert = convert)
library(keras)
install_keras()
install_keras(method = "conda", conda = reticulate::conda_binary())




# MNIST dataset is built into the Keras library. You can get it by calling the dataset_mnist()

library(keras)

mnist <- dataset_mnist()
X_train <- mnist$train$x
X_test <- mnist$test$x
y_train <- mnist$train$y
y_test <- mnist$test$y

# will only use linear layers (no convolutions), 
# so reshape  input images from 28×28 to 1×784 each wirh array_reshape()

# convert the labels stored as integers into  categories with  to_categorical() function


X_train <- array_reshape(X_train, c(nrow(X_train), 784))
X_train <- X_train / 255

X_test <- array_reshape(X_test, c(nrow(X_test), 784))
X_test <- X_test / 255

y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)

model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>%
    layer_dropout(rate = 0.25) %>% 
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(rate = 0.25) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 10, activation = "softmax")
summary(model)  # to print model


# before we can begin training – compiling the model. 
# choosing how loss is measured, choosing a function for reducing loss, and choosing a metric that measures overall performance
# Let’s go with categorical cross-entropy, Adam, and accuracy, respectively:

model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = c("accuracy")
    )

# call the fit() function to train the model. 
# The following snippet trains the model for 50 epochs, feeding 128 images at a time:

history <- model %>%  fit(X_train, y_train, epochs = 50, batch_size = 128, validation_split = 0.15)

# At the same time, you’ll see a chart updating as the model trains. It shows both loss and accuracy on training and validation subsets. 

model %>% evaluate(X_test, y_test)
# To make predictions on a new subset of data, you can use the predict_classes() function as shown below:
model %>% predict_classes(X_test)
