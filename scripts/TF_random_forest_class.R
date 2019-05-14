library(tensorflow)
library(reticulate)


#----------------------------------------- import modules 


np <- import("numpy", convert = FALSE)

py <- import_builtins(convert = FALSE)

learn <- tf$contrib$learn

random_forest <- tf$contrib$tensor_forest$client$random_forest

tensor_forest <- tf$contrib$tensor_forest$python$tensor_forest


# ---------------------------------------- define parameters


flags <- tf$app$flags
flags$DEFINE_float("learning_rate", 0.01, "Initial learning rate.")
flags$DEFINE_integer("max_steps", 1000L, "Number of steps to run trainer.")
flags$DEFINE_integer("num_trees", 100L, "Number of trees.")
flags$DEFINE_integer("max_nodes", 1000L, "Maximum number of nodes.")
flags$DEFINE_integer("batch_size", 100L, "Batch size.")
flags$DEFINE_integer("num_classes", 10L, "Number of classes.")
flags$DEFINE_integer("num_features", 784L, "Number of features.")
FLAGS <- parse_flags()


# ---------------------------------------- load data 


mnist <- learn$datasets$load_dataset("mnist")


# ---------------------------------------- pre processing


parms <- tensor_forest$ForestHParams(num_trees = FLAGS$num_trees, 
                                     max_nodes = FLAGS$max_nodes, 
                                     num_classes = FLAGS$num_classes, 
                                     num_features = FLAGS$num_features)

est <- learn$SKCompat(random_forest$TensorForestEstimator(params = parms, 
                                                          model_dir = "TF"))

train_data <- np$asarray(mnist$train$images, dtype = np$float32)
train_labels = np$asarray(mnist$train$labels, dtype = np$int32)

test_data <- np$asarray(mnist$test$images, dtype = np$float32)
test_labels <- np$asarray(mnist$test$labels, dtype = np$int32)

metric_name <- "accuracy"

metric <- dict("accuracy" = learn$MetricSpec(
  metric_fn = tf$contrib$metrics$streaming_accuracy,
  prediction_key = random_forest$eval_metrics$get_prediction_key(metric_name)))


# ---------------------------------------- run things 


# fit the model
model_fit <- est$fit(x = train_data, y = train_labels, batch_size = FLAGS$batch_size, max_steps = FLAGS$max_steps)

# get accuracy
train_eval <- est$score(x = train_data, y = train_labels, batch_size = FLAGS$batch_size, steps = 1, metrics = metric)

test_eval <- est$score(x = test_data, y = test_labels, batch_size = FLAGS$batch_size, metrics = metric)

# make predictions
preds <- est$predict(x = test_data)

init <- tf$global_variables_initializer()
sess <- tf$Session()
sess$run(init)

sess$run(list(model_fit,
              train_eval,
              test_eval,
              preds))


# ---------------------------------------- How to `read` the preds object


# Premise: the test dataset consists of 10,000 images, each one composed of 784 pixels 

np$shape(test_data)

# The aim of the RF classifier is to classify each one of the 10,000 images into a 0-9 digit (`classes`) 
# Precisely, the RF classifier assigns a probability that each one of the 10,000 images is a 0-9 digit 

dim(preds$classes)
dim(preds$probabilities)

# So let's see what is the RF prediction for the first image

preds$classes[1]
preds$probabilities[1,]

# check that the prediction is actually true
py_to_r(test_labels)[1]
