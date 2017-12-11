
library(ggplot2)
library(gridExtra)

# load iris data set
data(iris)

# Subset of iris data frame,
# The first 100 rows contains only species versicolor and setosa data
iris_sub_df <- iris[1:100, c(1, 2, 5)]

names(iris_sub_df) <- c("sepal_Length", "sepal_Width", "species")

head(iris_sub_df, n=5)

perceptron <- function(iris_sub_df, rate_learn, epochs) {

  png("plotIrisData.png")

  plot_iris_sub = ggplot(iris_sub_df, aes(x = sepal_Length, y = sepal_Width)) +
    geom_point(aes(colour=species, shape=species), size = 3) +
    xlab("sepal Length") +
    ylab("sepal Width") +
    ggtitle("Species vs sepal Length and sepal Width")

  print(plot_iris_sub)
  dev.off()

  # Assigning binary values
  # 1 to setosa (1:50 rows)
  iris_sub_df[, 4] <- 1

  # -1 to versicolor(51:100 rows)
  iris_sub_df[iris_sub_df[, 3] == "setosa", 4] <- -1

  head(iris_sub_df,n=100)
  x_training_data <- iris_sub_df[, c(1, 2)]
  y <- iris_sub_df[, 4]


  head(x_training_data, n=5)

  head(y, n=5)


  # initialize weight vector
  weight <- rep(0, dim(x_training_data)[2] + 1)
  error <- rep(0, epochs)


  # loop for number of epochs
  for (j in 1:epochs) {

    # loop for training data set
    for (i in 1:length(y)) {

      # Input,z to Activationfunction
      z <- sum(weight[2:length(weight)] *
                 as.numeric(x_training_data[i, ])) + weight[1]

      # based on value of z value of y_Classifier is set
      if(z < 0) {
        y_Classifier <- -1
      } else {
        y_Classifier <- 1
      }

      # If y_Classifier = y_Target do nothing, i.e there is no change in weight
      # Else calculate the weigth difference.
      difference_weight <- rate_learn * (y[i] - y_Classifier) *
        c(1, as.numeric(x_training_data[i, ]))

      # Update the weight vector
      weight <- weight + difference_weight

      # Update error function
      if ((y[i] - y_Classifier) != 0.0) {
        error[j] <- error[j] + 1
      }

    }
  }

  # weight to decide between the two species
  print(weight)
  # png("errorPlot.png")
  # plot(1:10, err, type="l", lwd=2, col="red", xlab="Nos of epoch", ylab="errors")
  # title("Errors vs epoch - learningRate = 0.5")
  # dev.off()
  return(error)
}


# Invoke the function perceptron.
err <- perceptron(iris_sub_df, 0.5, 10)

plot(1:10, err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch - learning rate = 0.5")


