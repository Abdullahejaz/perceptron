\name{perceptron}
\alias{perceptron}

\title{
Perceptron to do classification
}
\description{
The Perceptron is a simple learning algorithm designed by Frank Rosenblatt.
#' It take in training dataset and eventually learn to identify the correct classes based on the weight of the object
}
\usage{
perceptron(iris_sub_df, rate_learn, epochs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{iris_sub_df}{
It is the subset dataframe that contains 100 rows with three columns
}
  \item{rate_learn}{
It is the learning rate
}
  \item{epochs}{
It is the number of epochs
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned values are the weight vectors.
}

\author{
Abdullah Ejaz
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (iris_sub_df, rate_learn, epochs)
{
    png("plotIrisData.png")
    plot_iris_sub = ggplot(iris_sub_df, aes(x = sepal_Length,
        y = sepal_Width)) + geom_point(aes(colour = species,
        shape = species), size = 3) + xlab("sepal Length") +
        ylab("sepal Width") + ggtitle("Species vs sepal Length and sepal Width")
    print(plot_iris_sub)
    dev.off()
    iris_sub_df[, 4] <- 1
    iris_sub_df[iris_sub_df[, 3] == "setosa", 4] <- -1
    head(iris_sub_df, n = 100)
    x_training_data <- iris_sub_df[, c(1, 2)]
    y <- iris_sub_df[, 4]
    head(x_training_data, n = 5)
    head(y, n = 5)
    weight <- rep(0, dim(x_training_data)[2] + 1)
    error <- rep(0, epochs)
    for (j in 1:epochs) {
        for (i in 1:length(y)) {
            z <- sum(weight[2:length(weight)] * as.numeric(x_training_data[i,
                ])) + weight[1]
            if (z < 0) {
                y_Classifier <- -1
            }
            else {
                y_Classifier <- 1
            }
            difference_weight <- rate_learn * (y[i] - y_Classifier) *
                c(1, as.numeric(x_training_data[i, ]))
            weight <- weight + difference_weight
            if ((y[i] - y_Classifier) != 0) {
                error[j] <- error[j] + 1
            }
        }
    }
    print(weight)
    return(error)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
