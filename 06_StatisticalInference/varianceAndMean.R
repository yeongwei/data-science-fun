# Functions
dieRow <- function(numOfRow) { 
  sample(c(1,2,3,4,5,6), numOfRow, replace = TRUE)
};

biasedVariance <- function(randomVariable) {
  sum((randomVariable - mean(randomVariable))^2) / length(randomVariable)
};

unbiasedVariance <- function(randomVariable) {
  sum((randomVariable - mean(randomVariable))^2) / (length(randomVariable) - 1)
};

run <- function(numOfAttempts, numOfRow, print = TRUE) {
  DATA <- data.frame(Mean = c(), Variance = c(), NumberOfRows = c(), NumberOfAttempts = c());
  
  for (m in numOfAttempts) {
    for (n in numOfRow) {
      
      sample_ <- matrix(dieRow(n * m), m);
      mean_ <- rowMeans(sample_);
      biasedVariance_ <- apply(sample_, 1, function(eachRow) { biasedVariance(eachRow) });
      data_ = cbind(mean_, biasedVariance_, n, m);
      DATA <- rbind(DATA, data_);
      
    }
  }
  
  colnames(DATA) <- c("Mean", "Variance", "NumberOfRows", "NumberOfAttempts");
  
  library(ggplot2)
  PLOT <- ggplot(DATA, aes(x = Mean, y = Variance, color = NumberOfRows));
  PLOT <- PLOT + geom_point() + facet_grid(NumberOfAttempts ~ NumberOfRows);
  if (print) {
    print(PLOT);  
  } else {
    str(DATA); 
  }
}

# numOfAttempts aka ???
# numOfRow aka Size of Sample
run(c(100, 1000), seq(from = 100, to = 500, by = 100));
