#1
a = 0
b = 0

defective_rates <- c(0,0,0)
prods <- c(0,0,0)
prod_def <- c(0,0,0)

#Input of product values
repeat{ 
  for(x in 1:3){
    repeat{
      prods[x] <- as.numeric(readline(paste("Input value for product ", x, " (0.1 <= input <= 0.4): ")))
      if(prods[x]>=0.1 && prods[x]<=0.4){ 
        break
      }
      print("Input between 0.1 to 0.4 only") 
    }
  }
  a <- sum(prods)
  if(a==1){ 
    break
  }
  print("Summation of products") 
}
repeat{ 
  for(x in 1:3){
    repeat{
      defective_rates[x] <- as.numeric(readline(paste("Input value for defective rate ", x, " (0.01 <= input <= 0.05): ")))
      if(defective_rates[x]>=0.01 && defective_rates[x]<=0.05){
        break
      }
      print("Input between 0.01 to 0.05 only") 
    }
  }
  b <- sum(defective_rates)
  if(b ==0.12){ 
    break
  }
  print("The Summation of defective rates must be 0.12") 
}

for(x in 1:3){ 
  cat("Product ", x, " Percentage: ", prods[x], "; Defective Rate: ", defective_rates[x], "\n")
  prod_def[x] <- prods[x]*defective_rates[x]
}
prod_def[1]
prod_def[2]
prod_def[3]

ran_def <- sum(prod_def)
ran_def
cat("Random selected product of DEFECTIVE is ", ran_def)

#2
x <- c(0)
y <- c(0)
repeat{
  xN <- as.numeric(readline("Enter 1st value (x): "))
  yN <- as.numeric(readline("Enter 2nd value (y): "))
  
  xV <- numeric(yN)
  xP <- numeric(yN)
  yV <- numeric(yN)
  yP <- numeric(yN)
  
  xyP <- matrix(numeric(xN*yN), nrow = xN, ncol = yN)
  
  for (i in 1:xN) {
    repeat{
      xV[i] <- as.numeric(readline(paste("Enter x value ", i, ": ")))
      xP[i] <- as.numeric(readline(paste("Enter value for x probability ", i, " between 0 and 1: ")))
      if (xP[i]>=0 && xP[i]<=1) { 
        break
      }
      print("within 0 and 1 only!") 
    }
  }
  
  for (i in 1:yN) {
    repeat{
      yV[i] <- as.numeric(readline(paste("Enter y value", i, ": ")))
      yP[i] <- as.numeric(readline(paste("Enter value for y probability ", i, " between 0 and 1: ")))
      if (yP[i]>=0 && yP[i]<=1) { 
        break
      }
      print("within 0 and 1 only!") 
    }
  }
  if(sum(xP)+sum(yP)==1){
    break
  }
  print("Sum of x and y must be 1")
}

xmgn <- colSums(xyP)
ymgn <- rowSums(xyP)

meanX <- sum(xV * xmgn)
meanY <- sum(yV * ymgn)

xVR <- sum((xV - meanX)^2 * xmgn)
yVR <- sum((yV - meany)^2 * ymgn)

library(ggplot2)
pdf <- data.frame(x = xV, y = xP)
cdf <- data.frame(x = xV, y = cumsum(xP))

#For Pdf
pdfp<- ggplot(pdf, aes(x, y)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.5) +
  ggtitle("(PDF)") +
  xlab("Values of x") +
  ylab("Probability")
pdf_plot

#For Cdf
cdf_plot <- ggplot(cdf_data, aes(x, y)) +
  geom_line(color = "blue", linewidth = 1) +
  ggtitle("(CDF)") +
  xlab("Values of x") +
  ylab("Cumulative Probability")
cdf_plot

cat("first value of Marginal distribution(x):\n")
print(data.frame(xV, xP, xmgn))
cat("\nsecond value of Marginal distribution(y):\n")
print(data.frame(yV, yP, ymgn))

# 3
p <- 0.6

# Function to simulate a search until the key phrase is found
search_until_found <- function(p) {
  n_searches <- 0
  found <- FALSE
  while (!found) {
    n_searches <- n_searches + 1
    found <- runif(1) < p
  }
  n_searches
}

# Generate a sample of 10,000 random variables
set.seed(123)
n_simulations <- 10000
simulations <- replicate(n_simulations, search_until_found(p))

# Calculate mean and variance of the SIMULATED distribution
mean_simul <- mean(simulations)
var_simul <- var(simulations)

# Plot the pdf
hist(simulations, prob = TRUE, breaks = seq(0.5, max(simulations)+0.5, by = 1),
     main = "Simulated PDF of Searches until Key Phrase is Found",
     xlab = "Number of Searches", ylab = "Probability")
lines(density(simulations), lwd = 2)

# Calculate the CONDITIONAL distribution 
subset_simulations <- simulations[simulations > 3]
conditional_simulations <- subset_simulations - 3

# Calculate mean and variance of the CONDITIONAL distribution
mean_condi <- mean(conditional_simulations)
var_condi <- var(conditional_simulations)

# Plot the conditional pdf
hist(conditional_simulations, prob = TRUE, breaks = seq(0.5, max(conditional_simulations)+0.5, by = 1),
     main = "Simulated Conditional PDF of Searches until Key Phrase is Found (Given X > 3)",
     xlab = "Number of Searches", ylab = "Probability")
lines(density(conditional_simulations), lwd = 2)

# Markov memory less property
p_x4_given_xgt3 <- sum(simulations == 4 & simulations > 3) / sum(simulations > 3)
p_x1 <- sum(simulations == 1) / n_simulations
p_x5_given_xgt3 <- sum(simulations == 5 & simulations > 3) / sum(simulations > 3)
p_x2 <- sum(simulations == 2) / n_simulations

# Print results
cat("Mean of SIMULATED distribution:", mean_simul, "\n")
cat("Variance of SIMULATED distribution:", var_simul, "\n")
cat("Mean of Simulated CONDITIONAL distribution:", mean_condi, "\n")
cat("Variance of Simulated CONDITIONAL distribution:", var_condi, "\n")