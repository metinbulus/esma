\name{databahn}
\alias{databahn}


\title{Databahn Plots}

\description{
Creates alternative funnel plots with ggplot2 themes, but call them databahn plots. The name was suggested by Eren Can Aybek. 
}

\usage{
databahn(yi,
         sei, 
         tau2 = 0, 
         title = NULL, 
         xlab = "Mean Difference",
         ylab = "Standard Error",
         ci = c("normal", "t"),
         point.fill = "#ff8da1",
         point.color = "#3c556e",
         point.shape = 21,
         point.size = c("weights", "se", "none"),
         base.size = 12, 
         theme = c("bw", "test", "grey", "gray", "dark")) 
}

\arguments{
  \item{yi}{Effect sizes}
  \item{sei}{Standard errors}
  \item{tau2}{Estimated amount of total heterogeneity}
  \item{title}{Plot title}
  \item{xlab}{x-axis label}
  \item{ylab}{y-axis label}
  \item{ci}{Type of confidence intervals: c("normal", "t")}
  \item{point.fill}{Fill color for points}
  \item{point.color}{Border color for points}
  \item{point.shape}{Shape of the points}
  \item{point.size}{Size of the points: c("weights", "se", "none")}
  \item{base.size}{Base font size}
  \item{theme}{A ggplot theme: c("bw", "test", "grey", "gray", "dark")}
}

\examples{

n <- 30 # sample size per group
k <- 30 # number of studies
mu <- 0.20 # average effect size (or estimate)
tau2 <- 0.75 # effect heterogeneity for simulation

set.seed(123)
n1 <- rpois(k, n) # sample size for group 1
n2 <- rpois(k, n) # sample size for group 2
sd1 <- abs(rnorm(k)) # standard deviation for group 1
sd2 <- abs(rnorm(k)) # standard deviation for group 2
yi <- rnorm(k, mu, tau2) # mean difference (raw)
sei <- sqrt(sd1^2 / n1 + sd2^2 / n2) # standard error (Welch)


databahn(yi = yi, 
         sei = sei, 
         point.fill = "white",
         theme = "dark",
         point.size = "weights",
         title = "Databahn Plot")

databahn(yi = yi, 
         sei = sei, 
         theme = "test",
         point.size = "weights",
         title = "Databahn Plot")
}
