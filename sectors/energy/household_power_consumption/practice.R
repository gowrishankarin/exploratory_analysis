myHist <- function(mu) {
    g <- ggplot(galton, aes(x = child))
    g <- g + geom_histogram(fill = "salmon", binwidth = 1, 
                            aes(y = ..density..), colour = "black")
    g <- g + geom_density(size = 2)
    g <- g + geom_vline(xintercept = mu, size = 2)
    
    mse <- round(mean((galton$child - mu)^2), 3)
    
    g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse ))
    g
}

coinPlot : function(n){
    means <- cumsum(sample(0 : 1, n , replace = TRUE)) / (1  : n)
    g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
    g <- g + geom_hline(size=1.5 ,yintercept = 0.5,alpha=0.6,
                        linetype="longdash") + geom_line(size = 1)
    if(n<100){
        g <- g + geom_point(colour="red",size=3,alpha=0.8)
    }	 
    g <- g + labs(x = "Number of obs", y = "Cumulative mean")
    g <- g + scale_x_continuous(breaks=seq(0,n+1,ceiling(n/10)))
    print(g)
    invisible()
}

# The t distribution, invented by William Gosset in 1908, has thicker tails than the normal. Also, instead of having
#| two parameters, mean and variance, as the normal does, the t distribution has only one - the number of degrees of
#| freedom (df).

#| As df increases, the t distribution gets more like a standard normal, so it's centered around 0. Also, the t
#| assumes that the underlying data are iid Gaussian so the statistic (X' - mu)/(s/sqrt(n)) has n-1 degrees of
#| freedom.

#| To see what we mean, we've taken code from the slides, the function myplot, which takes the integer df as its input
#| and plots the t distribution with df degrees of freedom. It also plots a standard normal distribution so you can
#| see how they relate to one another.

myplot : function(df){
  d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                  x = xvals,
                  dist = factor(rep(c("Normal", "T"), c(k,k))))
  g <- ggplot(d, aes(x = x, y = y)) 
  g <- g + geom_line(size = 2, aes(colour = dist))
  print(g)
}

#| > myplot(2)

#| You can see that the hump of t distribution (in blue) is not as high as the normal's.
#| Consequently, the two tails of the t distribution absorb the extra mass, so they're
#| thicker than the normal's. Note that with 2 degrees of freedom, you only have 3 data
#| points. Ha! Talk about small sample sizes. Now try myplot with an input of 20.


myplot2 : function(df){
    d <- data.frame(n= qnorm(pvals),t=qt(pvals, df),
                    p = pvals)
    g <- ggplot(d, aes(x= n, y = t))
    g <- g + geom_abline(size = 2, col = "lightblue")
    g <- g + geom_line(size = 2, col = "black")
    g <- g + geom_vline(xintercept = qnorm(0.975))
    g <- g + geom_hline(yintercept = qt(0.975, df))
    print(g)
}
