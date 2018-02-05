library(ggplot2)
x = seq(from = -5, to = 5, length=100)
y = dnorm(x = x, mean = 0, sd = 1)
df=data.frame(x=x,y=y)
p=ggplot(data=df,aes(x=x, y=y)) + geom_line()+ ggtitle("Standard normal pdf")
p

n = 10000
x = rnorm(n = n, mean = 0, sd = 1)
q=ggplot(data.frame(x=x),aes(x))+geom_histogram()
q
X = 5