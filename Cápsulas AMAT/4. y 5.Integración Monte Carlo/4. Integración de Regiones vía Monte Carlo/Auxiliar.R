
set.seed(7)
mean(rexp(n = 3,rate = 1/10))



v<-c(1,1,1,1,2,2,3,4,5,5,5,6,6)
MASS::truehist(v,col="red",xaxt='n',nbins = 6)


set.seed(17)
n=1000
X<-rnorm(n)
MASS::truehist(X,col="skyblue")
abline(h=0,v=0,col="blue",lwd=2)
points(X,rep(0,length(X)),pch=3,col="orange")
  