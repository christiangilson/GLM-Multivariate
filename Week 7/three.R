xl = c(0,40); yl = c(-50, 0) ##plot limits

load("digit3.dat.rda")
D = digit3.dat

# flatten
X = rbind(D[,1,],D[,2,]) # we treat X as p by n (not n by p)

# white on black
par(bg = 'black',col.main="white")
for (i in 1:30) {
  plot(matrix(X[,i],13,2),xlim=xl,ylim=yl,type="l",lwd=5,col="white",as=1,main=paste0("data point: ",i)) 
  Sys.sleep(1/2)
}

# Show landmarks clearly
par(bg = 'white',col.main="black")
for (i in 1:3) {
  plot(matrix(X[,i],13,2),xlim=xl,ylim=yl,type="o",as = 1, main="the threes are encoded using landmarks")
  abline(h = 0,lwd=5,lty=2)
  abline(v = 0,lwd=5,lty=2)
  Sys.sleep(3)
}


# Plot the average 3, plus variation around the average.
x.bar = rowMeans(X) # E(3); the "average" 3. 
par(bg = 'black', col.main="Blue")
plot(matrix(x.bar,13,2),xlim=xl,ylim=yl,type="l",as = 1,lwd=5,,col="blue", main = "the average 3") 
for (i in 1:30) {
  lines(matrix(X[,i],13,2),lwd=0.5,col="white") 
  Sys.sleep(1/2)
}

S = var(t(X))       # if x ~ Normal, then (S,x.bar) contains all information about Number 3s.


## PCA ##


A = eigen(S)$vectors  # A: a matrix of principle component loading vectors 

# I also called the columns of A "prototypes of variation", "error templates" and a bunch of other things.

# each column of A is a "way" in which a three can deviate from the typical three.
# the first column a1, is the most salient way
# a2 is the second most salient, subject to the restriction that it is orthogonal to a1
# likewise a3, but orthogonal to both a1 and a2 etc...

lambda = eigen(S)$values # corresponding eigenvalues


par(bg = 'white')
plot(100*lambda/sum(lambda),type="o",col="blue", main="looks like we only have 9 effective dimensions")
abline(h=1)
Sys.sleep(5)

## Looks like there are only really 9 ways for 3's to vary.
## Since there could have been 23, this is where the phrase "dimension reduction" comes from.
## If 3's required all 23 dimensions, then they would have no structure, because in this case each
## landmark would be independent of any other (in fact the x and y components of each landmark would also be independent).

plot.pca = function(lambda,a,x.bar,xl,yl,p) {
  par(bg = 'black',col.main="green")
  hp = round(4*sqrt(lambda))
  range = seq(-hp,hp,length=100)
  for (z in range) {
    three = matrix(x.bar+ z*a,13,2)
    plot(three,xlim=xl,ylim=yl,as=1,main=paste0("principle component #",p),col="white",cex=2,lwd=3)
    Sys.sleep(1/100)
  }
}

# plot the first 9 principle components
for (a in 1:9) {
  plot.pca(lambda[a],A[,a],x.bar,xl,yl,a)
  Sys.sleep(1/2)
}


# plot them individually
a = 1
plot.pca(lambda[a],A[,a],x.bar,xl,yl,a)


# Removing an aspect from the data.

# a1 is interpreted as location on the main (off) diagonal of the page.
# Let's remove this (non interesting) aspect from the threes by setting Z1 = 0.

j = 1 # change this to 2, to delete a2, 3 to delete a3 etc...
for (i in 1:30) {
  my.three.x = X[,i]            # get a three
  my.three.z = t(A) %*% (my.three.x - x.bar)    # X = X.bar + AZ entails Z = A'(X-X.bar)
  my.three.z[j] = 0             # set Zj = 0
  
  my.three.x.standardized = x.bar + A %*% my.three.z  # now transfrom back 

  par(bg = 'black',col.main="white")
  plot(matrix(my.three.x,13,2),xlim=xl,ylim=yl,type="l",lwd=1,col="white",as=1)
  lines(matrix(my.three.x.standardized,13,2),xlim=xl,ylim=yl,lwd=5,col="blue")  
  Sys.sleep(1/2)
}
