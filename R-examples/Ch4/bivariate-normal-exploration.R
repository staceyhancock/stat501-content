# lets first simulate a bivariate normal sample
library(MASS)
bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, sqrt(1)*sqrt(15)*.9, sqrt(1)*sqrt(15)*.9, 15), 2, 2))
bivn2 <- mvrnorm(10000, mu = c(5, 0), Sigma = matrix(c(1, sqrt(1)*sqrt(15)*.25, sqrt(1)*sqrt(15)*.25, 15), 2, 2))

# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 100)
bivn.kde2 <- kde2d(bivn2[,1], bivn[,2], n = 100)

# fancy perspective
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)
par(new=TRUE)
persp(bivn.kde2, phi = 45, theta = 30, shade = .1, border = NA)

#install.packages("rgl")
require(rgl)

col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
col2 <- rainbow(length(bivn.kde2$z))[rank(bivn.kde2$z)]
open3d()
persp3d(x=bivn.kde, col = col1)
open3d()
persp3d(bivn.kde2, col=col2,surface3d(x,y,z, color = col2))

