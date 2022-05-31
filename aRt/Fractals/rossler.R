
####################
# Rossler Attractor 
####################

#dx / dt = - y - z
#dy / dt = x + a y
#dz / dt = b + z ( x - c )
#
#where a = 0.2, b = 0.2, c = 5.7

rossler<-function(n=2000,a=.2,b=.2,c=1.7,x0=0.0001,y0=0.0001,z0=0.0001){
    x<-c(x0,rep(NA,n-1))
    y<-c(y0,rep(NA,n-1))
    z<-c(z0,rep(NA,n-1))
    h<-0.015
    for (i in 2:n){
        x[i]<-x[i-1]-h*(y[i-1]+z[i-1])
        y[i]<-y[i-1]+h*(x[i-1]+a*y[i-1])
        z[i]<-z[i-1]+h*(b+z[i-1]*(x[i-1]-c))
    }
    require(rgl)
    rgl.clear()
    rgl.points(x,y,z, color=heat.colors(n), size=1)
}

rossler(2000,x0=3,y0=4,z0=.4)