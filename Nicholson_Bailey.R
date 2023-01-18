library(ggplot2)
library(plotly)
library(gridExtra)
library(ggpubr)
#Nicholson-Bailey Model

#H[t]=density of host species in generation t
#P[t]=density of parasitoid species in generation t
#a=per capita searching efficiency of parasitoid
#R=host reproductive rate
#c=average # of viable eggs laid by parasitoid on a single host
#Tets. known to disperse into control sites >5km from other established sites, at least 0.3 km/year, half dispersal rates > 2 km/year
#Average brood of 44 eggs per EA, up to 108, and four generations per year

#Basic Model
NBail<- function(R, a, c, T=100, H0=10, P0=1){
  
  H=rep(NA,T) #T=length of simulation time series
  P=rep(NA,T)
  
  H[1]=H0
  P[1]=P0
  
  for(t in 2:T){
    H[t]=R*H[t-1]*exp(-a*P[t-1])
    P[t]=c*H[t-1]*(1-exp(-a*P[t-1]))
  }
  res=list(H=H, P=P)
  return(res)
}

#Plots of model simulation values 
sim<-NBail(R=1.0, a=1.0,c= 1.0, H0 = 50, P0=5)
simdata<-data.frame(H=sim$H, P=sim$P, Generation=1:500)
ggplot(data=simdata, mapping = aes(x=Generation, y=H))+geom_point()+geom_point(aes(y=P),color="blue") #Blue=parasitoid, black=Hosts

# Including Density Dependence Model
#K=carrying capacity of the host, creating a density dependence of parasitoids and hosts on the capacity of the system

DensityNB<- function(R, a, c, K, T=100, H0=10, P0=1){
  
  H=rep(NA,T) #T=length of simulation time series
  P=rep(NA,T)
  
  H[1]=H0
  P[1]=P0
  
  for(t in 2:T){
    H[t]=H[t-1]*exp(R*((1-H[t-1])/K)-a*P[t-1])
    P[t]=c*H[t-1]*(1-exp(-a*P[t-1]))
  }
  res=list(H=H, P=P)
  return(res)
}

#NB Model with Coexistence
sim1<-DensityNB(R = 0.8, a = 1.0, c = 1.0, K = 3.0, T=100)
simdata1<-data.frame(H=sim1$H, P=sim1$P, Generation=1:100)
a<-ggplot(data=simdata1, mapping = aes(x=Generation, y=H))+geom_point()+geom_point(aes(y=P),color="blue")+geom_line()
a


#NB Model with damped oscillations to coexistence
sim1<-DensityNB(R = 1.8, a = 1.0, c = 1.0, K = 3.0, T=100)
simdata2<-data.frame(H=sim1$H, P=sim1$P, Generation=1:100)
b<-ggplot(data=simdata2, mapping = aes(x=Generation, y=H))+geom_point()+geom_point(aes(y=P),color="blue")+geom_line()
b



#NB Model with parasitoid extinction
sim1<-DensityNB(R = 1.8, a = 1.0, c = 1.0, K = 1.0, T=100)
simdata3<-data.frame(H=sim1$H, P=sim1$P, Generation=1:100)
c<-ggplot(data=simdata3, mapping = aes(x=Generation, y=H))+geom_point()+geom_point(aes(y=P),color="blue")+geom_line()
c

ggarrange(a,b,c, labels=c("i", "ii", "iii"))

#Nicholson-Bailey by changing the probability distribution of the functional response from the zero term of the Poisson distribution to the zero term of the negative binomial distribution, and non-trivial stability within the system is obtained. This led to other models that included both density-dependent aggregration (DDA), density-independent aggregrative (DIA), and the so-called CV^2 > 1 rule that states the if the coefficient of variation squared exceeds one, then both forms of aggregration stabilize the interactions.
#Include spatial patchiness model/spatial heterogeneity
#Two "phases": Dispersal phase + Parasitism phase

#Set up landscape and dispersal
#     p(x)    -       population density in x
#      K(x)    -       probability of individual dispersal to location
#                          x starting at location 0.
#       xl	-       maximum x coordinate (also - minimum)
#       dx	-       spatial step  delta x = 2*xl/np
#       np	-       number of points  in discretization for x
#                         (should be a power of 2 for  efficiency of FFT)
#       c	-       center of spatial  dispersal
#       D	-       diffusion constant (per step)
#       r	-       per-capita growth constant
np<-128
xl<-5
dx<-2*xl/np
c<-.4
D<-.25
r<-1.15
nsteps=10

#discretize space, define an initial population distribution

x<--xl+(0:np-1)*dx
p<-(abs(x+3)<=.5)
p0<-p

# define a dispersal kernel
K<-exp(-(x-c)^2/(4*D))/sqrt(4*pi*D)

# normalize K to make it have integral one

K<-K/(dx*sum(K))


#calculate the fft of K, multiplying by dx to account for the additional factor of np and converting from a interval length of 1 to 2*xl.  The fftshift accounts for using an interval of (-xl,xl) as opposed to (0,2*xl).
fK<-fft(fftshift(K))*dx

for(i in 1:nsteps){
  fp=fft(p);
  fg=fK*fp;
  g=r*abs(ifft(fg));
  plot(x=x,y=p0)
}

#Reproduction/Parasitism Phase

#Introduce multiple parasitoid species? Model superparasitism? Some subset of the parasitized hosts are getting double/triple parasitized. There is evidence that Tetrastichus has a decreased fitness in progeny due to superparasitism-more progeny will appear, per host, but their body and ovipositor length were shorter overall, and the sex ratio was more male-biased. This could have a significant effect on their population dynamics given different introduction scenarios.

#Adjust searching efficiency to change with predator density-doing this may increase stability. Hassell and Varley 1969
#a=QP^-m


#Stabilizing Mechanisms: 
#Refuges
#Size/stage structure-some escape in TIME that protects the host from parasitoids. Some fraction of population is going to get old enough to survive and maintain the population.

#Metapopulation dynamics-Patches in the environment provide temporary refuges. Relaxes the assumption that the system is closed in the NB model. Having refuges in space that have not been discovered by the natural enemy can help them persist for a longer period of time. 


#Build multiple models with each factor/change, and then compare with experimental/field data and judge model accuracy/application


library(matconv)
#NB-IDE Model
H[t]=R*H[t-1]*exp(-a*P[t-1])
P[t]=c*H[t-1]*(1-exp(-a*P[t-1]))


out<-mat2r(inMat = "NB_Model.m")
out$rCode
