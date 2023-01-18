%
%	NBSPACE
%
%	matlab code to iterate an enhanced Nicholson-Bailey model
%	for host and parasite, with each capable of random (diffusive)
%	movement.
%
%	The model itself looks like this:
%	Intermediate variables (*) are created  using a discrete
%	enhanced N-B step:
%       
%		h* = h(t) exp[ r (1 - h(t)/k) - a p(t) ]
%		p* = n h(t) ( 1 - exp[- a p(t) ] )
%	
%	This population is then allowed to disperse using a spatial
%	convolution, * below, (like a long step of  a heat equation):
%	
%		h(t+1) = p_h(x,y) * h*
%		p(t+1) = p_p(x,y) * p*
%	
%	p_h and p_p can be any probability functions representing the
%	dispersal of the two species; I have used Gaussians below.
%
%	Parameters appearing (and their interpretation):
%
%		r	reproduction rate of host  species
%		k	carrying capacity of host species
%		a	encounter rate or effectiveness of parasite on host
%		n	parasites produced by a successful infestation
%	
%	Parameters used for the code:
%
%		np	number of points in x and y directions (a power of 2)
%		xl	length of domain in x and y directions
%		dx	grid spacing
%		dt	a time-step length
%		mu	followed by h and p -- diffusion parameter for each
%				species
%		ngens 	number of steps to run the code (number of 
%			generations
%
%
%

%
%	set parameters and spatial grids
% 
np=128; mup=.02; muh=.02; dt=.5; xl=10; dx=2*xl/np;
x=linspace(-xl,xl-dx,np);
y=x;
[X,Y]=meshgrid(x,y);
ngens=100;

%
%	set up spatial parameters
%

n=ones(np);
a=4*ones(np);
r=1.75*ones(np);
k=ones(np);
 %	    k=.01*ones(np)+(-.25<=X & X<=.25)+(abs(X)>=1.75 & abs(X)<=2.25)...
 %	                              +(abs(X)>=3.75 & abs(X)<=4.25)...
 %	                              +(abs(X)>=5.75 & abs(X)<=6.25)...
 %	                              +(abs(X)>=7.75 & abs(X)<=8.25)...
  %	                             +(abs(X)>=9.75 & abs(X)<=10.25);
kmax=max(max(k)); 


%
%	Set up stuff for movie
%
M=moviein(ngens); %	If you want movies

%
%	Set up initial conditions
% 

p0=.5*(1+cos(.5*pi*X/xl+pi/4*rand(np)).*sin(pi*Y/xl-pi/2*rand(np)));
h0=.5*rand(np).*(1+cos(4*pi*sqrt(X.^2+Y.^2)/xl));

%
%	Define movement kernels for host and parasite	
% 

hhker=exp(-(X.^2+Y.^2)/(4*muh*dt))/(4*pi*dt*muh)*dx^2;
Fhker=fft2(hhker);	%	FFT is taken because we will use it often,
			% there are two factors of dx; one for each dimension
pker=exp(-(X.^2+Y.^2)/(4*mup*dt))/(4*pi*dt*mup)*dx^2;
Fpker=fft2(pker);	%	FFT is taken because we will use it often
			% there are two factors of dx; one for each dimennsion

%
%	Now we are in a position to iterate for
% 	a number of generations equal to ngens
%
for j=1:ngens

%
%	Advance the life cycles using adjusted N-B model
%

hn=h0.*exp(r.*(1-h0./k)-a.*p0);
pn=n.*h0.*(1-exp(-a.*p0));

%
% 	Do the dispersal step
%

  fhn=fft2(hn);		%	First take fft of each species 
  fpn=fft2(pn);		%	First take fft of each species 

%
%	Now do the convolutions.
%	The fftshift serves to center the probability functions.
%
  h0=real(fftshift(ifft2(Fhker.*fhn)));
  p0=real(fftshift(ifft2(Fpker.*fpn)));

%
%	Now plot the solution
%

pcolor(X,Y,h0-p0), shading flat, colormap(hot), axis square, colorbar
%subplot(1,2,1)
%pcolor(X,Y,h0),shading flat, colormap hot, title('Host'),colorbar
%subplot(1,2,2)
%pcolor(X,Y,p0),shading flat, colormap jet, title('Parasite'),colorbar

M(:,j) = getframe; %	If you want movies

end
%	
%	Here ends the iteration
%

%surf(X,Y,h0-p0), shading interp
%  
%  To play the movie,  use this command:
%  movie(M,2,8)
