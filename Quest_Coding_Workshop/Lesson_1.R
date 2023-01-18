# Write whatever you want

#Vectors 

#Vectors are a basic data structure in RStudio. <- will assign a variable and create a vector.

a<- 0

a<-"Character" # character type 

a<- 8>9 #Logical/Boolean 

class(a) #Tells you what class your variable is

a<-as.integer(2.4)
a<-c(1,4,5,2,1,2)
#You can also concatenate vectors together using the paste() function
b<-c(2,5,1,2,3,2)

paste(a,b)

#Ways to make easier vectors: 1:N, or seq() functions 

x<-1:30

b<-seq(1,20, by=3)

b[2]

q<-b[c(1,3,1)]
rm(q) #remove an object

#remove elements by using the - sign 
x<-x[-2]
x[c(-1,-5)]

x<-c(a=1,b=2,c=3)
x
x[c("a","b")]

#Could also use logical operators to turn elements "on" or "off" 
x[c(TRUE, FALSE, FALSE)]

#Conditional operators
# & represents "and" 
# | represents "or" 
# ! represents "not" 

# 1. Create a vector object of length 12 that has all the month names as elements in order. 
month<-c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

# 2. Use the bracket operator [] to display the 3rd through 5th month. 
months2<-month[3:5]
months2<-month[c(3,4,5)]
#Logical Operators 
5>3 
5>=5 
5==3
5!=3
5>3 & 1!=2
1==2 & 1!=2
# 3. Display the above vector object in reverse order. 
months3<-month[5:3]
months3
months3<-months2[3:1]
months3
# 4. Create a boolean vector that displays whether a given element of the month vector is equal to May 
bool1<-month=="may"
bool1

# 5. Use help command on the rnorm function to learn how to generate a vector of 10 random numbers from a random variable with a normal distribution with mean 6 and sd of 3. 
set.seed(1000) #Standardize your results
rand1<-rnorm(10, mean=6, sd=3)

# 6. Use the functions mean, sd, and sqrt to calculate the mean and standard error of the mean for the random variables from question 5. Formula for standard error of the mean is on the board. 
mean1<-mean(rand1)
standerr<-sd(rand1)/sqrt(10)


# Data Structures

#Matrix-a two dimensional array since it'll hold rows and columns. matrix()

matrix(ncol=4, nrow=4)

matrix(data=1:16, ncol=4, nrow=4,byrow=TRUE)

#cbind() and rbind()
a<-c(1,2,3)
b<-c(4,5,6)
m<-cbind(a,b)
m #to rename columns you should use colnames()


#List Created using the list() function 

x<-list("here's a list item", c(1,2,3,4,5))
x


#Data frames-similar to spreadsheets with rows/columns, and column variables can have different data types. Can use read.csv() function to create a dataframe from a particular spreadsheet. 
x<-read.csv("Test_data.csv")


#Subsetting matrices requires you to specify both the rows and columns, in that order. 
vec<-1:12
m<-matrix(data=vec, nrow=4)
m

m[2,3]

m[2:3, 2:3]

m[2,3, drop=FALSE]
m[,2 ]
m[,2, drop=FALSE]

#Subsetting Dataframes 
varX<-1:12
varY<-rep(c("Low", "Med", "High"), each=4) #rep() repeats/replicates
varZ<-runif(12)
df<-data.frame(varX,varY,varZ)

rand<-list(varX=22, varY="Low", varZ=0.223)

newframe<-rbind(df,rand)
print(newframe)
str(newframe)

#we can also add a new column 
varA<-runif(13)
newframe<-cbind(newframe, varA)


#Subsetting Lists 

newlist<-list(1:10, matrix(nrow=4, 1:8, byrow=TRUE), letters[1:3], pi)

newlist[2]
newlist[c(1,2,3)]

#[[]] will be the contents within the elements. 
newlist[[2]][3,2]

newlist<-list(val=1:10, mat=matrix(nrow=4, 1:8, byrow=TRUE), alph=letters[1:3], pie=pi)

newlist$mat

#unlist() will unfurl the list

unlisted<-unlist(newlist)
unlisted


# Cleaning Data
DF<-data.frame(x= c(1,2,3), y=c(0,10,NA))
DF
#complete.cases() function 
complete <- complete.cases(DF$y)
complete
clean <- DF[complete.cases(DF$y), ]
clean
which(!complete.cases(DF$y))

? na.omit


#Control structures ifelse, for and while loops
lions <- 20
tigers <- 15
lions<-sample(1:100,1)
tigers<-sample(1:100, 1)
if (lions > tigers) {
  print("Lions win!")
  print(lions)
} else{
  print("Tigers win!")
  print(tigers)
} 

ifelse()


#Iteration for() loops. Structure is for(x in y){action}

for(i in 1:10){
  print(i)
}

#Nested for loops 
for(i in 1:10){
  for (j in letters[1:5]){
    print(paste(i, j))
  }
}

#While loops. while(conditional){action}
z<-1
while(z>0.1){
  z<-runif(1)
  cat(z,"\n")
}

#Functions 
airTemp<-c(32, 35, 37, 39, 40, 32, 36, 31)
Abundance<-c(100, 104, 49, 100, 132, 160, 150, 190)
df<-as.data.frame(cbind(airTemp, Abundance))

grab_stats<-function(x){
  model<-lm(df$airTemp~df$Abundance) #lm(y~x)
  summary(model)
  u
  u<-unlist(summary(model))
  slope<-u$coefficients2
  pval<-u$coefficients8
  print(paste("slope=",slope))
  print(paste("p=value",pval))
}

grab_stats(df)

source(file path)

#Plotting Fundamentals 
data(mtcars)


#Scatter plot 
plot(mtcars$wt, mtcars$mpg, main="Scatter Plot", pch=16, cex=1.3, xlab="Weight", ylab="Miles Per Gallon")+abline(lm(mtcars$mpg~mtcars$wt))


#Bar Graph 
cars<-mtcars[1:6, 1:5]
barplot(cars$mpg, main="Cars", xlab="Car Models", ylab="mpg")

#Histograms 
x<-runif(400, 1, 200)
hist(x, freq=FALSE, col="red")

#Box Plot 
boxplot(mtcars$mpg, col="lightblue", ylab="mpg", xlab="all vehicles", main="Box Plot")


#Logisitic Regression Analysis 
bodysize<-rnorm(20, 30, 2)
bodysize<-sort(bodysize) #sort bodysize by ascending order 
survive<-c(0,0,0,0,0,0, 1,0, 1,1,0,0,0,1,0,1,1,1,1,1)


n<-20
survive<-sample(c(0,1), replace=TRUE, size=n)
dat<-as.data.frame(cbind(bodysize, survive))
dat

plot(bodysize, survive, xlab="Body Size", ylab="Prob. of survival")

g<-glm(survive~bodysize, family=binomial, dat)

curve(predict(g, data.frame(bodysize=x), type="resp"), add=TRUE)

points(bodysize, fitted(g), pch=20)




