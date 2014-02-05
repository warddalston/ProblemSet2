###########################################################
##### Problem Set 2 - Dalston Ward - February 6, 2013 #####
###########################################################

#Start everything by emptying the workspace.  (don't use this if concurrently working on other projects in R!)
rm(list=ls())
#Second, set the working directory as appropriate.  This makes the code cleaner and helps to keep track of any saved files.
setwd("~/Documents/WashU 2nd Year/Applied Stats Programming/Jan 30/PS2/ProblemSet2/")

########### 1. Calculating Violations  ##########

#First, write out several "mini functions" used within the Benford Law Stats function:

# this little function takes as input a vector, and then returns a character vector of only the first element of that vector
IntegerSelector <- function(x){
  x <- sapply(x,gsub,pattern="-",replacement="") #these next three lines are necessary to deal with negatives and decimals! 
  x <- sapply(x,gsub,pattern="0",replacement="")
  x <- sapply(x,gsub,pattern="\\.",replacement="")
  sapply(x,substr,start=1,stop=1)}  
IntegerSelector(19:25) # See it in action. 

# This function is designed to take as input a vector of single digit numbers.  It then uses grep to identify the elements of that vector beggining with each of the 9 single digit positive integers.  Next, it counts how often each of the 9 digits occurs, and returns a vector of length 9 with this information for each digit.  The return vector is arranged in ascending order, and has the single digit integers as names.  (can be used on matrices with the help of apply(), in which case it returns a matrix!)
LetterCounter <- function(x){
  out <- sapply(as.character(1:9),grep,x,simplify=FALSE)
  lengths <- sapply(out,length,USE.NAMES=TRUE)
  return(lengths)
}

#Performs the calculations necesary to estimate the Leemis M statistic. The input should be a length 9 vector (such as the columns of the output of my LetterCounter function)  
LeemisM <- function(x){
  max(x-log10(1+1/(1:9))) #because of R's vectorization and the fact that the input should be length 9, the use of 1:9 inside of the log10 function works correctly.  
}

#This performs the calculations necessary for the estimation of the Cho-Gains D statistic.  Again, the input should be length 9.  
ChoGainsD <- function(x){ 
  inner <- x-log10(1+1/(1:9)) #takes advantage of vectorization and proportions always having 9 rows as well. 
  inner.sq <- inner^2 #more vectorization
  inner.sum <- sum(inner.sq) #vectorize!
  final <- sqrt(inner.sum) 
  return(final)
}

#takes as inputs x, a vector or matrix of election results, and statistics, which can be specified as "m" for Leemis M, "d" for Cho-Gains D, or both "m" and "d" which returns both (this is the default).  Any other elements in statistic are ignored unless neither "m" nor "d" are present, in which case the function returns the digit distribution and a message asking the user to set statistic to m and/or d.   
BenfordLawStats <- function(x,statistic=c("m","d")){  #choose your statistic: m or d! defaults to both.
  
  #coerce whatever input you give into a numeric matrix.  (vectors become a 1 column matrix.)
  VoteTotals <- apply(as.matrix(x),2,as.numeric) 
  
  # Pick out the first digit of every element in the matrix.
  FirstSignificantDigit <- apply(VoteTotals,2,IntegerSelector)
  
  #Now create a 9 by n (number of columns of input x) matrix with the number of times each integer begins an element within each column 
 IntegerTotals <- apply(FirstSignificantDigit,2,LetterCounter)
  
  #Now divide these totals by the total number of rows to get proportions
  Proportions <- IntegerTotals/nrow(VoteTotals)
  
  #for when only one of the two stats is calculated...
  m.stats <- NULL
  d.stats <- NULL
  
  #for when only M need be calculated
  if(any(statistic%in%"m")){ 
  #Proportions should always have 9 rows, meaning that simply using my LeemisM function and apply allows us to estimate this statistic.  
    m.stats <- apply(Proportions,2, LeemisM )
   # m.stats <- sqrt(nrow(VoteTotals))*m.stats #multiplies by the square root of the number of observations, as suggested by Myunghoon.  
    names(m.stats) <- colnames(VoteTotals) #for easily understandable output 
  } 
  
  #for when only D need be calculated
  if(any(statistic%in%"d")){
    #Again, since Proportions should always have 9 rows, the use of ChoGainsD and apply allows us to estimate these statistics.  
    d.stats <- apply(Proportions,2, ChoGainsD)
    #d.stats <- sqrt(nrow(VoteTotals))*d.stats #multiplies by the square root of the number of observations, as suggested by Myunghoon.  
    names(d.stats) <- colnames(VoteTotals) # for easy interpretation (This line may be unneccesary!)
  }
  
  #and finally, neither
  if(any(!statistic%in%c("m","d"))){
    warning("Please set statistic to m and/or d!") #all you get back is a warning here.  Don't choose things that aren't m or d! 
    stop
  }
  output <- list(LeemisM=m.stats,ChoGainsD=d.stats,DigitDistribution=IntegerTotals) #put everything together in a list
  class(output) <- "benfords" #so that I can use a fancy version of print on my function!
  return(output)
}

x <- matrix(sample(1:1000,size=80,replace=T),ncol=4)#Some artificial data to show how it works
colnames(x) <- c("This","Bird","Has","Flown") 
BenfordLawStats(x)

########### 2. Critical Values  ##########

#some functions to call within my function:

AlphaChecksM <- function(x,...) { #function will check for significance of M stats
    x <- round(x,...)
  if( 0.851 <= x & x < .967 ){ # 0.10
    x <- paste(round(x,...),"*",sep="") # single star
  }
  if( 0.967 <= x & x < 1.212){ #0.05
    x <- paste(round(x,...),"**",sep="") # two stars
  }
  if( 1.212 <= x ){ #0.01
    x <- paste(round(x,...),"***",sep="") #three stars 
  }
  return(x)
}

AlphaChecksD <- function(x,...) { #function will check for significance of D stats
    x <- round(x,...)
  if( 1.212 <= x & x < 1.330 ){ # 0.10
    x <- paste(round(x,...),"*",sep="") # single star
  }
  if( 1.330 <= x & x < 1.569){ #0.05
    x <- paste(round(x,...),"**",sep="") # two stars
  }
  if( 1.569 <= x ){ #0.01
    x <- paste(round(x,...),"***",sep="") #three stars 
  }
  return(x)
}

#prints the lines of my output functions
StatPrinter <- function(x){
  if(!is.null(names(x))){ #if the object has a name, it prints the name alongside the test statistic
  cat("Column name: ", as.name(names(x)),"-", "Test Statistic: ", x, "\n")  
  } else {cat("Test Statistic:",x,"\n")} #just print the test statistic if there is no assocaited name (mainly when the input to BenfordLawStats is simply a vector!)
}

#this is my function.  By naming it this way and classifying BenfordLawStats output as "BenfordLaw" it will automatically call when I run the BenfordLawStats function without saving the output to an object.  
print.benfords <- function(x,digits=3){ #note: one can change the number of digits reported. 
  if(!is.null(x$LeemisM)) { #do these things when the Leemis' M is being calculated
    cat("Null hypothesis: no fraud","\n")
    cat("Statistic: Leemis\' m","\n")
   StatsToPrint <- sapply(x$LeemisM,AlphaChecksM,digits=digits)  #This code checks each test statistic against the critical values to determine significance, and adds the *'s when appropriate
    for(i in 1:length(StatsToPrint)){ #this prints the output with column names (when applicable).  Couldn't get this to work outside of a for loop (i.e. in with sapply)
      StatPrinter(StatsToPrint[i])
    }
  }
  
  if(!is.null(x$ChoGainsD) & !is.null(x$LeemisM)){ #prints this little line when both M and D are being reported, just to make the output easy to read
    cat("--------------------------","\n")
  }
  if(!is.null(x$ChoGainsD)) { #same as the M statistic above, only with the D statistic.
    cat("Null hypothesis: no fraud","\n")
    cat("Statistic: Cho-Gains\' d","\n")
    StatsToPrint <- sapply(x$ChoGainsD,AlphaChecksD,digits=digits)  
    for(i in 1:length(StatsToPrint)){
      StatPrinter(StatsToPrint[i])
    }
  }
  if(!is.null(x$ChoGainsD) | !is.null(x$LeemisM)){ #prints the significance codes (unless both D and M are null)
  cat("--------------------------","\n")
  cat("Level of critical value met for null hypothesis rejection:","\n")
  cat("\'***\' 0.01 \'**\' 0.05 \'*\' 0.10", "\n") 
  }
  if(is.null(x$ChoGainsD) & is.null(x$LeemisM)){ #when both D and M are null, do nothing.  All one gets back is the warning written into the BenfordLawStats funciton.  
   NULL
  }
}

nines <- rep(9,99)
BenfordLawStats(nines)

########### 3. Testing ##########

#1. Develop a function that will unit test your function. This function can be designed in any way you like, but bust meet the following conditions:

#The function takes some test data, and then the "true" statistic and distribution for this data, plus the number of digits to calculate equality to 
BLawTest <- function(TestData1,TrueTestData1Dist,TrueTestData1LeemisM,TrueTestData1ChoGainsD,TestData2=NULL,TrueTestData2Dist=NULL,TrueTestData2LeemisM=NULL,TrueTestData2ChoGainsD=NULL,digits=3){

  #The funciton is divided into two sections: One for Test Data 1, another for Test Data 2. At least one set of Test Data is required.  The second set of Test Data is optional
  
  output1 <- BenfordLawStats(TestData1) #first, run my function and store the results.  
  
  result1 <- c(0,0,0) #this is the indicator to return at the end of the function to determine if things worked right
  
  #now, begin some comparisons: First, Leemis m
  functionM1 <- round(output1$LeemisM,digits=3)
  TrueM1 <- round(TrueTestData1LeemisM,3)
  Mcomparisons1 <- mapply(identical,functionM1,TrueM1)
  if(all(Mcomparisons1)){ #use the identical funciton because it is more accurate with digits. 
    result1[1] <- 1
  } else { 
  #when this is not true it prints a message telling you where it failed
  cat("Unit Test of Test Data 1 failed at Leemis\' m","\n") 
  }
  
  #Second, the Cho-Gains d
  functionD1 <- round(output1$ChoGainsD,digits=3)
  TrueD1 <- round(TrueTestData1ChoGainsD,3)
  Dcomparisons1 <- mapply(identical,functionD1,TrueD1)
  if(all(Dcomparisons1)){
    result1[2] <- 1 
  } else { 
  cat("Unit Test of Test Data 1 failed at Cho-Gains\' d","\n")
  }
  
  #Finally, compare the Benford Distributions
  if(all(mapply(identical,as.numeric(output1$DigitDistribution),as.numeric(TrueTestData1Dist)))){
    result1[3] <- 1
  } else { 
  cat("Unit Test of Test Data 1 failed at Benford Distribution","\n")
  }
  
  #Make a little vector to be used for storing my result
  test.result <- NULL
  
  #Finally, determine if all tests are passed
  if(sum(result1)==3) { 
    test.result[1] <- TRUE
    names(test.result) <- "Test Data 1"
  } else { 
    test.result[1] <- FALSE
    names(test.result) <- "Test Data 1"
  }
  
  #Here begins the section for Test Data 2.  There's a switch around the whole thing, which makes it not run if no data is supplied by the user for Test Data 2!
  
  output2 <- NULL #so that the output still works if the user decides not to include two data sets.  
  
  if(!is.null(TestData2)){
    
  output2 <- BenfordLawStats(TestData2) #first, run my function and store the results.  
  
  result2 <- c(0,0,0) #this is the indicator to return at the end of the function to determine if things worked right
  
  #now, begin some comparisons: First, Leemis m
  functionM2 <- round(output2$LeemisM,digits=3)
  TrueM2 <- round(TrueTestData2LeemisM,3)
  Mcomparisons2 <- mapply(identical,functionM2,TrueM2)
  if(all(Mcomparisons2)){ #use the identical funciton because it is more accurate with digits. 
    result2[1] <- 1
  } else { 
    #when this is not true it prints a message telling you where it failed
    cat("Unit Test of Test Data 2 failed at Leemis\' m","\n") 
  }
  
  #Second, the Cho-Gains d
  functionD2 <- round(output2$ChoGainsD,digits=3)
  TrueD2 <- round(TrueTestData2ChoGainsD,3)
  Dcomparisons2 <- mapply(identical,functionD2,TrueD2)
  if(all(Dcomparisons2)){
    result2[2] <- 1 
  } else { 
    cat("Unit Test of Test Data 2 failed at Cho-Gains\' d","\n")
  }
  
  #Finally, compare the Benford Distributions
  if(all(mapply(identical,as.numeric(output2$DigitDistribution),as.numeric(TrueTestData2Dist)))){
    result2[3] <- 1
  } else { 
    cat("Unit Test of Test Data 2 failed at Benford Distribution","\n")
  }
  
  #Finally, determine if all tests are passed
  if(sum(result2)==3) { 
    test.result[2] <- TRUE
    names(test.result) <- c("Test Data 1", "Test Data 2")
  } else { 
    test.result[2] <- FALSE
    names(test.result) <- c("Test Data 1", "Test Data 2")
  }
  }
  #the result is a true or false plus any messages printed into the console with cat()
  return(list(TestResult=test.result,TestData1Results=output1,TestData2Results=output2))
}

# Make up some random data, and calculate the "True Values" for this data
set.seed(1801) #for replicatability 

TestData1 <- rnbinom(10,3,.3) #this is data that meets Benford's law!
BenfordLawStats(TestData1)
TrueTestData1Dist <- c(3,1,0,2,0,2,2,0,0)
TrueTestData1Prop <- c(.3,.1,0,.2,0,.2,.2,0,0)
Mlogs <- -log10(1+1/1:9)
TrueMstats1 <- TrueTestData1Prop+Mlogs
TrueTestData1LeemisM <- sqrt(10)*max(TrueMstats1)

TrueDSquared1 <- TrueMstats1^2
TrueDSum1 <- sum(TrueDSquared1)
TrueDroot1 <- sqrt(TrueDSum1)
TrueTestData1ChoGainsD <- sqrt(10)*TrueDroot1

TestData2 <- rnorm(10,10) #Test Data where Benford's law is NOT met. 
BenfordLawStats(TestData2)
TrueTestData2Dist <- c(8,0,0,0,0,0,0,1,1)
TrueTestData2Prop <- c(.8,0,0,0,0,0,0,.1,.1)
TrueMstats2 <- TrueTestData2Prop+Mlogs
TrueTestData2LeemisM <- sqrt(10)*max(TrueMstats2)

TrueDSquared2 <- TrueMstats2^2
TrueDSum2 <- sum(TrueDSquared2)
TrueDroot2 <- sqrt(TrueDSum2)
TrueTestData2ChoGainsD <- sqrt(10)*TrueDroot2

BLawTest(TestData1, TrueTestData1Dist,TrueTestData1LeemisM,TrueTestData1ChoGainsD,TestData2, TrueTestData2Dist,TrueTestData2LeemisM,TrueTestData2ChoGainsD,digits=3) #Compares my function to the truth, and returns a TRUE for both sets of test data, as required. Also returns the results of the BenfordLawStats function, as required.

#The function also works with only one data set: 
BLawTest(TestData1, TrueTestData1Dist,TrueTestData1LeemisM,TrueTestData1ChoGainsD,digits=3) #Simply gives a NULL as the output for the element of the list called "TestData2Results" 


