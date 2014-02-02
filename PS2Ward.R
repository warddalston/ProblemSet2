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
    names(m.stats) <- colnames(x) #for easily understandable output 
  } 
  
  #for when only D need be calculated
  if(any(statistic%in%"d")){
    #Again, since Proportions should always have 9 rows, the use of ChoGainsD and apply allows us to estimate these statistics.  
    d.stats <- apply(Proportions,2, ChoGainsD)
    names(d.stats) <- colnames(x) # for easy interpretation
  }
  
  #and finally, neither
  if(any(!statistic%in%c("m","d"))){
    warning("Please set statistic to m and/or d!") 
    return(list(DigitDistribution=IntegerTotals))
  }
  output <- list(LeemisM=m.stats,ChoGainsD=d.stats,DigitDistribution=IntegerTotals)
  class(output) <- "BenfordLaw" #so that I can use a fancy version of print on my function!
  return(output)
}

x <- matrix(sample(100:1000,size=80,replace=T),ncol=4)#Some artificial data to show how it works
colnames(x) <- c("This","Bird","Has","Flown") 
x<- BenfordLawStats(x)
rm(x)

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

StatPrinter <- function(x){
  cat("Column name: ", as.name(names(x)),"-", "Test Statistic: ", x, "\n")   
}

print.BenfordLaw <- function(x,...){
  if(!is.null(x$LeemisM)) {
    cat("Null hypothesis of no fraud test for Leemis\' m statistic:","\n")
   StatsToPrint <- sapply(x$LeemisM,AlphaChecksM,digits=3)  
    for(i in 1:length(StatsToPrint)){
      StatPrinter(StatsToPrint[i])
    }
  }
  
  if(!is.null(x$ChoGainsD) & !is.null(x$LeemisM)){
    cat("--------------------------","\n")
  }
  if(!is.null(x$ChoGainsD)) {
    cat("Null hypothesis of no fraud test for Cho-Gains d statistic:","\n")
    StatsToPrint <- sapply(x$ChoGainsD,AlphaChecksD,digits=3)  
    for(i in 1:length(StatsToPrint)){
      StatPrinter(StatsToPrint[i])
    }
  }
  cat("--------------------------","\n")
  cat("Signif. Codes: \'***\' 0.01 \'**\' 0.05 \'*\' 0.10", "\n") 
}

print(x)

x <- as.character(rep(9,20)) #some sample data
names(x) <- c("Dalston")
BenfordLawStats(x)
print(BenfordLawStats(x)) #should return with stars!
rm(x)