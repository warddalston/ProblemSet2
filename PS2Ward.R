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
  
  #for when only M need be calculated
  if(any(statistic%in%"m") & !any(statistic%in%"d")){ 
  #Proportions should always have 9 rows, meaning that simply using my LeemisM function and apply allows us to estimate this statistic.  
    m.stats <- apply(Proportions,2, LeemisM )
    names(m.stats) <- colnames(x) #for easily understandable output 
    return(list(LeemisM=m.stats,DigitDistribution=IntegerTotals))
  } 
  
  #for when only D need be calculated
  if(any(statistic%in%"d") & !any(statistic%in%"m")){
    #Again, since Proportions should always have 9 rows, the use of ChoGainsD and apply allows us to estimate these statistics.  
    d.stats <- apply(Proportions,2, ChoGainsD)
    names(d.stats) <- colnames(x) # for easy interpretation
    return(list(ChoGainsD=d.stats,DigitDistribution=IntegerTotals))
  }
  
  #both M and D
  if(any(statistic%in%"m") & any(statistic%in%"d")){
    m.stats <- apply(Proportions,2, LeemisM )
    names(m.stats) <- colnames(x) #for easily understandable output 
    d.stats <- apply(Proportions,2, ChoGainsD)
    names(d.stats) <- colnames(x) # for easy interpretation
    return(list(LeemisM=m.stats,ChoGainsD=d.stats,DigitDistribution=IntegerTotals))
  }
  
  #and finally, neither
  if(any(!statistic%in%c("m","d"))){
    print("Please set statistic to m and/or d!") 
    return(list(DigitDistribution=IntegerTotals))
  }
}

x <- matrix(sample(100:1000,size=80,replace=T),ncol=4)#Some artificial data to show how it works
colnames(x) <- c("This","Bird","Has","Flown") 
BenfordLawStats(x)
rm(x)

########### 2. Critical Values  ##########

BenfordLawStatsSignif <- function(x,statistic="m"){  #choose your statistic: m or d! defaults to m. 
  
  
  #coerce whatever input you give into a numeric matrix.  (vectors become a 1 column matrix.)
  VoteTotals <- apply(as.matrix(x),2,as.numeric) 
  
  # Pick out the first digit of every element in the matrix.
  FirstSignificantDigit <- apply(VoteTotals,2,IntegerSelector)
  
  #Now create a 9 by n (number of columns of input x) matrix with the number of times each integer begins an element within each column 
  IntegerTotals <- apply(FirstSignificantDigit,2,LetterCounter)
  
  #Now divide these totals by the total number of rows to get proportions
  Proportions <- IntegerTotals/nrow(VoteTotals)
  
  if(statistic=="m"){ 
    # because of the vectorization of R, and that proportions will always have only 9 rows, 
    # can simply use apply to perform the calculations necessary. 
    m.stats <- apply(Proportions,2, function(x) max(x-log10(1+1/(1:9))) )
    names(m.stats) <- colnames(x) #for easily understandable output 
    alpha.checks.m <- function(x) { #function will check for significance 
      
      if( 0.851 <= x & x < .967 ){ # 0.10
        x <- paste(round(x,3),"*",sep="") # single star
      }
      if( 0.967 <= x & x < 1.212){ #0.05
        x <- paste(round(x,3),"**",sep="") # two stars
      }
      if( 1.212 <= x ){ #0.01
        x <- paste(round(x,3),"***",sep="") #three stars 
      }
      return(x)
    }
    print("* indicates rejection of the null hypothesis at the alpha = 0.10 level")
    print("** indicates rejection of the null hypothesis at the alpha = 0.05 level")
    print("*** indicates rejection of the null hypothesis at the alpha = 0.01 level")
    print("Caution: The presence or lack of stars is not the only way to make inferences from statistical test")
    return(list(m.stats=sapply(m.stats,alpha.checks.m,simplify=TRUE,USE.NAMES=TRUE)))
  } 
  if(statistic=="d"){
    d.stats <- apply(Proportions,2, function(x){ 
      inner <- x-log10(1+1/(1:9)) #takes advantage of vectorization and proportions always having 9 rows as well. 
      inner.sq <- inner^2 #more vectorization
      inner.sum <- sum(inner.sq) #vectorize!
      final <- sqrt(inner.sum) 
    }
    )
    names(d.stats) <- colnames(x) # for easy interpretation
    alpha.checks.d <- function(x) { #function will check for significance 
      
      if( 1.212 <= x & x < 1.330 ){ # 0.10
        x <- paste(round(x,3),"*",sep="") # single star
      }
      if( 1.330 <= x & x < 1.569){ #0.05
        x <- paste(round(x,3),"**",sep="") # two stars
      }
      if( 1.569 <= x ){ #0.01
        x <- paste(round(x,3),"***",sep="") #three stars 
      }
      return(x)
    }
    print("* indicates rejection of the null hypothesis at the alpha = 0.10 level")
    print("** indicates rejection of the null hypothesis at the alpha = 0.05 level")
    print("*** indicates rejection of the null hypothesis at the alpha = 0.01 level")
    print("Caution: Alpha levels are arbitraily chosen, and there is nothing inherently special about alpha=.05")
    return(list(d.stats=sapply(d.stats,alpha.checks.d,simplify=TRUE,USE.NAMES=TRUE)))
  }
  if(!statistic%in%c("m","d")){
    print("Please set statistic to m or d!") 
  }
}

x <- matrix(as.character(rep(9,20)),ncol=4,nrow=20) #some sample data
colnames(x) <- c("Dalston","is","checking his","work")
BenfordLawStatsSignif(x,statistic="d") #should return with stars!
rm(x)