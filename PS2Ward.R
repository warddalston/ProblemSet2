###########################################################
##### Problem Set 2 - Dalston Ward - February 6, 2013 #####
###########################################################

#Start everything by emptying the workspace.  (don't use this if concurrently working on other projects in R!)
rm(list=ls())
#Second, set the working directory as appropriate.  This makes the code cleaner and helps to keep track of any saved files.
setwd("~/Documents/WashU 2nd Year/Applied Stats Programming/Jan 30/PS2/ProblemSet2/")

########### 1. Benford Law statistics ##########

BenfordLawStats <- function(x,statistic="m"){  #choose your statistic: m or d! defaults to m. 
  
  #coerce whatever input you give into a numeric matrix.  (vectors become a 1 column matrix.)
  Vote.Totals <- apply(as.matrix(x),2,as.numeric) 
  
  # Pick out the first digit of every element in the matrix.
  First.significant.digit <- apply(as.matrix(Vote.Totals),2,function(x) {sapply(x,substr,start=1,stop=1)} )
  
  #Now create a 9 by n (number of columns of input x) matrix with the number of times each integer begins an element within each column 
  Integer.totals <- apply(First.significant.digit,2,function(x){sapply(as.character(1:9),function(y) length(grep(y,x)))})
  
  #Now divide these totals by the total number of rows to get proportions
  Proportions <- Integer.totals/nrow(Vote.Totals)
  
  if(statistic=="m"){ 
    # because of the vectorization of R, and that proportions will always have only 9 rows, 
    # can simply use apply to perform the calculations necessary. 
    m.stats <- apply(Proportions,2, function(x) max(x-log10(1+1/(1:9))) )
    names(m.stats) <- colnames(x) #for easily understandable output 
    return(list(M.stats=m.stats))
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
    return(list(d.stats=d.stats))
  }
  if(!statistic%in%c("m","d")){
    print("Please set statistic to m or d!") 
  }
}

x <- matrix(sample(100:1000,size=80,replace=T),ncol=4) #Some artificial data to show how it works
BenfordLawStats(x,statistic="m") 
rm(x)

###### Part 2: now add a way to test for significance! ##########

BenfordLawStatsSignif <- function(x,statistic="m"){  #choose your statistic: m or d! defaults to m. 
  
  #coerce whatever input you give into a numeric matrix.  (vectors become a 1 column matrix.)
  Vote.Totals <- apply(as.matrix(x),2,as.numeric) 
  
  # Pick out the first digit of every element in the matrix.
  First.significant.digit <- apply(as.matrix(Vote.Totals),2,function(x) {sapply(x,substr,start=1,stop=1)} )
  
  #Now create a 9 by n (number of columns of input x) matrix with the number of times each integer begins an element within each column 
  Integer.totals <- apply(First.significant.digit,2,function(x){sapply(as.character(1:9),function(y) length(grep(y,x)))})
  
  #Now divide these totals by the total number of rows to get proportions
  Proportions <- Integer.totals/nrow(Vote.Totals)
  
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
BenfordLawStatsSignif(x,statistic="m") #should return with stars!
rm(x)