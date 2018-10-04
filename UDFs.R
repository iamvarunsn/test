# UDF-1: Palindrome
Palindrome <- function(x) {
  x <- as.character(x)
  y <- strsplit(x, "")
  y <- unlist(y)
  x <- as.numeric(y)
  y <- rev(as.numeric(y))
  
  x <- as.numeric(paste(x, collapse = ""))
  y <- as.numeric(paste(y, collapse = ""))
  
  if(x == y)
  {
    cat(x, "is a palindrome.")
    
  } else {
    cat(x, "is not a palindrome.")
  }
}

# UDF-2 Prime Number Check
CheckPrime <- function(x) {
  i <- 1
  j <- 0
  for(i in 1:x)
  {
    if(x%%i == 0 & i != x)
    {
      # print(i)
      j <- j + 1
    }
  }
  
  if(j == 1)
  {
    return(T)
    j <- 0
  } else {
    return(F)
  }
}

# UDF-2a Prime Number Check and Return
CheckPrimeAndReturn <- function(x) {
  i <- 1
  j <- 0
  for(i in 1:x)
  {
    if(x%%i == 0 & i != x)
    {
      # print(i)
      j <- j + 1
    }
  }
  
  if(j == 1)
  {
    return(x)
    j <- 0
    # } else {
    #   factors.composite <- Factorize(x)
    #   for (k in 1:length(factors.composite)) {
    #     CheckPrimeAndReturn(factors.composite[i])
    #   }
    }
}

# UDF-3 Factorization
Factorize <- function(x) {
  a <- 1
  b <- 0
  factors <- c()
  
  while(a <= x & a != b){
    if(x %% a == 0 & a != b){
      b <- x/a
      if (a == b) {
        factors <- append(factors, a)
        break
      } else  {
      factors <- append(factors, c(a, b))
      }
    }
    
    a <- a + 1
  }
  factors <- sort(factors, F)
  return(factors)
}

# UDF-4 Prime Factorization
PrimeFactorize <- function(x) {
  y <- x
  a <- 1
  b <- 0
  i <- 1
  j <- 0
  
  primefactors <- c()
  factors <- c()
  factor.count <- c()
   
  # while(a <= x & a != b){
  #   if(x %% a == 0 & a != b){
  #     b <- x/a
  #     if (a == b) {
  #       factors <- append(factors, a)
  #       break
  #     } else  {
  #       factors <- append(factors, c(a, b))
  #     }
  #   }
  #   
  #   a <- a + 1
  # }
  factors <- sapply(x, Factorize)

  for (i in 1:length(factors)) {
    primefactors <- append(primefactors, unlist(sapply(factors[i], CheckPrimeAndReturn)))
  }
  
  i <- 1
  
  for (i in 1:length(primefactors)) {
    while (x %% primefactors[i] == 0) {
      j <- j + 1
      x <- x / primefactors[i]
    }
    factor.count <- append(factor.count, j)
    j <- 0
  }
  
  Prime.Factors.And.Exponent <- data.frame("Exponent" = factor.count, "Prime Factor" = primefactors, "Number" = y)
  
  return(Prime.Factors.And.Exponent)
}

# UDF-5 Least Common Multiple of any two numbers
LCMNormal <- function(x, y){
  if (max(x, y)%%min(x, y) == 0) {
    return(max(x, y))
  } else  {
    return(x * y)
  }
}

# UDF-5a Least Common Multiple of a sequence created using any two numbers
LCMSequence <- function(x, y) {
  factors.sequence <- seq(x, y)
  # prime.number.vec <- c()
  # composite.number.vec <- c()
  # factors.prime <- list()
  factors.prime.list <- list()
  tmp <- list()
  
  # for (i in 1:length(factors.sequence)) {
  #   CheckPrime(factors.sequence[i])
  #   if(CheckPrime(factors.sequence[i]) == T){
  #     prime.number.vec <- append(prime.number.vec, factors.sequence[i])
  #   } else{
  #     composite.number.vec <- append(composite.number.vec, factors.sequence[i])
  #   }
  # }
  
  # return(data.frame(sapply(composite.number.vec, PrimeFactorize)))
  # return(PrimeFactorize(composite.number.vec[1]))
  # for(j in 1:length(composite.number.vec)){
  #   
  #   tmp2 <- composite.number.vec[j]
  #   tmp <- PrimeFactorize(composite.number.vec[j])
  #   
  #   factors.prime.list <- rbind(factors.prime.list, tmp)
  #   # names(factors.prime.list) <- assign(nam <- paste("N", tmp2, sep = "."), composite.number.vec)
  #   
  #   factors.prime[j] <- list(rep(tmp$Prime.Factor, tmp$Exponent))
  #   # names(factors.prime) <- assign(nam <- paste("N", tmp2, sep = "."), composite.number.vec)
  # }
  
  for (i in 1:length(factors.sequence)) {
    tmp <- PrimeFactorize(factors.sequence[i])
    factors.prime.list <- rbind(factors.prime.list, tmp)
  }
  
  df <- factors.prime.list
  
  df
  
  # df <- split(df, df$Prime.Factor)
  # 
  # tmp3 <- c()
  # for(i in 1:length(df)){
  #   df[[i]] <- df[[i]][do.call(order, -df[[i]]), ]
  #   tmp3 <- rbind(tmp3, c(df[[i]][1, 1], df[[i]][1, 2]))
  # }
  # 
  # smp <- 1
  # 
  # for(i in 1:length(df)){
  #   smp <- smp * tmp3[i, 2] ^ tmp3[i, 1]
  # }
  # 
  # # factors.prime[[j+1]] <- prime.number.vec
  # # factors.prime
  # 
  return(tmp)
}

# UDF-6 Root Calculator
RootCalculator <- function(x, y)  {                  # x - number, y - root
  if(y > 6) {
    print("Polynomials above Sixth Degree not supported")
  }
  if(y == 2)  {
    
  }
}

# UDF-7 Weekday Checker
is.weekday <- function(x)  {
  check.weekday <- as.POSIXlt(x)$wday
  if(check.weekday > 0 & check.weekday < 6) {
    return(T)
  } else  {
    return(F)
  }
}
