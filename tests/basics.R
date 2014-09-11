# Simple Tests for vecapply library

#library(vecapply)
source('../R/vecapply.R')

aList <- list(1,2,3,4,5)
aFun <- function(x) { x + 1 }
e <- quote(lapply(aList, aFun))

ve <- va_compile(e)

#expected result is
# va_vec2list(va_vecFun(aFun)(va_list2Vec(aList)))

eval(ve)


e1 <- quote(lapply(aList, function(x) { x + 1 }))
ve1 <- va_compile(e1)


#The third case
test <- function () {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { x + 1 }
    lapply(aList, aFun)
}


test2 <- function() {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { 
        y <- x + 1
        y * 2
    }
    lapply(aList, aFun)
}

grad.func <- function(yx) {
    y <- yx[1]
    x <- c(1, yx[2])  # add 1 to est interception
    error <- (sum(x *theta) - y)
    delta <- error * x
    return(delta)
}

#test var binding
test3 <- function() {
    aList <- list(1,2,3,4,5)
    if(True){
        bList <- "abcd"
    }
}

#test Reduce('+', lapply(aFun))
test4 <- function() {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { 
        y <- x + 1
        y * 2
    }
    Reduce('+', lapply(aList, aFun))
}

#test Reduce
test5 <- function() {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { 
        y <- x + 1
        y * 2
    }
    outList <- lapply(aList, aFun)
    Reduce('+', outList)
}

#test if else transform
test6 <- function(x) {
    if(x) { y} else { x}
    
    if(y) { x} else { 1}
}

