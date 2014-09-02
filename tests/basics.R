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