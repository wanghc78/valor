# Simple Tests for vecapply library

#library(vecapply)
source('../R/cmputility.R')
source('../R/vecapply.R')
source('../R/runtime.R')

CHECK <- function(a, b, case_name) {
    if(!isTRUE(all.equal(a,b))) stop("[FAIL]",case_name)
    else cat("[PASS]", case_name, '\n')
}

aList <- list(1,2,3,4,5)
aVec <- c(1,2,3,4,5)
aMat <- matrix(1:12, 3, 4)
aFun <- function(x) { x + 1 }
bList <- list(2,3,4,5,6)

### Simple Case
e <- quote(lapply(aList, aFun))

ve <- va_compile(e)
#expected result is
# va_vec2list(va_vecFun(aFun)(va_list2Vec(aList)))

CHECK(eval(ve), eval(e), "Simple Case") 

### Test the function object is directly inside the lapply
e1 <- quote(lapply(aList, function(x) { x + 1 }))
ve1 <- va_compile(e1)

CHECK(eval(ve1), eval(e1), "Anonymous function object")


### Simple full code
fun1 <- function () {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { x + 1 }
    lapply(aList, aFun)
}

vfun1 <- va_cmpfun(fun1)

CHECK(vfun1(), fun1(), "Simple full function")

### full function 2

fun2 <- function() {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { 
        y <- x + 1
        y * 2
    }
    lapply(aList, aFun)
}

vfun2 <- va_cmpfun(fun2)

CHECK(vfun2(), fun2(), "Full function 2")


### Scalar function vectorization
sfun1 <- function(yx) yx[1]
vsfun1 <- va_vecClosure(sfun1)

CHECK(apply(aMat, 1, sfun1), vsfun1(aMat), "Vec []")

sfun2 <- function(yx) c(1, yx[2])
vsfun2 <- va_vecClosure(sfun2)
CHECK(t(apply(aMat, 1, sfun2)), vsfun2(aMat), "c()")

sfun3 <- function(x) sum(x)
vsfun3 <- va_vecClosure(sfun3)
CHECK(apply(aMat, 1, sfun3), vsfun3(aMat), "sum")

############ The below are more complex functions, hard to use CHECK

### Mscalar function 
grad.func <- function(yx) {
    y <- yx[1]
    x <- c(1, yx[2])  # add 1 to est interception
    error <- (sum(x *theta) - y)
    delta <- error * x
    return(delta)
}

v_grad.func <- va_vecClosure(grad.func)


#test Reduce('+', lapply(aFun))
testReduce_1 <- function() {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { 
        y <- x + 1
        y * 2
    }
    Reduce('+', lapply(aList, aFun))
}

va_cmpfun(testReduce_1)

#test Reduce
testReduce_2 <- function() {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { 
        y <- x + 1
        y * 2
    }
    outList <- lapply(aList, aFun)
    Reduce('+', outList)
}

va_cmpfun(testReduce_2)

#test if else transform
testIf_1 <- function(x) {
    if(x) { y} else { x}
    
    if(y) { x} else { 1}
}

va_vecClosure(testIf_1)

testIf_2 <- function(x) {
    if(x) { y }
    
    if(y) { 1 }
}

va_vecClosure(testIf_2)


#test var binding
testIf_3 <- function(x) {
    aList <- list(1,2,3,4,5)
    if(True){
        bList <- aList
    } else {
        bList <- x
    }
}

va_vecClosure(testIf_3)


testIf_4 <- function(x) {
    aList <- list(1,2,3,4,5)
    if(False){
        bList <- x
    } 
}

va_vecClosure(testIf_4)


#lapply's result as lapply
testLapply_1 <- function() {
    dists <- lapply(pts, dist.func)
    ids <- lapply(dists, which.min) 
}

va_cmpfun(testLapply_1)


testLoopLapply_1 <- function () {
    aList <- list(1,2,3,4,5)
    aFun <- function(x) { x + 1 }
    for(i in 1:50) {
        bList<- lapply(aList, aFun)
    }
}

va_cmpfun(testLoopLapply_1)


testTwoLapply_1 <- function() {
    aList <- list(1:2, 3:4, 5:6)
    bList <- lapply(aList, function(a){a+1})
    cList <- lapply(bList, which.min)
    cList
}

va_cmpfun(testTwoLapply_1)


testUnsupported_1 <- function(train_items_category) {
    names(which.max(table(train_items_category)))
}
va_vecClosure(testUnsupported_1)


testVectorInputFun_1 <- function(train_items_category) {
    which.min(train_items_category)
}
va_vecClosure(testVectorInputFun_1)


testSimplyAssign_1 <- function(a) {
    r <- a
}
va_vecClosure(testSimplyAssign_1)

#Testing ignore va_debug node
test_va_debug1 <- function() {
    aList <- list(1:2, 3:4, 5:6)
    va_debug(bList <- lapply(aList, function(a){a+1}))
    cList <- lapply(aList, function(a){a+1})
}

va_cmpfun(test_va_debug1)

test_va_debug2 <- function(a) {
    r1 <- a[1]
    va_debug(print(a[1]))
}
va_vecClosure(test_va_debug2)
