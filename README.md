# VALOR - Vectorization of AppLy for Interpretation Overhead Reduction of R

*valor* is an experimental R package that transforms Apply class of 
operations into a direct function call to improve the performance, like
```
lapply(1:5, function(x){x^2}) ===> (1:5)^2
```
## Key Concept

In a more general form, it acts as
```
aList <- ... # a list or vector data structure
aFun <- ... # a function for one element in aList
lapply(aList, aFun) 
  ===> vec2list(vecfun(aFun)(list2vec(aList)))
```
where

* _aList_ is transformed into a vector representation, like `list(1,2,3)` ===> `1:3`
* _aFun_ is transformed into a function that can process a vector input in a batch
* The final result from _aFun_ is transformed back as a list form to conform to the original _lapply_ output.

### Optimizations
The package applies several optimizations to get better performance, including joint _reduce_ optimization.

## Package Installation

You must install _devtools_ package in your R environment first, and load it.
```
install.packages("devtools")
library(devtools)
```

Then you can install the latest trial version of  _valor_ package from Github directly
```
install_github("wanghc78/valor", subdir="pkg")
library(vecapply)
```


## Usage Guide
*Warning*: The package is still in its early phase, and it can only support _lapply_ transformation, 
and cannot handle arbitrary cases. You comments and suggestions are highly welcome.

The package only provide two simple interfaces

| Name | Description |
|------|-------------|
|va_compile(anExpr)| transform _lapply_ in the input expression _anExpr_|
|va_cmpfun(aFun) | transform _lapply_ in the input function _aFun_ |

The interface will return the transformed expression or function, and then you can evaluate
the transformed object then. A simple example

```
library(vecapply)

foo <- function() {
  aList <- list(1:2,3:4,5:6)
  alpha <- 0.5
  aFun <- function(x) { alpha*x[1] + x[2] }
  lapply(aList, aFun)
}

foo<-va_cmpfun(foo)

foo()
```

[Other examples](Examples.md)


## Report Issues/Feedback 

You are very welcome to submit the issues and your feedback in the github issue system.



