% File vecapply/pkg/man/vecapply.Rd
% Part of the R vecapply package, https://github.com/wanghc78/vecapply
% Distributed under BSD license

\name{va_compile}
\alias{va_cmpfun}
\alias{va_veclen}
\alias{va_vecClosure}
\title{Vectorization of apply class of operations}
\usage{
va_compile(expr, env = .GlobalEnv, options = NULL)
va_cmpfun(fun, options = NULL)
va_vecClosure(singleObjectFun, options = NULL)
}
\arguments{
  \item{expr}{expression that contains statements of lapply calls}
  \item{fun}{function that contains statements of lapply calls}
  \item{options}{list of vectorization compiling options}
  \item{singleObjectFun}{a function that is used to be applied to one single element in lapply}
}
\description{
  vecapply package transforms Apply class of 
  operations into a direct function call to improve the performance, like
  "lapply(1:5, function(x){x^2}) ===> (1:5)^2 "

  Suppose we have "lapply(listInput, singleObjectFun)", the package uses compiler 
  and runtime technologies to transform the "listInput" into a vector representation 
  "vecInput", and transform the "singleObjectFun" which can process one element at a 
  time into a function "vecFun" which can process a vector at a batch. Then the package
  will rewrite the original lapply call into a direct function invoke "vecFun(vecInput)".
  
  By leveraging the vector computing capability of R, the output expression can run much faster
  (up to 10x level) than the original lapply call. The package also applies other  
  optimizations, including merging reduce call into transformation, remove redundant data
  transformations, etc.

  The current version only supports transform expressions or functions that contain lapply 
  function calls

}
\examples{
# Compile an expression
data <- 1:10
expr <- quote(lapply(data, function(x){x+1}))
vexpr <- va_compile(expr)

vres <- eval(vexpr)
identical(eval(expr), vres)

# Compile a function
foo <- function() {
  squareFun <- function(x) { x * x }
  Reduce('+', lapply(data, squareFun))
}

vfoo <- va_cmpfun(foo)
vfoo()

# Just generate the vector version of a single object function
bar <- function(x) { x[1] * x[2]}
vbar <- va_vecClosure(bar)

}
\keyword{apply}
\keyword{lapply}
\keyword{programming}