# OOPSLA2015 Artifact Evaluation Instruction

## Overview

The artifacts of paper22 include
* This README.md as the installation and performance evaluation guide. The document can also be found at 
* The valor R package, which does the compiler transformation and provides runtime functions to support the execution of the vectorized code.
* The benchmark application codes, which was used in the paper's performance evaluation.

## Installation
### Manual Installation

The manual installation guide has been tested in a standard Linux Platform and Cygwin. Other platforms may also support all the steps if they can install GNU-R, GIT and Python.

1. Pre-Requirement

Standard GNU tool chains with gcc/gfortran for installing GNU-R. GIT tool for checking out code and benchmarks. Python for running performance evaluation scripts.

2. GNU-R Installation 

* Linux Platform
  Download GNU-R source code from http://cran.rstudio.com/src/base/R-3/R-3.1.2.tar.gz, unzip it, configuration and make. You may have to install other pre-required software libraries for GNU-R installation. Please follow the instructions in the error log if you meet. 

* Cygwin
  Use Cygwin setup tools to install the latest R binaries, 3.1.2.
  
3. Download and install R Apply vectorizaiton package.

Starting from here, all the artifacts of paper22 will be stored in the _paper22_ sub-directory under the current user's home directory, which is _~/paper22_.

The R apply vectorization package is open sourced, and can be downloaded directly from https://github.com/wanghc78/valor .

```bash
~ $ cd paper22
~/paper22 $ git clone https://github.com/wanghc78/valor.git
```

Then launch R and install the th 
```bash
~/paper22 $ R CMD INSTALL valor/pkg
```
If there is no problem, the console will shown _DONE (vecapply)_ in the last line of the installation log.

4. Download benchmark code.

The benchmark code is also open-sourced, and can be downloaded directly from https://github.com/wanghc78/benchmarks/tree/vecapply . Please clone the vecapply branch version since this branch contains the benchmark codes for the paper.

```bash
git clone -b vecapply https://github.com/wanghc78/benchmarks.git
```

The benchmark codes are located under _~/paper22/benchmarks/algorithm_. The following files will be used in the performance evaluation section.


### Virtual Machine

The virtual machine image is a Linux Fedora 19 x86 image with 4G memory as the initial setup. Username/passowrd: oopsla2015/paper22. R, valor package and benchmark codes have been installed in the virtual machine following the previous document. 

## Simple Translation Examples

Launch R
```bash
~/paper22 $ R
```
And in the R console, type `?va_cmpfun` to get the package's help. You can use key "q" to quit the help document. You can following the help document to try some simple transformation.

### Compile an expression
```R
data <- 1:10
expr <- quote(lapply(data, function(x){x+1})) #get an expression
vexpr <- va_compile(expr) #vectorize the expression
vres <- eval(vexpr) #evaluate the vectorized expression
identical(eval(expr), vres) #check the result
```

### Compile a function
```R
foo <- function() {
    squareFun <- function(x) { x * x }
    Reduce('+', lapply(data, squareFun))
}

vfoo <- va_cmpfun(foo)  #compile the whole function
vfoo #show the function transformation's result
vfoo() #evaluate the vector function function
```

### Generate the vector version of a function
```R
bar <- function(x) { x[1] * x[2]}  #a single object function
vbar <- va_vecClosure(bar) #the vector version of the single object function
```

By default, the package will report the vectorization's result in the console log starting with "WARN:" prompt. 

## Conducting Performance Evaluation of the Paper

**Note** The performance evaluation results reported in the paper was evaluated in a server with large memory(64G). Because the vectorized code requires more memory (Paper section), the speedup number in the  virtual machine (4G mem default) may be smaller than the number reported, and the virtual machine cannot run the large data size test, for example 4x and 16x data input. If you used the manual installation in a similar platform as the paper, you can run large input and get the similar result.

## Brief Introduction of the source code of valor package.
