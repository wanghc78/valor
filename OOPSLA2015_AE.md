# OOPSLA2015 Artifact Evaluation Instruction

## Overview

The artifacts of paper23 include
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
  Download GNU-R source code from http://cran.rstudio.com/src/base/R-3/R-3.1.2.tar.gz, unzip it, configuration and make. You may have to install other pre-required software libraries for GNU-R installation. Please follow the instructions in the error log if you meet. The procedure below is just a reference. 
  
```bash
~ $ tar -xzvf http://cran.rstudio.com/src/base/R-3/R-3.1.2.tar.gz
~ $ cd R-3.1.2
~/R-3.1.2 $ ./configure
~/R-3.1.2 $ make 
```  

You may have to add some configurations in the configure step, such as `./configure --with-x=no`. After installation, please set the system path to ensure you can execute `R` and `Rscript` in any directories.

* Cygwin
  Use Cygwin setup tools to install the latest R binaries, 3.1.2.
  
3. Download and install R Apply vectorizaiton package.

Starting from here, all the artifacts of paper23 will be stored in the _paper23_ sub-directory under the current user's home directory, which is _~/paper23_.

The R apply vectorization package is open sourced, and can be downloaded directly from https://github.com/wanghc78/valor .

```bash
~ $ cd paper23
~/paper23 $ git clone https://github.com/wanghc78/valor.git
```

Then launch R and install the th 
```bash
~/paper23 $ R CMD INSTALL valor/pkg
```
If there is no problem, the console will shown _DONE (vecapply)_ in the last line of the installation log.

4. Download benchmark code.

The benchmark code is also open-sourced, and can be downloaded directly from https://github.com/wanghc78/benchmarks/tree/vecapply . Please clone the vecapply branch version since this branch contains the benchmark codes for the paper.

```bash
git clone -b vecapply https://github.com/wanghc78/benchmarks.git
```

The benchmark codes are located under _~/paper23/benchmarks/algorithm_. The following files will be used in the performance evaluation section.


### Virtual Machine

The virtual machine image is a Linux Fedora 16 x86 image with 4G memory as the initial setup. Username/passowrd: oopsla2015/paper23. R, valor package and benchmark codes have been installed in the virtual machine following the previous document. 

## Simple Translation Examples

Launch R
```bash
~/paper23 $ R
```
And in the R console, type `library(vecapply)` to load the package, and `?va_cmpfun` to get the package's help. You can use key "q" to quit the help document. You may following the help document to try some simple transformation.

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

**Note** The performance evaluation results reported in the paper was evaluated in a server with large memory(64G). Because the vectorized code requires more memory (Paper section 3.4), the speedup number in the virtual machine (4G memory default) will be different than the number reported, and the virtual machine cannot run the large data size test, for example 4x and 16x data input. If you used the manual installation in a similar platform as the paper, you can run large input and get the similar result.

### Evaluation one example (LR-1var)

The following R files will be used in the evaluation. (Note: LR-1var is the LR example in the paper)
* setup_LR-1var.R Provide the input dataset for LR
* LR-1var\_lms\_lapply.R The original non-vec LR, iterative algorithm
* LR-1var\_lms\_lapply\_cmp.R The version with `va_cmpfun` wrapper. The run() function is exactly the same as the above one except this run will be redefined with `run <- va_cmpfun(run)`.
* LR-1var\_ols\_lapply.R The original non-vec LR, direct algorithm
* LR-1var\_ols\_lapply\_cmp.R The version with `va_cmpfun` wrapper. Similar to the iterative implementation, the run() function is exactly the same as the above one except this run will be redefined with `run <- va_cmpfun(run)`.

You can use `diff LR_lms_lapply.R LR_lms_lapply_cmp.R` and `diff LR_ols_lapply.R LR_ols_lapply_cmp.R` to see the changes.

#### Testing iterative algorithm

We can test 1M input and 15 iterations with the following command. The benchmark has been instrumented to show each iteration's time.
```bash
~/paper23 $ cd benchmarks/algorithm/LR
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_lms_lapply.R 1000000 15 #Original code
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_lms_lapply_cmp.R 1000000 15 #Vectorized code
```
You can append `| grep "Time ="` in the command line to only extract the time info. The time of iteration 6~15 are used to calculate the per-iteration time for LR iterative algorithm. 

In the vectorized version, there is one line showing "[INFO]va\_list2vec Time =". And the time is the data permutation time (defined as "PERM\_DOWN" in the paper). This time is used to calculate the data transformation overhead (Paper section 5.4).  

#### Testing direct algorithm

We still use test 1M input in the test. The benchmark has been instrumented to show each iteration's time.
```bash
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_ols_lapply.R 1000000 #Original code
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_ols_lapply_cmp.R 1000000 #Vectorized code
```
You can append `| grep "Time ="` in the command line to only extract the time info. Still you can get the data permutation time from the line showing "[INFO]va\_list2vec Time =".

### Vectorization Speedup Evaluation (Paper Section 5.3)


### Data Transformation Overhead Evaluation (Paper Section 5.4)

### Vectorization of Nested Apply Functions Evaluation (Paper Section 5.5)

### Vector Programming in applications Evaluation (Paper Section 5.6)

### Tiling in Vectorization (Paper Section 5.7)


## Brief Introduction of the source code of valor package.
