# OOPSLA2015 Artifact Evaluation Instruction

## I. Overview

The artifacts of paper23 include
* This README.md as the installation and performance evaluation guide. The document can also be found at 
* The valor R package, which does the compiler transformation and provides runtime functions to support the execution of the vectorized code.
* The benchmark application codes, which was used in the paper's performance evaluation.

Section II of this document describes the details steps to install R, the valor R package described in the paper and benchmarks used in the evaluation into a Linux environment. But you can always use the Virtualbox image provided. Section III is the Getting Started Guide, which help the user get familiar with the valor package. The first subseciton of Section IV is also the Getting Started Guide to help uesr praticse basic performance benchmarking. The rest of Section IV is the detail steps to reproduce the paper's evaluation section. Section V is a brief introduction of the valor package's source code.

## II. Installation
### Manual Installation

The manual installation guide has been tested in a standard Linux Platform and Cygwin. Other platforms may also support all the steps if they can install GNU-R, GIT and Python(in /usr/bin/python).

1. Pre-Requirement

  Standard GNU tool chains with gcc/gfortran for installing GNU-R. GIT tool for checking out code and benchmarks. Python for running performance reporting scripts.

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

  The benchmark codes are located under _~/paper23/benchmarks/algorithm_. The following files will be used in the performance evaluation section. You can read [this article https://github.com/rbenchmark/benchmarks/blob/master/docs/writting_benchmark.md] to understand the benchmark code's structure.

  * Iterative Algorithms
  
| Name in the Paper | Path | Filename |  Notes |
|-------------------|------|----------|--------|
|**ICA**|ICA|setup_ica.R|Testing input generator|
| |ICA|ica_lapply.R|Original iterative implementation|
| |ICA|ica_lapply_cmp.R|Original iterative with va_cmpfun() wrapper|
|**k-Means**|k-means|setup_k-means-1D.R|Testing input generator for k-Means|
| |k-means|k-means-1D_lapply.R|Original iterative implementation|
| |k-means|k-means-1D_lapply_cmp.R|Original iterative with va_cmpfun() wrapper|
| |k-means|k-means-1D_lapply_mtrans_level2.R|Used in Section 5.6 K-Means Inner Apply|
| |k-means|k-means-1D_lapply_mtrans_opt.R|Used in Section 5.6 K-Means Both Applys|
|**k-Means-nD**|k-means|setup_k-means.R|Testing input generator for k-Means-nD|
| |k-means|k-means_lapply.R|Original iterative implementation|
| |k-means|k-means_lapply_cmp.R|Original iterative with va_cmpfun() wrapper|
| |k-means|k-means_lapply_mtrans_level2.R|Used in Section 5.6 K-Means-nD Inner Apply|
| |k-means|k-means_lapply_mtrans_opt.R|Used in Section 5.6 K-Means-nD Both Applys|
|**LogitReg**|LogitRegression|setup_LogitRegre-1var.R|Testing input generator for LogitReg|
| |LogitRegression|LogitRegre-1var_lapply.R|Original iterative implementation|
| |LogitRegression|LogitRegre-1var_lapply_cmp.R|Original iterative with va_cmpfun() wrapper|
|**LogitReg-n**|LogitRegression|setup_LogitRegre.R|Testing input generator for LogitReg-n|
| |LogitRegression|LogitRegre_lapply.R|Original iterative implementation|
| |LogitRegression|LogitRegre_lapply_cmp.R|Original iterative with va_cmpfun() wrapper|
|**LR**|LR|setup_LR-1var.R|Testing input generator for both LR and LR-OLS|
| |LR|LR-1var_lms_lapply.R|Original iterative implementation|
| |LR|LR-1var_lms_lapply_cmp.R|Original iterative with va_cmpfun() wrapper|
| |LR| LR-1var_lms_lapply_mtrans_tiling.R|Used in Section 5.7 Tiling test|
|**LR-n**|LR|setup_LR.R|Testing input generator for both LR-n and LR-OLS-n|
| |LR|LR_lms_lapply.R|Original iterative implementation|
| |LR|LR_lms_lapply_cmp.R|Original iterative with va_cmpfun() wrapper|
| |LR| LR_lms_lapply_mtrans_tiling.R|Used in Section 5.7 Tiling test|


  * Direct Algorithms
  ***Note*** There is a typo in the paper. The **LR-OST/LR-OST-n** should be *LR-OLS/LR-OLS-n*
  
| Name in the Paper | Path | Filename |  Notes |
|-------------------|------|----------|--------|
|**NN**|k-NN|setup_k-NN.R|Testing input generator for both NN and kNN|
| |k-NN|NN_lapply.R|Original direct implementation|
| |k-NN|NN_lappy_cmp.R|Original direct with va_cmpfun() wrapper|
| |k-NN|NN_lapply_level2.R|Used in Section 5.6 NN Inner Apply|
| |k-NN|NN_lappy_mtrans.R|Used in Section 5.6 NN Both Applys|
|**k-NN**|k-NN|setup_k-NN.R|Testing input generator for both NN and kNN|
| |k-NN|k-NN_lapply.R|Original direct implementation|
| |k-NN|k-NN_lappy_cmp.R|Original direct with va_cmpfun() wrapper|
| |k-NN|k-NN_lapply_level2.R|Used in Section 5.6 k-NN Inner Apply|
| |k-NN|k-NN_lappy_mtrans.R|Used in Section 5.6 k-NN Both Applys|
|**LR-OLS**|LR|setup_LR-1var.R|Testing input generator for both LR and LR-OLS|
| |LR|LR-1var_ols_lapply.R|Original direct implementation|
| |LR|LR-1var_ols_lapply_cmp.R|Original direct with va_cmpfun() wrapper|
|**LR-OLS-n**|LR|setup_LR.R|Testing input generator for both LR-n and LR-OLS-n|
| |LR|LR_ols_lapply.R|Original direct implementation|
| |LR|LR_ols_lapply_cmp.R|Original direct with va_cmpfun() wrapper|
|**Monte Carlo**|Pi|setup_Pi.R|Testing input generator for Monte Carlo|
| |Pi|Pi_lapply.R|Original direct implementation|
| |Pi|Pi_lapply_cmp.R|Original direct with va_cmpfun() wrapper|
|**PCA**|Pi|setup_PCA.R|Testing input generator for PCA|
| |PCA|PCA_lapply.R|Original direct implementation|
| |PCA|PCA_lapply_cmp.R|Original direct with va_cmpfun() wrapper|

  
### Virtual Machine

The virtual machine image is a Linux Fedora 16 x86 image with 4G memory as the initial setup. Username/passowrd: oopsla2015/paper23. R, valor package and benchmark codes have been installed in the virtual machine following the previous document. 

## III. Simple Translation Examples

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

## IV. Conducting Performance Evaluation of the Paper

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

In the vectorized version, there is one line showing "[INFO]va\_list2vec Time =". And the time is the data permutation time (defined as "PERM\_DOWN" in the paper). This time is used to calculate the data transformation overhead (Paper section 5.4). 

You can append `| grep "Time ="` in the command line to only extract the time info. The time of iteration 6~15 are used to calculate the per-iteration time for LR iterative algorithm. And you can append `| ../../../valor/report.py` in the command line to report the summarized information, like "[Iterative]AvgIterTime = 3.594200 Overhead = 345.345%". The report.py is temporarily stored in the valor package only for the OOPSLA Artificats evaluation. Please make sure in your command the correct path location for report.py.

***Note***: In cygwin environment, the pipe system for R doesn't work correctly. You may need insert ` | tee /dev/null` before grep or report.py to correct the pipe issue.
 

#### Testing direct algorithm

We still use test 1M input in the test. The benchmark has been instrumented to show each iteration's time.
```bash
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_ols_lapply.R 1000000 #Original code
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_ols_lapply_cmp.R 1000000 #Vectorized code
```

Similar to testing iterative algorithm, you can append `| grep "Time ="` or `| ../../../valor/report.py` to get processed information, including the time without overhead and data permutation overhead percentage.

### Vectorization Speedup and Overhead Evaluation (Paper Section 5.3/5.4)

The following commands are used to reproduce the time measured in Section 5.3 and 5.4

#### Iterative algorithm

```bash

```

#### Direct Algorithm

```bash

```


### Vectorization of Nested Apply Functions Evaluation (Paper Section 5.5)

### Vector Programming in applications Evaluation (Paper Section 5.6)

### Tiling in Vectorization (Paper Section 5.7)


## V. Brief Introduction of the source code of valor package.
