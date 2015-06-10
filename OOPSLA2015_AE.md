# OOPSLA2015 Artifact Evaluation Instruction

## I. Overview

There are three groups of files containing supporting material for paper23

* This installation and performance evaluation guide (OOPSLA2015_AE.md). This document can also be found at https://github.com/wanghc78/valor/blob/master/OOPSLA2015_AE.md 
* The valor R package, containing the vectorizing restructurer and the runtime functions needed for execution of the vectorized code.
* The benchmark codes used in the paper to evaluate the effectiveness of the vectorizer.

Section II of this document discuss how to carry out the installations necessary to repeat the experiments presented in the paper. Specifically, it describes how to install R in a Linux environment and how to download the valor R package and the benchmark codes. Alternatively, the Virtualbox image provided can be used. Section III is the Getting Started Guide which prsents useful information on how to use the valor package. Section IV list a series of steps that can be followed to reproduce the paper's evaluation section. Section V is a brief description of the valor package's source code and the benchmarks.

## II. Installation

### Manual Installation

The following manual installation steps were tested in a standard Linux Platform and Cygwin. Other platforms may also support all the steps if GNU-R, GIT and Python can be installed in them.

#### 1. Prerequisites

* GNU compilers gcc/gfortran for installing GNU-R. 
* GIT for checking out code and benchmarks. 
* Python installed at /usr/bin/python for running performance reporting scripts.

#### 2. Installation of GNU-R 

**Linux Platform**

Download GNU-R source code from http://cran.rstudio.com/src/base/R-3/R-3.1.2.tar.gz, unzip it, configuration, and make. You may have to install other software modules to complete the GNU-R installation. Please follow the instructions in the error log if there are problems. The procedure below is just a reference. 
  
```bash
~ $ wget http://cran.rstudio.com/src/base/R-3/R-3.1.2.tar.gz
~ $ tar -xzvf R-3.1.2.tar.gz
~ $ cd R-3.1.2
~/R-3.1.2 $ ./configure
~/R-3.1.2 $ make 
```

You may have to add some configurations in the configure step, such as `./configure --with-x=no`. After installation, please set the system path to ensure you can execute `R` and `Rscript` in any directories.

**Cygwin**

Use Cygwin setup tools to install the latest R binaries, for example 3.1.2.
  
#### 3. Download and install R Apply vectorizaiton package.

At this point, all the artifacts of paper23 will be stored in the _paper23_ sub-directory under the current user's home directory, which is _~/paper23_.

The R vectorization package is open sourced and can be downloaded from https://github.com/wanghc78/valor .

```bash
~ $ cd paper23
~/paper23 $ git clone https://github.com/wanghc78/valor.git
```

Then launch R and install the package

```bash
~/paper23 $ R CMD INSTALL valor/pkg
```
If the installation was successful, the console will shown _DONE (vecapply)_ in the last line of the installation log.

#### 4. Download benchmark codes.

The benchmark code is also open-sourced and can be downloaded from https://github.com/wanghc78/benchmarks/tree/vecapply . Please clone the vecapply branch version since this branch contains the benchmark codes for the paper.

```bash
git clone -b vecapply https://github.com/wanghc78/benchmarks.git
```

The benchmark codes are located under _~/paper23/benchmarks/algorithm_. The scripts in the sub-directories are the ones that were used in the performance evaluation. The document https://github.com/rbenchmark/benchmarks/blob/master/docs/writting_benchmark.md describes the structure of the benchmarks. A the benchmark scripts are listed in Section V of this document.

  
### Use Virtual Machine

The file URL at https://oopsla15aec.hotcrp.com points to the Virtualbox (https://www.virtualbox.org/) image file. The virtual machine image is a Linux Fedora 16 x86 image. Please create a Fedora x86 Linux system with 4G memory. The linux's Username/passowrd: oopsla2015/paper23. R, valor package and benchmark codes have been installed in the virtual machine following the above document. The valor package and benchmarks are stored in paper23 directory under oopsla2015's home directory.

## III. Simple Translation Examples

Launch R
```bash
~/paper23 $ R
```
And, in the R console, type `library(vecapply)` to load the package, and `?va_cmpfun` to get the package's help. You can use key "q" to quit the help document. The help document contain some simple transformation examples.

### Compile (translate into vector code) an expression
```R
library(vecapply)
data <- 1:10
expr <- quote(lapply(data, function(x){x+1})) #get an expression
vexpr <- va_compile(expr) #vectorize the expression
vres <- eval(vexpr) #evaluate the vectorized expression
identical(eval(expr), vres) #check the result
```

### Compile (translate into vector code) a function
```R
foo <- function() {
    squareFun <- function(x) { x * x }
    Reduce('+', lapply(data, squareFun))
}

vfoo <- va_cmpfun(foo)  #compile the whole function
vfoo #show the function transformation's result
vfoo() #evaluate the vector function function
```

### Generate the vector version of a function only
```R
bar <- function(x) { x[1] * x[2]}  #a single object function
vbar <- va_vecClosure(bar) #the vector version of the single object function
```

By default, the package will report the vectorization's result in the console log starting with "WARN:" prompt. 

## IV. Reproducing the performance evaluation reported in the paper

**Note** The performance evaluation results reported in the paper were conducted using a server with large memory(64G). Because of the memory requirements of the vectorized code (Paper section 3.4), if the memory is not as large as that used for the evaluation discussed in the paper the speedups that will be obtained with the virtual machine (4G memory default) will be different than the number reported in the paper, and the virtual machine will not be able to run the large data size tests (e.g. 4x and 16x data input). If the installation is done in a platform similar to that used for the paper it will be possible to run large inputs and get similar results as those in the paper.

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

You can append `| grep "Time ="` in the command line to only extract the time info. The time of iteration 6~15 are used to calculate the per-iteration time for LR iterative algorithm. And you can append `| ../../../valor/report.py` in the command line to report the summarized information, like "[Iterative]AvgIterTime = 3.594200 Overhead = 345.345%". The report.py is temporarily stored in the valor package only for the OOPSLA Artificats Evaluation. Please make sure the correct path location of report.py in your command.

***Note***: In cygwin environment, the pipe system for R may not work correctly. You may need to insert ` | tee /dev/null` before grep or report.py to correct the pipe issue.
 

#### Testing direct algorithm

We still use test 1M input in the test. The benchmark has been instrumented to show each iteration's time.
```bash
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_ols_lapply.R 1000000 #Original code
~/paper23/benchmarks/algorithm/LR $ Rscript --vanilla LR-1var_ols_lapply_cmp.R 1000000 #Vectorized code
```

Similar to testing iterative algorithm, you can append `| grep "Time ="` or `| ../../../valor/report.py` to get processed information, including the time without overhead and data permutation overhead percentage.

### Vectorization Speedup and Overhead Evaluation (Paper Section 5.3/5.4)

Suppose you are in the directory of "~/paper23/benchmarks/algorithm", the following commands could be used to reproduce the time measured in Section 5.3 and 5.4. Please note, the commands may run up to minutes to hours. You can reduce the input size to reduce the running time.

#### Iterative algorithm

```bash
cd ICA
echo ICA nonVec 1M samples, 2 signals, 15 iterations
Rscript --vanilla ica_lapply.R 1000000 2 15 | ../../../valor/report.py
echo ICA Vec 1M samples, 2 signals, 15 iterations
Rscript --vanilla ica_lapply_cmp.R 1000000 2 15 | ../../../valor/report.py
cd ..
cd k-means
echo k-Means nonVec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply.R 1000000 10 15 | ../../../valor/report.py
echo k-Means Vec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
echo k-Means-nD nonVec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply.R 1000000 10 3 15 | ../../../valor/report.py
echo k-Means-nD Vec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply_cmp.R 1000000 10 3 15 | ../../../valor/report.py
cd ..
cd LogitRegression
echo LogitReg nonVec 1M points, 15 iterations
Rscript --vanilla LogitRegre-1var_lapply.R 1000000 15 | ../../../valor/report.py
echo LogitReg Vec 1M points, 15 iterations
Rscript --vanilla LogitRegre-1var_lapply_cmp.R 1000000 15 | ../../../valor/report.py
echo LogitReg-n nonVec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LogitRegre_lapply.R 1000000 10 15 | ../../../valor/report.py
echo LogitReg-n Vec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LogitRegre_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
cd ..
cd LR
echo LR nonVec 1M points, 15 iterations
Rscript --vanilla LR-1var_lms_lapply.R 1000000 15 | ../../../valor/report.py
echo LR Vec 1M points, 15 iterations
Rscript --vanilla LR-1var_lms_lapply_cmp.R 1000000 15 | ../../../valor/report.py
echo LR-n nonVec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LR_lms_lapply.R 1000000 10 15 | ../../../valor/report.py
echo LR-n Vec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
cd ..
```

#### Direct Algorithm

```bash
cd k-NN
echo NN nonVec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply.R 10000 10000 10 | ../../../valor/report.py
echo NN Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply_cmp.R 10000 10000 10 | ../../../valor/report.py
echo k-NN nonVec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply.R 10000 10000 10 5 | ../../../valor/report.py
echo k-NN Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply_cmp.R 10000 10000 10 5 | ../../../valor/report.py
cd ..
cd LR
echo LR-OLS nonVec 1M points
Rscript --vanilla LR-1var_ols_lapply.R 1000000 | ../../../valor/report.py
echo LR-OLS Vec 1M points
Rscript --vanilla LR-1var_ols_lapply_cmp.R 1000000 | ../../../valor/report.py
echo LR-OLS-n nonVec 1M length 10 vec variable
Rscript --vanilla LR_ols_lapply.R 1000000 10| ../../../valor/report.py
echo LR-OLS-n Vec 1M length 10 vec variable
Rscript --vanilla LR_ols_lapply_cmp.R 1000000 10 | ../../../valor/report.py
cd ..
cd Pi
echo nonVec Monte Carlo 1M points
Rscript --vanilla Pi_lapply.R 1000000 | ../../../valor/report.py
echo Vec Monte Carlo 1M points
Rscript --vanilla Pi_lapply_cmp.R 1000000 | ../../../valor/report.py
cd ..
cd PCA
echo nonVec PCA 1M length 10 vector samples
Rscript --vanilla PCA_lapply.R 1000000 10 | ../../../valor/report.py
echo nonVec PCA 1M length 10 vector samples
Rscript --vanilla PCA_lapply_cmp.R 1000000 10 | ../../../valor/report.py
cd ..
```

The two scripts are also saved as "bench_iter.sh" and "bench_direct.sh" in the algorithm directory.

### Vectorization of Nested Apply Functions Evaluation (Paper Section 5.5)

The base nonVec time and the **Outer Apply** schema's time could be grappbed from the above section. The following commands can be used to get the **Inner Apply** and **Both Applys**'s time.

```bash
cd k-means
echo k-Means Inner Apply Vec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply_mtrans_level2.R 1000000 10 15 | ../../../valor/report.py
echo k-Means Both Applys Vec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply_mtrans_opt.R 1000000 10 15 | ../../../valor/report.py
echo k-Means-nD Inner Apply Vec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply_mtrans_level2.R 1000000 10 3 15 | ../../../valor/report.py
echo k-Means-nD Inner Applys Vec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply_mtrans_opt.R 1000000 10 3 15 | ../../../valor/report.py
cd ..
cd k-NN
echo NN Inner Apply Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply_mtrans_level2.R 10000 10000 10 | ../../../valor/report.py
echo NN Both Applys Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply_mtrans.R 10000 10000 10 | ../../../valor/report.py
echo k-NN Inner Apply Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply_mtrans_level2.R 10000 10000 10 5 | ../../../valor/report.py
echo k-NN Both Applys Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply_mtrans.R 10000 10000 10 5 | ../../../valor/report.py
cd ..
```

The above script is stored as bench_nested.sh

The current compiler in valor package doesn't support the two testing schemas transformation. The scripts used in the test are manually generated.    

### Vector Programming in applications Evaluation (Paper Section 5.6)

The benchmarks LR-n, LogitReg-n, k-Means-nD, LR-OLS_n can change the value of n to test the use of vector programming in the original code. Here, the LR is used as the example. 

```bash
cd LR
echo LR-n 1M samples, 15 iterations. n = 1
Rscript --vanilla LR_lms_lapply.R 1000000 1 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 1 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 2
Rscript --vanilla LR_lms_lapply.R 1000000 2 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 2 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 3
Rscript --vanilla LR_lms_lapply.R 1000000 3 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 3 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 4
Rscript --vanilla LR_lms_lapply.R 1000000 4 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 4 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 5
Rscript --vanilla LR_lms_lapply.R 1000000 5 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 5 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 6
Rscript --vanilla LR_lms_lapply.R 1000000 6 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 6 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 7
Rscript --vanilla LR_lms_lapply.R 1000000 7 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 7 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 8
Rscript --vanilla LR_lms_lapply.R 1000000 8 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 8 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 9
Rscript --vanilla LR_lms_lapply.R 1000000 9 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 9 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 10
Rscript --vanilla LR_lms_lapply.R 1000000 10 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
cd ..
```
Here, the n is the 2nd parameter in the command line. In LogitReg-n, k-Means-nD, and LR-OLS_n, the n values are always the 2nd parameer. You can simplify duplicate the commands used in Section5.3/5.4 evaluation to reprodcue the test.

The above script is stored as bench_veclen.sh.

### Tiling in Vectorization (Paper Section 5.7)

The current valor package does not contain the feature to automatically generate vectorized code with different tiling size. The tiling experiment was based on manually translated code. Here, LR (1var) is used to show the steps to reproduce the result.

```bash
cd LR
echo LR 1M samples, 15 iterations. tile = [1000 2000 5000 10000 20000 50000 100000 200000 500000]
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 1000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 2000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 5000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 10000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 20000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 50000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 100000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 200000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 500000 | ../../../valor/report.py
echo LR-n 1M samples with n = 10, 15 iterations. tile = [1000 2000 5000 10000 20000 50000 100000 200000 500000]
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 1000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 2000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 5000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 10000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 20000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 50000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 100000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 200000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 500000 | ../../../valor/report.py
cd ..
```

The above script is stored as bench_tiling.sh

## V. Brief Introduction of the source code of valor package.

### Valor Package

The source code is under pkg sub-directory, where the structure follows standard R package structure. 

| Filename |  Notes |
|----------|--------|
|man\vecapply.Rd| help document used in R ?va_cmpfun command |
|R\cmputility.R| Valor Compiler utility functions |
|R\runtime.R| Valor runtime function implementations, such as PERM\_DOWN(), PERM\_UP() implementations|
|R\vecapply.R| Valor compiler main functions|
|tests\basics.R| Simple tests of Valor package|


### Benchmarks

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
