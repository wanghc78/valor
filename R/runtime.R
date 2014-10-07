# Author: Haichuan Wang (hwang154@illinois.edu)
#
# vecruntime.R provides all the runtime functions used by vecapply package
###############################################################################

############## Runtime functions ##############
# All VecApply runtime functions (exported) has a prefix "va_"

# Return the length of the vector
# v must be a List(SoA)       --> return the length of the list
#        or a multi-dim array --> return the first dim size
va_vecLen <- function(v) { #v must be vec, or SoA
    if(is.list(v)){
        va_vecLen(v[[1]])
    } else {
        v_dim <- dim(v) 
        if(is.null(v_dim)) {
            length(v)
        } else {
            v_dim[1]
        }
    }
}

# Transform a low dimension data into the high dimension data by replication
# The input data cannot be list. But the ref could be
va_repVecData <- function(data, ref) {
    stopifnot(is.atomic(data)) #input must be atomic
    n <- va_vecLen(ref) #need calculate the ref's length
    
    if(n > 1) {
        vdata <- rep(data, each=n)
        data_len = length(data)
        if(data_len > 1) { #then the result is an array
            if(is.array(data)) {
                dim(vdata) <- c(n, dim(data))
            } else {
                dim(vdata) <- c(n, data_len)
            }
        }
        vdata
    } else {
        data # no need rep
    }
}

# Transformation a low dimension data into the dimension data by replicaiton, but expand 2nd dim
va_colRepVecData <- function(data, ref) {
    stopifnot(is.atomic(data)) #input must be atomic
    n <- va_vecLen(ref) #need calculate the ref's length
    
    if(n > 1) {
        if(is.array(data)) {
            #very complex
            vdata<-apply(data, 2, function(acol){rep.int(acol, n)})
            dimV <- dim(data)
            dimSz <- length(dimV)
            dim(vdata) <- c(dimV[1], n, dimV[2:dimSz])
        } else {
            #scalar or vector
            vdata <- rep.int(data, n)
            data_len <- length(data)
            if(data_len > 1) {
                dim(vdata) <- c(data_len, n)
            }
        }
        vdata
    } else {
        data # no need rep
    }
}

# Transform a vector to a list element. each element in the list is by removing the 1st dim of the vector
# Will do it recursively
va_vec2list <- function(v) {
    if(is.list(v)) {
        v_len <- length(v)
        v_attr <- attributes(v)
        #transform each elements into list
        for(i in 1:v_len){
            v[[i]] <- va_vec2list(v[[i]])
        }
        l_len <- length(v[[i]])
        #then compose small one
        aos <- lapply(1:l_len, function(i){ 
                    e <- list()
                    for(ei in 1:v_len){
                        e[[ei]] <- v[[ei]][[i]]                  
                    }
                    attributes(e) <- v_attr
                    e
                })
        return(aos)
    }
    
    
    #only support v as vector or matrix
    #TODO: think about how to access the 1st slice of a multi-dim array
    v_dim <- dim(v) 
    if(is.null(v_dim)) {
        n <- length(v)
        lapply(1:n, function(i) v[i])
    } else {
        n <- v_dim[1]
        lapply(1:n, function(i) v[i,])
    }   
}

# Transform a list into a vector form. 
# It also handles transform of a AoS into SoA
va_list2vec <- function(l) {
    stopifnot(length(l) > 0)
    e1 <- l[[1]] #pick the first one
    if(is.list(e1)) {
        len_e <- length(e1)
        soa <- list() #the result
        for(i in 1:len_e) {
            soa[[i]] <- va_list2vec(lapply(l, function(e){e[[i]]}))
        }
        attributes(soa) <- attributes(e1)
        return(soa)
    }
    
    #then e1 is atomic type
    #only support v as vector or matrix
    tmp <- simplify2array(l)
    # if is.list(tmp) { warning, and return the list }
    if(is.null(dim(tmp))) {
        tmp
    } else if(is.matrix(tmp)) { #matrix
        t(tmp)
    } else { #multidim array
        tmp_dim_len <- length(dim(tmp))
        aperm(tmp, c(tmp_dim_len, 1:(tmp_dim_len-1)))
    }
}

# Get the nth element in a vector data. We should get the element from the last dim
# Because the vectorization is always done in the 1st dim

va_getByIdx <- function(v, idx) {
    #if v is a vector, return [idx]
    #if v is a matrix, return [,idx]
    #if v is a 3D arra, [,,idx]
    v_dim <- dim(v) 
    if(is.null(v_dim)) {
        v[idx]
    } else {
        if(length(v_dim) == 2) {
            v[,idx]
        } else {
            stopifnot(length(v_dim) == 3)
            v[,,idx]
        }
    }
}


# A very simple way to vectorize a function.
va_rawVecFun <- function(fun) {
    function(v) {
        if(is.array(v)) { 
            apply(v, 1, fun)
        } else {
            sapply(v, fun)
        }
    }
}


# A real vectorizatoin function transformer
# dim: how many dimensions for the current transformation context, a pure runtime
# Still this function is external facing, user will see the result
va_vecClosure <- function(clos, options = NULL) {
    type <- typeof(clos)
    
    if (type == "closure") {
        body = body(clos)
        forms = formals(clos)
        cntxt <- make.toplevelContext(makeCenv(environment(clos)), options)
        ncntxt <- make.vecfunctionContext(cntxt, forms, body)
        #add all formals as vecvars
        ret <- veccmp(body, ncntxt)
        body(clos) <- ret[[1]]
        cat("[va_vecFun: result]"); print(clos)
        clos
    }
    else if (typeof(clos) == "builtin" || type == "special") { 
        #still need to check, if the function supports vec input. otherwise, return a raw vectorize form
        clos
    }
    else { stop("cannot vectorize a non-function") }
}

# Target function of Reduce('+', aList), Apply(sum, ...)
va_reduceSum <- function(v) {
    if(is.list(v)) {Reduce('+', v)} #just for safe
    if(is.array(v)) { colSums(v) }
    else { sum(v) }
}

# Target function of Apply(mean,...)
va_reduceMean <- function(v) {
    if(is.array(v)) { colMeans(v) }
    else { mean(v) }
}


