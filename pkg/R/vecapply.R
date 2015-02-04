# Author: Haichuan Wang (hwang154@illinois.edu)
###############################################################################


## Name convention
## va_*: all external exposure functions, including runtime functions and compiler user interface
## small character started camelCase names: compiler internal function names
##   veccmp*: Used for vectorization compiler
## Large character started CamelCase names: data structures used by compiler functions







#input ret[[3]], 
#return if ret[[3]] is true, then insert list2vec wrapper
#otherwise 
getRetVal <- function(ret) {
    if(ret[[3]]) as.call(c(quote(va_vec2list), ret[[1]])) 
    else ret[[1]]
}

#input a list. If it is not a symobl, just add the wrapper
#if it is a symbol gen
# { if(! (exists(".va.listVal", inherits = FALSE) 
#         && (is.null(.vasrc.listVal) || identical(.vasrc.listVal, listVal))) ) {
#      .va.listVal <- va_list2vec(listVal)
#      .vasrc.listVal <- listVal
#   }
#   .va.listVal
# }
#
# Hanle a scalar function in the similar way except the wrapper is va_vecClosure

genVecObjectNode <- function(listVal, isData = TRUE) {
    transfun <- if(isData) {quote(va_list2vec) } else { quote(va_vecClosure) }
    if(is.symbol(listVal)) {
        listVal_name <- as.character(listVal)
        vecValName <- paste(".va", listVal_name, sep='.')
        vecValSym <- as.symbol(vecValName)
        vecSrcSym <- as.symbol(paste(".vasrc", listVal_name, sep='.'))
        ret <- quote({
                    if(! (exists(".va.listVal", inherits = FALSE) 
                          && (is.null(.vasrc.listVal) || identical(.vasrc.listVal, listVal))) ) {
                        .va.listVal <- va_list2vec(listVal)
                        .vasrc.listVal <- listVal
                    }
                    .va.listVal
                }
        )
        #now directly modify the quote's AST
        ret[[2]][[2]][[2]][[2]][[2]][[2]] <- vecValName #exisit
        ret[[2]][[2]][[2]][[2]][[3]][[2]][[2]][[2]] <- vecSrcSym #is.null node
        ret[[2]][[2]][[2]][[2]][[3]][[2]][[3]][[2]] <- vecSrcSym #identical arg1
        ret[[2]][[2]][[2]][[2]][[3]][[2]][[3]][[3]] <- listVal #identical arg2
#        if(!isData) { #function node, remove is.null check
#            ret[[2]][[2]][[2]][[2]][[3]] <- ret[[2]][[2]][[2]][[2]][[3]][[2]][[3]]
#        }
        ret[[2]][[3]][[2]][[2]] <- vecValSym  #if block, 1st assign's LHS
        ret[[2]][[3]][[2]][[3]][[1]] <- transfun #if block, 1st assign's RHS fun
        ret[[2]][[3]][[2]][[3]][[2]] <- listVal #if block, 1st assign's RHS fun
        ret[[2]][[3]][[3]][[2]] <-  vecSrcSym
        ret[[2]][[3]][[3]][[3]] <- listVal
        ret[[3]] <- vecValSym
        ret
    } else {
        as.call(list(transfun, listVal))   
    }
}

# SparkR's data vectorization should be
# { if(! (exists(".va.listVal", inherits = FALSE) 
#         && (is.null(.vasrc.listVal) || identical(.vasrc.listVal, listVal))) ) {
#                .va.listVal <- cache(lapplyPartition(listVal,
#                                                    va_list2vec)) # cache here
#                .vasrc.listVal <- listVal
#            }
#            .va.listVal
#} 
genSparkRVecDataNode <- function(listVal) {
    transfun <- quote(va_list2vec)
    if(is.symbol(listVal)) {
        listVal_name <- as.character(listVal)
        vecValName <- paste(".va", listVal_name, sep='.')
        vecValSym <- as.symbol(vecValName)
        vecSrcSym <- as.symbol(paste(".vasrc", listVal_name, sep='.'))
        ret <- quote({ if(! (exists(".va.listVal", inherits = FALSE) 
                                && (is.null(.vasrc.listVal) || identical(.vasrc.listVal, listVal))) ) {
                        .va.listVal <- cache(lapplyPartition(listVal,
                                        va_list2vec)) # cache here
                         .vasrc.listVal <- listVal
                       }
                       .va.listVal
                     })
        #now directly modify the quote's AST
        ret[[2]][[2]][[2]][[2]][[2]][[2]] <- vecValName #exisit
        ret[[2]][[2]][[2]][[2]][[3]][[2]][[2]][[2]] <- vecSrcSym #is.null node
        ret[[2]][[2]][[2]][[2]][[3]][[2]][[3]][[2]] <- vecSrcSym #identical arg1
        ret[[2]][[2]][[2]][[2]][[3]][[2]][[3]][[3]] <- listVal #identical arg2
        ret[[2]][[3]][[2]][[2]] <- vecValSym
        ret[[2]][[3]][[2]][[3]][[2]][[3]] <- transfun 
        ret[[2]][[3]][[2]][[3]][[2]][[2]] <- listVal
        ret[[2]][[3]][[3]][[2]] <-  vecSrcSym
        ret[[2]][[3]][[3]][[3]] <- listVal
        ret[[3]] <- vecValSym
        ret
    } else {
        #lapplyPartition(listVal, va_list2vec)
        as.call(list(as.symbol("lapplyPartition"), listVal, transfun))   
    }
}

# input: aList is the list from a symbol or an expression
# the problem here, aList may be a var's name, 
#  or a var's name wrapped inside va_vec2list. Reason: a lapply result is directly sent to the consumer(e.g. Reduce)
tryGetVecDataBinding <- function(aList, cntxt) {
    vecval <- NULL
    
    #if aList is a symbol, try to lookup the symbol in the pool first
    if(typeof(aList) == 'symbol') {
        aList = cntxt$localbindings[[as.character(aList)]]
    }
    
    #now aList is an expression either va_vec2list(.va.list_dara)
    # or lapplyPartition(.va.list_data, va_vec2list)
    if(!is.null(aList)  && typeof(aList) == "language"){
        fun_name <- as.character(aList[[1]])
        if(fun_name == "va_vec2list") {
            vecval <- aList[[2]]
        } else if(fun_name == "lapplyPartition" && as.character(aList[[3]]) == "va_vec2list"){
            vecval <- aList[[2]]
        }
    }
    vecval
}

## Scalar Transformation
## Each return includes [updatedNode, updatedCntxt, isDenseData]
## updatedNode: the new AST node
## updatedCntxt: mainly care about the localbindings
## isDenseData: TRUE/FALSE. after lapply->directly call: the value is changed to true
applycmp<- function(e, cntxt) {
    if (typeof(e) == "language")
        applycmpCall(e, cntxt)
    else 
        list(e, cntxt, FALSE) # nothing changed in the scalar space
}

#It's the scalar context, only care about the lapply/reduce transformation
applycmpCall <- function(call, precntxt) {   
    cntxt <- make.callContext(precntxt, call) #current context
    log_info(cntxt, "[ScalarVisit]Call:", paste(format(call), collapse = "\n"))
    
    fun <- call[[1]]; fun_name <- as.character(fun)
    args <- call[-1]
    
    isDenseData <- FALSE #only lapply will change it
    
    if("lapply" == fun_name && length(args) == 2
        && (isBaseVar("lapply", cntxt) || isSparkRVar("lapply", cntxt))
        && ( typeof(args[[2]]) == "language" 
             || ! as.character(args[[2]]) %in% UnsupportedFuns)) {
        # args[[2]] may be language 
        #in this case, the lapply's argument, input maybe another lapply expr or symbol

        isSparkRContext <- isSparkRVar("lapply", cntxt)
        ## handle data
        ret <-  applycmp(args[[1]], cntxt)
        l <- getRetVal(ret) #wrap vec2list ondemand
        cntxt <- ret[[2]]
        
        vecval <- tryGetVecDataBinding(l, cntxt) #try to get the vec rep
        if(is.null(vecval)) {
            if(isSparkRContext == TRUE){
                vecval <- genSparkRVecDataNode(l)
            } else {
                vecval <- genVecObjectNode(l, isData = TRUE)      
            }         
        }
        
        ## handle function function
        sf <- args[[2]] #scalar function
        sf_name <- if(typeof(sf) == "language") {"_anonymousFun"} else{ as.character(sf) }
        
        # handle either lapply in base package or in SparkR package
        if(! (isSparkRContext == TRUE)) { #base case
            if(sf_name %in% VectorInputFuns) {
                #use special handling to denseData as input
                # idea: if the function take vector as input (sum, which.min, etc.), then vec2list's src must be SoA
                #       then, just do simplify2array, and apply with dim = 1 to get the result
                # This is only workable for basevar's lapply, not for sparkR
                
                #wrap the vecval into simplify2array #wrap it into apply
                vecval <- as.call(list(as.symbol("simplify2array"), vecval))
                call <- as.call(list(as.symbol("apply"), vecval, 1, sf))
                isDenseData <- FALSE 
            } else {
                # use vf to process
                vf <- genVecFunNode(sf, cntxt)
                call <- as.call(list(vf, vecval))
                isDenseData <- TRUE #the only place change the list rep into vec rep                
            }
        } else { # in sparkR package
            vf <- genVecFunNode(sf, cntxt)
            call <- as.call(list(as.symbol("lapplyPartition"), vecval, vf))
            isDenseData <- TRUE #the only place change the list rep into vec rep                   
        }
    } else if(fun_name == "Reduce" && isBaseVar(fun_name, cntxt) 
            && args[[1]] == '+' #only support Reduce add
            && (length(args) == 2 || length(args) == 3 && args[[3]] == 0)) {
        #no need transform args[[1]]
        #transform args[[2]]
        ret <- applycmp(args[[2]], cntxt) 
        l <- getRetVal(ret) #may be wrapped 
        cntxt <- ret[[2]] #update local context
        vecval <- tryGetVecDataBinding(l, cntxt)
                
        if(is.null(vecval)) {
            call <- as.call(c(fun, '+', l))
        } else {
            #replace with va_reduceSum(vecval)
            call <- as.call(c(quote(va_reduceSum), vecval))
        }
    } else if(fun_name == "reduce" && isSparkRVar(fun_name, cntxt)) {
        #SparkR's reduce is reduce(rdd, func)
        #no need transform args[[2]], the func
        #transform data args[[1]]
        ret <- applycmp(args[[1]], cntxt) 
        l <- getRetVal(ret) #may be wrapped 
        cntxt <- ret[[2]] #update local context
        vecval <- tryGetVecDataBinding(l, cntxt)
        
        if(is.null(vecval)) { #no need to use lapplyPartition to rewrite
            call <- as.call(c(fun, l, args[[2]]))
        } else {
            #Must transform the data to lapplyPartition with reduce
            partReduceNode <- quote(lapplyPartition(data, 
                                                    function(vData) { 
                                                     list(va_reduce('+', vData))
                                                    })
                                    )
            #change the node
            partReduceNode[[2]] <- vecval #process the data
            partReduceNode[[3]][[3]][[2]][[2]][[2]] <- args[[2]] #change the op
            #gen final node
            call <- as.call(c(fun, partReduceNode, args[[2]]))
        }
    } else if(fun_name == "countByKey" && isSparkRVar(fun_name, cntxt)) {
        print("checked countByKey")
        #Transform count by key: countByKey(rdd)
            ret <- applycmp(args[[1]], cntxt) 
            l <- getRetVal(ret) #may be wrapped
            cntxt <- ret[[2]]
            vecval <- tryGetVecDataBinding(l, cntxt)
            if(is.null(vecval)) {
                print("veccall is null")
                call <- as.call(c(fun, l)) # no need change anything here
            } else {
                #use 
                call <- quote(collect(reduceByKey(lapplyPartition(inputData, va_countByKey), "+", numPartitions(inputData))))
                call[[2]][[2]][[2]] <- vecval #the lapplyParition's input
                call[[2]][[4]][[2]] <- vecval #the numPartitions' input
            }
    
    } else if(fun_name == "reduceByKey" && isSparkRVar(fun_name, cntxt)) {
        print("checked reduceByKey")
        #Transform count by key: reduceByKey(rdd, op, numPart)
        ret <- applycmp(args[[1]], cntxt) 
        l <- getRetVal(ret) #may be wrapped
        cntxt <- ret[[2]]
        vecval <- tryGetVecDataBinding(l, cntxt)
        
        #no need visit args[[2]] the op 
        # visit args[[3]]
        ret <- applycmp(args[[3]], cntxt)
        args[[3]] <- getRetVal(ret) #may be wrapped
        cntxt <- ret[[2]]
        
        if(is.null(vecval)) {
            call <- as.call(c(fun, l, args[[2]], args[[3]])) # no need change anything here
        } else {
            #use 
            call <- quote(
                    reduceByKey(lapplyPartition(inputData, function(part){va_reduceByKey(part, op)}), op, numPart))
            call[[2]][[2]] <- vecval #the lapplyParition's input
            call[[2]][[3]][[3]][[2]][[3]] <- args[[2]] # the op in the annonymous fun
            call[[3]] <- args[[2]] # outer op
            call[[4]] <- args[[3]] # outer numPart
        }
    } else if(fun_name == "<-" || fun_name == "=") {
        #only handle simple case
        #the police is if the ret[[3]] is TRUE, then we need create an wrapper
        #  { .va.left <- noWrapVector, change the assign to delayedAssign}
        
    
        lhs <- getAssignedVar(call)
        ret <- applycmp(args[[2]], cntxt)
        rhs <- getRetVal(ret)
        cntxt <- ret[[2]]
        if (ret[[3]]) {  #dense value returned
            #create a temp variable .va.lhsname
            #note the delayed assign's 2nd para(the value) should also defne .vasrc.data
            tmpVarName <- paste(".va", lhs, sep=".") #.va.lhs name
            tmpVarSrcName <- paste(".vasrc", lhs, sep=".") #.vasrc.lhs name
            #Gen dense expr assign
            tmpAssignStmt <- as.call(c(quote(`<-`), as.symbol(tmpVarName), ret[[1]]))
            #Gen clean the src stmt .vasrc.lhs <- NULL
            cleanSrcStmt <- as.call(list(quote(`<-`), as.symbol(tmpVarSrcName), quote(NULL)))
            #Gen the real delayed assign
            # delayedAssign("lhs", .vasrc.lhs <- vec2list(.va.lhs))
            # the reason of RHS is an expr: update .varsc.lhs
            delayAssignValRHS <- as.call(c(quote(va_vec2list), as.symbol(tmpVarName)))
            # Hack for SparkR context
            if(isSparkRVar("lapply", cntxt)) { #the lapply is from sparkR
                # delayedAssign("lhs", .vasrc.lhs <- lapplyParition(.va.lhs, vec2list)))
                delayAssignValRHS <- as.call(c(as.symbol("lapplyPartition"), as.symbol(tmpVarName), quote(va_vec2list)))
            }
            
            delayAssignValStmt <- as.call(c(quote(`<-`), as.symbol(tmpVarSrcName), delayAssignValRHS))
            delayAssignStmt <- as.call(c(quote(delayedAssign), lhs, delayAssignValStmt))
            #finally the wrapper "{"
            call <- as.call(c(quote(`{`), tmpAssignStmt, cleanSrcStmt, delayAssignStmt))
            #and the binding is 
            cntxt$localbindings[[lhs]] <- delayAssignValRHS #add to the vec2list result as binding
        } else {
            cntxt$localbindings[[lhs]] <- rhs #add one binding
            call <- as.call(c(fun, args[[1]], rhs))
        }
    } else if(fun_name == "assign" && isBaseVar("assign", cntxt)
            || fun_name == "delayedAssign" && isBaseVar("delayedAssign", cntxt)) {
        #only handle simple case
        if(length(args) != 2 || !is.character(args[[1]]) || length(args[[1]]) != 1){
            cntxt$stop(gettext("cannot handle binding of complex assign or delayedAssign function"),
                    cntxt)
        }
        lhs <- args[[1]]
        ret <- applycmp(args[[2]], cntxt) #the expr
        rhs <-getRetVal(ret)
        cntxt <- ret[[2]]
        cntxt$localbindings[[lhs]] = rhs
        call <- as.call(c(fun, args[[1]], rhs))
    }
    else if(fun_name == "function") {
        #apply transformation will not directly handle nested call
        #Nothing
    }
    else { #other general function, not control
        #must visit each args, but not the call
        controlCntxt <- list()
        for (i in seq_along(args)) {
            ret <- applycmp(args[[i]], cntxt)
            args[[i]] = getRetVal(ret) #note the second one is the dim size
            if(fun_name == "if"){
                if(i == 1) {
                    cntxt <- ret[[2]]
                    controlCntxt <- list(cntxt) # the base
                } else {
                    #not update cntxt, use prebase
                    controlCntxt <- c(controlCntxt, ret[[2]])
                }
            } else if(fun_name == "for") {
                if(i <= 2) {
                    cntxt <- ret[[2]]
                    controlCntxt <- list(cntxt)
                } else{
                    cntxt <- ret[[2]]
                    controlCntxt <- c(controlCntxt, cntxt)
                }
            } else if(fun_name == "while") {
                if(i == 1) {
                    cntxt <- ret[[2]]
                    controlCntxt <- list(cntxt)
                } else{
                    cntxt <- ret[[2]]
                    controlCntxt <- c(controlCntxt, cntxt)
                }
            } else{
                cntxt <- ret[[2]]
            }
        }
        call <- as.call(c(fun, as.list(args)))
        #clean the localdefs because of the contro flow
        #TODO: a better way is to do localdefs merge. if there are different binding, remove that binding
        if(length(controlCntxt) >= 1) {
            cntxt <- controlCntxt[[1]]
        }
    }
    list(call, cntxt, isDenseData)
}


## Recursive vec compile structure - The entrance of the vec compilation
## e: expr
## cntxt: compile context
## This is the top level visiting function. The value returned is a list of 3
## ret[[1]] the transformed object
## ret[[2]] the new context, with the new vector variables listed in 
## ret[[3]] The AST object is belong to the vector world or scalar world
##    0: scalar world; 1: vector with 1 dim; 2: vector with two dim space 
veccmp <- function(e, cntxt) {
    if (typeof(e) == "language")
        veccmpCall(e, cntxt)
    else if (typeof(e) == "symbol")
        veccmpSym(e, cntxt)
    else if (typeof(e) == "pairlist" || typeof(e) == "NULL") #note empty args
        veccmpFormals(e, cntxt)
    else if (typeof(e) == "bytecode")
        cntxt$stop(gettext("cannot vec compile byte code literals in code"),
                cntxt)
    else if (typeof(e) == "promise")
        cntxt$stop(gettext("cannot vec compile promise literals in code"),
                cntxt)
    else
        veccmpConst(e, cntxt)
}

# Mapping low dim built-in functions into high dim built-in functions
# It only lists functions that low dim and high dim are different 
BuiltinVecFuns <- list(
        c = "cbind",
        sum = "rowSums",
        mean = "rowMeans",
        unlist = "simplify2array",
        tcrossprod = "va_tcrossprod",
        which.min = "va_which.min",
        which.max = "va_which.max"
        )
#These function by default support replication expansion. So no need do manu repExpand        
ImplicitRepFuns <- c("+", "-", "*", "/", "%/%","^", "%%", ">", ">=", "<", "<=", "!=", "==", "cbind")

#Used for building the simple Basic Block's defs binding calculation
ControlFlowFuns <- c("for", "if", "repeat", "while", "return", "switch")

#Unsupported Funcs that cannot be vectorized
UnsupportedFuns <- c(".Internal", ".External", "names", "table")

#Not vectorize these funs with languageFuns
SupportedFuns <- c("lapply", "tanh")

# Vector object as input functions, but has no direct higher function mapping available for lapply
VectorInputFuns <- c("min", "max", "order")

# Get the AST Node for the vectorized version of the function (symbol or real object)
# only expand one dimension right now. Called by handling fun symobl transformation
# 
# if fun is a symbol
#  if the symbol is belong to language symbol, return directly
#  otherwise return a wrapper va_vecFun(fun) which will trigger a runtime call
# if fun is a closure, just call the va_vecFun(closure)
genVecFunNode <- function(fun, cntxt) {
    
    if (typeof(fun) == 'symbol') {
        #either return a wrapper, or return the real object
        name <- as.character(fun)
        #Risk %*% function cannot be handled simplily
        if (name %in% languageFuns || name %in% SupportedFuns) { 
            fun #generally it will not happen
        } else if(!is.null(BuiltinVecFuns[[name]])) {
            as.symbol(BuiltinVecFuns[[name]])
        }
        else {
            genVecObjectNode(fun, isData = FALSE) #get the wrapper va_vecClosure() with runtime binding
        }
    } else if (typeof(fun) == 'closure') { # a real runtime object
        va_vecClosure(fun) #note here vecClosure will only return the closure
    } else if (typeof(fun) == 'language' && as.character(fun[[1]])== 'function') {
        # It's a static input function object, so just get the object and do compiling time binding
        va_vecClosure(eval(fun))
    }
    else {
        cntxt$stop(gettext("cannot vec a function that is neither a symbol nor a closure"),
                cntxt)
    }
}

veccmpSym <- function(sym, cntxt) {
    log_info(cntxt, "[VecVisit]Symbol:", sym)
    if(identical(sym, EmptySymbol())) { return(list(sym, cntxt, 0L)) }
    #try to search this symbol in the context's environment.
    #If it is a vector var, return dim = 1, otherwise return 0
    if (isVecVar(sym, cntxt)) {
        list(sym, cntxt, 1L) # a vectorized one
    } else {
        list(sym, cntxt, 0L)  # a scala one
    }
    
}

veccmpConst <- function(val, cntxt) {
    #constant definetely 
    log_info(cntxt, "[VecVisit]Constant:", paste(format(val), collapse = "\n"))
    list(val, cntxt, 0L) #the second is the dim of this constant
}

veccmpFormals <- function(forms, cntxt) {
    log_info(cntxt, "[VecVisit]Formals:", names(forms), " <-> ", as.character(forms))
    list(forms, cntxt, 0L) #not change forms right now. maybe need vectorize the binding values
}


#There are basically two types of compile call actions
#  In scalar space: identify lapply, and transform it into direct function call
#  In the vector space, follow the formal's def-use chain, transform all operations into vector form
veccmpCall <- function(call, precntxt) {
    cntxt <- make.callContext(precntxt, call)
    log_info(cntxt, "[VecVisit]Call:", paste(format(call), collapse = "\n"))
    
    fun <- call[[1]]; fun_name <- as.character(fun)
    args <- call[-1]
    
    #tmp<-findExprVecVarUse(call, cntxt)
    #will distinguish it is an assign.
    # If yes, transfor RHS, 
    #    If RHS is vec, add the LHS into vector list
    #
    # If no. normal transform
    if (fun_name %in% UnsupportedFuns) {
        #unsupported functions, then first need to check the vector symbol usage of the expression
        #then wrapper it as a function, with a mapply
        log_warn(cntxt, "  ==>>Call Unsupported fun, gen generic vec wrapper for: ", fun_name)
        vecvars <- findExprVecVarUse(call, cntxt) #note typically there is no write
        if(length(vecvars) > 0) {
            #build the function, with the whole call
            wrapfun <- wrapExprToFun(vecvars, call)
            
            if(length(vecvars) == 1) {
                #use runtime function  va_vecApplyWrapper(vecvars, wrapfun)
                args<- list(as.call(c(quote(va_vecApplyWrapper), vecvars, wrapfun)))
            } else {
                #now wrap vectors to vec2list
                for(i in seq_along(vecvars)) {
                    vecvars[[i]] <- as.call(c(quote(va_vec2list), vecvars[[i]]))
                }
                args<- list(as.call(c(quote(mapply), wrapfun, vecvars)))
            #return the mapply. 
            }
            # have to insert another level list2vec
            fun <- quote(va_list2vec)
            vecFlag <- 1L
        } else {
            vecFlag <- 0L
        }
        
    } else if (fun_name == '=' || fun_name == '<-') {

        #transform RHS first
        ret <- veccmp(args[[2]], cntxt)
        cntxt <- ret[[2]] #update local context
        vecFlag <- ret[[3]] #from the assign
        args[[2]] <- ret[[1]]
        
        #now build the link if required
        lhsSym <- getAssignedVar(call)
        if(is.symbol(args[[1]])) { #lhs is a symbol
            cntxt$localbindings[[lhsSym]] <- args[[2]] #note here the link is built with the wrapper
            if(vecFlag) { cntxt <- addCenvVecvars(cntxt, lhsSym) }
        } else { #lhs is a call
            # handle a speical case, the rhs is vecFlag = FALSE, but lhs var is a vector
            # 20141004 ?? Why this situation happens. Need a flag
            
            expectedDim <- 0L # a flag to indicate whether to insert va_repVecDataOnDemand
            if(!vecFlag && isVecVar(lhsSym, cntxt)) {
                refvar <- as.symbol(cntxt$dimvars[1])
                #?? Do we really need the va_repVecData here. R supports replication during assign
                # only if args[[2]] is a real-scalar case, there is no need to do replication.
                #args[[2]] <- as.call(list(quote(va_repVecData), args[[2]], refvar))
                vecFlag <- 1L
            } else if(vecFlag && !isVecVar(lhsSym, cntxt)) {
                #Need handle special cases e.g.
                #   scalar code: y <- integer(5). then y[1] <- a
                cntxt <- addCenvVecvars(cntxt, lhsSym)
                # check the dim access of lhs
                expectedDim <- length(args[[1]]) - 2 + 1 #remove '[' and 'sym', add additional dim 
            }
            #should handle lhs is a symbol or lhs is a call. e.g. x[1]
            ret <- veccmp(args[[1]], cntxt)
            args[[1]] <- ret[[1]]
            cntxt <- ret[[2]] #update local context
            
            if(expectedDim > 0) { #insert the stmt
                refvar <- as.symbol(cntxt$dimvars[1])
                expandStmt <- as.call(c(quote(va_repVecDataOnDemand), as.symbol(lhsSym), expectedDim, refvar))
                expandAssignStmt <- as.call(c(quote(`<-`), as.symbol(lhsSym), expandStmt))
                args[[2]] <- as.call(c(quote(`{`), expandAssignStmt, args[[2]]))
            }
        }
    } else if (fun_name == "assign" && isBaseVar("assign", cntxt)
            || fun_name == "delayedAssign" && isBaseVar("delayedAssign", cntxt)) {
        if(length(args) != 2 || !is.character(args[[1]]) || length(args[[1]]) != 1){
            cntxt$stop(gettext("cannot vec an complex assign or delayedAssign function"),
                cntxt)
        }
        lhsSym <- args[[1]]
        ret <- veccmp(args[[2]], cntxt)
        cntxt <- ret[[2]] #update local context
        if(ret[[3]]) { #now the LHS should be added into the vecVars
            vecFlag <- 1L
            cntxt <- addCenvVecvars(cntxt, lhs)
        } else {
            vecFlag <- 0L
        }
        cntxt$localbindings[[lhsSym]] <- ret[[1]]
        args[[2]] <- ret[[1]] #in all cases, the lhs will not be transformed
    } else if (fun_name == "[") { 
        #e.g. x[1] ('['(x,1) )or x[1,2]
        dimsret <- integer(length(args))
        for (i in seq_along(args)) {
            ret <- veccmp(args[[i]], cntxt)
            cntxt <- ret[[2]] #update local context
            args[[i]] <- ret[[1]] #note the second one is the dim size
            dimsret[i] <- ret[[3]]
        }
        
        if(dimsret[1]) { #val is vectorized
            #then all indices cannot be vectorized
            if(sum(dimsret) > 1L) {
                cntxt$stop(gettext("cannot vec an index call with vec-val and any index is vectorized"),
                        cntxt)
            } 
            # now try to insert an empty index in [[2]]
           args[3:(length(args)+1)] <- args[2:length(args)]
           args[[2]] <- EmptySymbol()
           vecFlag <- 1L
        } else { #not vectorized, relatively simple
            #then at most one index can be vectorized
            if(sum(dimsret) > 1L) {
                cntxt$stop(gettext("cannot vec an index call with non-vec-val but more than one index is vectorized"),
                        cntxt)
            } else if(sum(dimsret) == 1L) {
                vecFlag <- 1L
            } else {
                vecFlag <- 0L
            }
        }
    } else if(fun_name == "[["){
        #only consider args[[1]] and args[[2]]
        # val[[idx]]: 4 combinations. 
        #  val scalar, idx: scalar. nothing
        #  val scalar, idx: vector. ==> vectorize val, use [] to replace [[]]. return vector
        #  val vector, idx scalar. no need change
        #  val vector, idx vector ==> cannot support right now

        ret <- veccmp(args[[1]], cntxt)
        args[[1]] <- ret[[1]]
        cntxt <- ret[[2]]
        valVecFlag <- ret[[3]]
        
        ret <- veccmp(args[[2]], cntxt)
        args[[2]] <- ret[[1]]
        cntxt <- ret[[2]]
        idxVecFlag <- ret[[3]]
        
        if(valVecFlag > 0) { #val is changed to vector object
            if(idxVecFlag > 0) {
                cntxt$stop(gettext("[Error]Cannot handle [[ vector index of a vector list"),
                        cntxt)
            }
            #no need change
            vecFlag <- valVecFlag
        } else { #val still scalar object
            if(idxVecFlag > 0) {
                #must first wrap args[[1]] with data wrapper
                fun <- quote(`[`) #change fun to []
                #And should consider dynamic linkage optimization
                args[[1]] <- genVecObjectNode(args[[1]], isData = TRUE)
            }
            vecFlag <- idxVecFlag
        }
    } else if(fun_name == "$"){
        #only consider args[[1]] and args[[2]]
        #args[[2]] must be a scalar, other wise cannot handle
        valret <- veccmp(args[[1]], cntxt)
        args[[1]] <- valret[[1]]
        cntxt <- valret[[2]]
        vecFlag <- valret[[3]]
        
        idxret <- veccmp(args[[2]], cntxt)
        if(idxret[[3]] > 0) {
            cntxt$stop(gettext("[Error]Cannot handle $ access a vector label"),
                    cntxt)
        }
        args[[2]] <- idxret[[1]]
        cntxt <- idxret[[2]]
        
        #do op shift
        if(vecFlag > 0 && idxret[[3]] == 0) {
            #2nd condition must satisify
            #if the val part args[[1]] is an index [1] with vector object. switch it

            #look for inner index
            innerCall <-args[[1]] #may be just a symbol, or (), or index[]
            while(typeof(innerCall) == "language" && as.character(innerCall[[1]]) == '(') {
                innerCall <- innerCall[[2]] #remove ()
            }
            if(typeof(innerCall) == "language" && as.character(innerCall[[1]]) == '[') {
                # do the replacement
                tmp<-innerCall[[3]] #tmp store index
                innerCall[[1]] <-quote(`$`)
                innerCall[[3]] <-args[[2]]
                args[[1]] <- innerCall
                fun <- quote(`[`)
                args[[2]] <- tmp
            }
        }
    } else if(fun_name == "%*%") { 
        #there are some special rules to handle cross product
        valret <- veccmp(args[[1]], cntxt)
        args1 <- valret[[1]]
        cntxt <- valret[[2]]
        vecFlag1 <- valret[[3]]
        valret <- veccmp(args[[2]], cntxt)
        args2 <- valret[[1]]
        cntxt <- valret[[2]]
        vecFlag2 <- valret[[3]]
        # case 1: both sides are vectorized, Not support     
        if(vecFlag1 > 0 && vecFlag2 > 0) {
            cntxt$stop(gettext("[Error]Cannot trans %*% on two vectorized objects!"),
                    cntxt)
        } else if(vecFlag2 > 0) {
            #if the 2nd op is vectorized, just do left/right switch and transform the matrix
            vecFlag <- vecFlag2
            args[[1]] <- args2
            args[[2]] <- as.call(c(quote(t), args1))
        } else {
            #could be both args are not vectorized, or 1st args are vectorized
            #no other changes
            vecFlag <- vecFlag1 
            args[[1]] <- args1
            args[[2]] <- args2
        } 
    } else if (fun_name == "{") {
        #vector space, { function call only consider the last arg's result a vecFlag's result
        vecFlag <- 0L #by default it is sequential
        for (i in seq_along(args)) {
            ret <- veccmp(args[[i]], cntxt)
            args[[i]] <- ret[[1]] #note the second one is the dim size
            cntxt <- ret[[2]]
            vecFlag <- ret[[3]]
        }   
    } else if (fun_name == "if") {
        #TODO: not fully support inside if's variable assign. Need track variable assign inside
        #control flow basics.
        # algorithm. transform cond. 
        #if isVec(cond) -> transform (trueStmt), false(stmt). should be scalar. otherwise stop
        #if notVec(cond) -> transform trueStmt, falseStmt. if any isVec, wrap the other one and return.
        vecFlag <- 0L
        condRet <- veccmp(args[[1]], cntxt)
        cntxt <- condRet[[2]]
        trueRet <- veccmp(args[[2]], cntxt)
        postTrueCntxt <- trueRet[[2]]
        
        if(length(args) > 2) { #has false block
            falseRet <- veccmp(args[[3]], cntxt)
            postFalseCntxt <- falseRet[[2]]            
        } else {
            falseRet <- list(quote(NULL), cntxt, 0L);
            postFalseCntxt <- cntxt
        }
        if(condRet[[3]]) { #condition is vectorized
            #change to ifelse function call
            fun <-quote(ifelse)
            args[[1]] <- condRet[[1]]
            args[[2]] <- trueRet[[1]]
            if(!is.null(falseRet[[1]])) {
                args[[3]] <- falseRet[[1]] 
            } else {
                args <- as.list(args)
                length(args) <- 3 #force insert null                
            }
            vecFlag <- 1L
        } else {
            #if is istill if
            args[[1]] <- condRet[[1]]
            refvar <- as.symbol(cntxt$dimvars[1])
            if(trueRet[[3]]) { #true block is vec
                vecFlag <- 1L
                args[[2]] <- trueRet[[1]]
                if(length(args) > 2) {
                    if(falseRet[[3]]) {
                        args[[3]] <- falseRet[[1]]
                    } else { #span false
                        args[[3]] <- as.call(list(quote(va_repVecData), falseRet[[1]], refvar))
                    }
                }
            } else {  #true block scalar
                if(length(args) > 2) {
                    args[[3]] <- falseRet[[1]]
                    if(falseRet[[3]]){
                        vecFlag <- 1L
                        args[[2]] <- as.call(list(quote(va_repVecData), trueRet[[1]], refvar))
                    } else {
                        args[[2]] <- trueRet[[1]]
                    }
                } else { #only true block
                    args[[2]] <- trueRet[[1]]
                }
            }
        }
        
        #TODO: merge the localbindings, and merge the local bindings, and insert wrapper
        # function(a) {if(cond) { y <- 1} else { y<- a} } 
        # true block have to insert setdiff(postFalseCntxt$vecvars, postTrueCntxt$vecvars)
        # false block have to insert setdiff(postTrueCntxt$vecvars, postFalseCntxt$vecvars)
        cntxt$vecvars <- union(postTrueCntxt$vecvars,postFalseCntxt$vecvars)
    }
    else if(fun_name == "function") {
        #vec transform for nested function. no need transform formals
        if(!is.null(args[[1]])) {
            ret <- veccmp(args[[1]], cntxt)
            args[[1]] <- ret[[1]]
            cntxt <- ret[[2]]
        }
        #handle body
        ret <- veccmp(args[[2]], cntxt)
        args[[2]] <- ret[[1]]
        cntxt <- ret[[2]]
        vecFlag <- ret[[3]]  #even a function could be changed to vector
    } 
    else if(fun_name == "lapply") { #here only check lapply, could be SparkR's lapply 
        #invector context, lapply or other apply, if the list is vectorized. then it cannot handle
        #if the function is vectorized, then the result is changed to vectorized. but lapply will not be changed.
        dimsret <- integer(length(args))
        for (i in seq_along(args)) {
            ret <- veccmp(args[[i]], cntxt)
            args[[i]] <- ret[[1]] #note the second one is the dim size
            cntxt <- ret[[2]]
            dimsret[i] <- ret[[3]]
        }
        if(dimsret[1] > 0) {
            cntxt$stop(gettext("[Error]Cannot trans lapply a vectorized list object inside vec transform!"),
                    cntxt)
        }
        if(sum(dimsret) > 0 && dimsret[2] == 0) {
            cntxt$stop(gettext("[Error]Cannot trans lapply with function is still scalar but other inputs are vectors!"),
                    cntxt)
        }
        vecFlag <- dimsret[2]
        
        if(vecFlag > 0 && length(args) > 2) { #wrap args[[i]] where i>2 with dimsret[i] == 0
            refvar <- as.symbol(cntxt$dimvars[1])
            for(i in 3:length(args)) {
                if(dimsret[i] == 0) {
                    args[[i]] <- as.call(list(quote(va_repVecData), args[[i]], refvar))                    
                }
            }
        }
    }
    else if(fun_name %in% VectorInputFuns && isBaseVar(fun_name, cntxt)) {
        #use special handling to denseData as input
        # idea: if the function take vector as input (order, max, etc.), then vec2list's src must be SoA
        #       then, just do simplify2array, and apply with dim = 1 to get the result
        
        ret <- veccmp(args[[1]], cntxt)
        args[[1]] <- ret[[1]]
        cntxt <- ret[[2]]
        vecFlag <- ret[[3]]
        if(vecFlag > 0) {  #Wrap it into va_vecApplyWrapper
            args<- list(as.call(c(quote(va_vecApplyWrapper), args[[1]], as.symbol(fun_name))))
            fun <- quote(va_list2vec)
        }    
    }
    else if(fun_name == "for") { #has three args. 
        #right now doesnot support conditions are vectorized
        ret <- veccmp(args[[1]], cntxt)
        if(ret[[3]] > 0) {
            cntxt$stop(gettext("[Error]Cannot trans for loop with vector induction var"),
                    cntxt)
        }
        args[[1]] <- ret[[1]]
        cntxt <- ret[[2]]
        
        ret <- veccmp(args[[2]], cntxt)
        if(ret[[3]] > 0) {
            cntxt$stop(gettext("[Error]Cannot trans for loop with vector loop scope"),
                    cntxt)
        }
        args[[2]] <- ret[[1]]
        cntxt <- ret[[2]]
        
        #the body
        ret <- veccmp(args[[3]], cntxt)
        args[[3]] <- ret[[1]]
        cntxt <- ret[[2]]
        
        vecFlag <- 0L #for loop will not return valuable stuff
    }
    else {
        #in vectorization situation. This will not handle lapply anymore right now
        # the algorithm, just visit the args, if any dim is larger than 0, 
        # wrap all with vec data except the 
        # and vectorize the function.
        dimsret <- integer(length(args))
        for (i in seq_along(args)) {
            ret <- veccmp(args[[i]], cntxt)
            args[[i]] <- ret[[1]] #note the second one is the dim size
            dimsret[i] <- ret[[3]]
            controlCntxt <- list()
            if(fun_name == "while") {
                if(i == 1) {
                    cntxt <- ret[[2]]
                    controlCntxt <- list(cntxt)
                } else{
                    cntxt <- ret[[2]]
                    controlCntxt <- c(controlCntxt, cntxt)
                }
            } else{
                cntxt <- ret[[2]]
            }
        }
        #TODO: merge control context's variable vector attributes
        
        vecFlag <- as.integer(any(dimsret))
        if(vecFlag) { #then every operand whose dims is scalar should be wrapped as vecdata
            fun <- genVecFunNode(fun, cntxt)
            fun_name <- as.character(fun)
            refvar <- as.symbol(cntxt$dimvars[1])
            for (i in seq_along(args)) {
                #This is an optimization. If the args[[i]] is a scalar, and the fun is a math operator, then no need expand
                if(dimsret[i] == 0L &&
                   (  !fun_name %in% ImplicitRepFuns
                    || !is.atomic(args[[i]])
                    || length(args[[i]]) != 1L)) {
                    args[[i]] <- as.call(list(quote(va_repVecData), args[[i]], refvar))
                }
            }
        } 
    }
    call <- as.call(c(fun, as.list(args)))
    list(call, cntxt, vecFlag)
}


############### Client Interface of the compiler ###################

va_compile <- function(e, env = .GlobalEnv, options = NULL) {
    cenv <- makeCenv(env)
    cntxt <- make.toplevelContext(cenv, options)
    cntxt$env <- addCenvVars(cenv, findLocals(e, cntxt))
    
    #now identify the lapply expression
    ret <- applycmp(e, cntxt)
    getRetVal(ret) #Still need insefrt wrapper if needed
}

#Top level interface to va_opt an function, the function body should be transformed
va_cmpfun <- function(f, options = NULL) {
    type <- typeof(f)
    if (type == "closure") {
        body = body(f)
        forms = formals(f)
        cntxt <- make.toplevelContext(makeCenv(environment(f)), options)
        ncntxt <- make.functionContext(cntxt, forms, body)
        ret <- applycmp(body, ncntxt)
        body(f) <- ret[[1]]
        f
    }
    else if (typeof(f) == "builtin" || type == "special")
        f
    else stop("cannot compile a non-function")    
}

