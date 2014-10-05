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
# { if(! exists(".va.listVal", inherits = FALSE) || !identical(.vasrc.listVal, listVal)) {
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
                    if(! exists(".va.listVal", inherits = FALSE) || !identical(.vasrc.listVal, listVal) ) {
                        .va.listVal <- va_list2vec(listVal)
                        .vasrc.listVal <- listVal
                    }
                    .va.listVal
                }
        )
        #now directly modify the quote's AST
        ret[[2]][[2]][[2]][[2]][[2]] <- vecValName
        ret[[2]][[2]][[3]][[2]][[2]] <- vecSrcSym
        ret[[2]][[2]][[3]][[2]][[3]] <- listVal
        ret[[2]][[3]][[2]][[2]] <- vecValSym
        ret[[2]][[3]][[2]][[3]][[1]] <- transfun
        ret[[2]][[3]][[2]][[3]][[2]] <- listVal
        ret[[2]][[3]][[3]][[2]] <-  vecSrcSym
        ret[[2]][[3]][[3]][[3]] <- listVal
        ret[[3]] <- vecValSym
        ret
    } else {
        as.call(list(transfun, listVal))   
    }
}

# input: aList is the list from a symbol or an expression
tryGetVecDataBinding <- function(aList, cntxt) {
    vecval <- NULL
    if(typeof(aList) == 'symbol') {
        listExpr = cntxt$localbindings[[as.character(aList)]]
        if(!is.null(listExpr) && typeof(listExpr) == "language"
                && as.character(listExpr[[1]]) == "va_vec2list") {
            vecval = listExpr[[2]] #should be a symbol with .va. prefix
        }
        
    } else if(typeof(aList) == "language" && as.character(aList[[1]]) == "va_vec2list") {
        vecval = aList[[2]]
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
    cat("[ScalarVisit]Call:", format(call), '\n')
    
    cntxt <- make.callContext(precntxt, call) #current context
    
    fun <- call[[1]]; fun_name <- as.character(fun)
    args <- call[-1]
    
    isDenseData <- FALSE #only lapply will change it
    
    if("lapply" == fun_name 
        && isBaseVar("lapply", cntxt) && length(args) == 2
        && ! as.character(args[[2]]) %in% UnsupportedFuns) {
        #in this case, the lapply's argument, input maybe another lapply expr or symbol

        ## data
        ret <-  applycmp(args[[1]], cntxt)
        l <- getRetVal(ret) #wrap vec2list ondemand
        cntxt <- ret[[2]]
        
        ## function
        f <- args[[2]]
        # no matter which situation, just wrap f with va_vecFun
        vf <- genVecFunNode(f, cntxt)
        
        vecval <- tryGetVecDataBinding(l, cntxt) #try to get the vec rep
        if(is.null(vecval)) {
            vecval <- genVecObjectNode(l, isData = TRUE)            
        }
        call <- as.call(list(vf, vecval))
        isDenseData <- TRUE #the only place change the list rep into vec rep
        
        #TODO: Handle lapply's input is a vec2list case
        # idea: if the function take vector as input (sum, which.min, etc.), then vec2list's src must be SoA
        #       then, just do simplify2array, and apply with dim = 1 to get the result
        
    } else if(fun_name == "Reduce" && isBaseVar("Reduce", cntxt) 
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
        
    } else if(fun_name == "<-" || fun_name == "=") {
        #only handle simple case
        #the police is if the ret[[3]] is TRUE, then we need create an wrapper
        #  { .va.left <- noWrapVector, change the assign to delayedAssign}
        
        lhs <- getAssignedVar(call)
        ret <- applycmp(args[[2]], cntxt)
        rhs <- getRetVal(ret)
        cntxt <- ret[[2]]
        if (ret[[3]]) {  #dense value returned
            #create a temp variable
            tmpVarName <- paste(".va", lhs, sep=".") #.va.lhs name
            tmpAssignStmt <- as.call(c(quote(`<-`), as.symbol(tmpVarName), ret[[1]]))
            delayAssignRHS <- as.call(c(quote(va_vec2list), as.symbol(tmpVarName)))
            delayAssignStmt <- as.call(c(quote(delayedAssign), lhs, delayAssignRHS))
            #finally the wrapper "{"
            call <- as.call(c(quote(`{`), tmpAssignStmt, delayAssignStmt))
            #and the binding is 
            cntxt$localbindings[[lhs]] <- delayAssignRHS #add to the vec2list result as binding
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
    else if (typeof(e) == "pairlist")
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
        mean = "rowMeans"
        )
#These function by default support replication expansion. So no need do manu repExpand        
ImplicitRepFuns <- c("+", "-", "*", "/", "%/%","^", "%%", ">", ">=", "<", "<=", "!=", "==")

#Used for building the simple Basic Block's defs binding calculation
ControlFlowFuns <- c("for", "if", "repeat", "while", "return", "switch")

#Unsupported Funcs that cannot be vectorized
UnsupportedFuns <- c(".Internal", ".External")

# Vector object as input functions, but has no direct higher function mapping available for lapply
VectorInputFuns <- c("min", "max", "which.min", "which.max")

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
        if (name %in% languageFuns) { 
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
    cat("[VecVisit]Symbol:", sym, '\n')
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
    cat("[VecVisit]Constant:", format(val), '\n')
    list(val, cntxt, 0L) #the second is the dim of this constant
}

veccmpFormals <- function(forms, cntxt) {
    cat("[VecVisit]Formals:", names(forms), " <-> ", as.character(forms), '\n')
    list(forms, cntxt, 0L) #not change forms right now. maybe need vectorize the binding values
}


#There are basically two types of compile call actions
#  In scalar space: identify lapply, and transform it into direct function call
#  In the vector space, follow the formal's def-use chain, transform all operations into vector form
veccmpCall <- function(call, precntxt) {
    cat("[VecVisit]Call:", format(call), '\n')
    
    cntxt <- make.callContext(precntxt, call)

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
        cat("  ==>>Call Unsupported fun, gen generic vec wrapper for: ", fun_name, "\n")
        vecvars <- findExprVecVarUse(call, cntxt) #note typically there is no write
        if(length(vecvars) > 0) {
            #build the function, with the whole call
            wrapfun <- wrapExprToFun(vecvars, call)
            
            #build the mapply
            fun<- quote(mapply)
            #now wrap vectors to vec2list
            for(i in seq_along(vecvars)) {
                vecvars[[i]] <- as.call(c(quote(va_vec2list), vecvars[[i]]))
            }
            args<- list(as.call(c(quote(mapply), wrapfun, vecvars)))
            #return the mapply.   # have to insert another level list2vec
            fun <- quote(va_list2vec)
            vecFlag <- 1L
        } 
    } else if (fun_name == '=' || fun_name == '<-') {
        #should handle lhs is a symbol or lhs is a call. e.g. x[1]
        if(!is.symbol(args[[1]])) {
            #should transform the lhs
            ret <- veccmp(args[[1]], cntxt)
            args[[1]] <- ret[[1]]
            cntxt <- ret[[2]] #update local context
        } 
        
        #transform RHS
        ret <- veccmp(args[[2]], cntxt)
        cntxt <- ret[[2]] #update local context
        vecFlag <- ret[[3]] #from the assign
        args[[2]] <- ret[[1]]
        
        #now build the link if required
        lhsSym <- getAssignedVar(call)
        if(is.symbol(args[[1]])) { #lhs is a symbol
            cntxt$env$localbindings[[lhsSym]] <- args[[2]] #note here the link is built with the wrapper
            if(vecFlag) { cntxt <- addCenvVecvars(cntxt, lhsSym) }
        } else { #lhs is a call
            # handle a speical case, the rhs is vecFlag = FALSE, but lhs var is a vector
            # 20141004 ?? Why this situation happens. Need a flag
            if(!vecFlag && isVecVar(lhsSym, cntxt)) {
                refvar <- as.symbol(cntxt$dimvars[1])
                args[[2]] <- as.call(list(quote(va_repVecData), args[[2]], refvar))
                vecFlag <- 1L
            }
            
            if(vecFlag && !isVecVar(lhsSym, cntxt)) {
                #TODO: need handle special cases e.g.
                #   scalar code: y <- integer(5). then y[1] <- a
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
        cntxt$env$localbindings[[lhsSym]] <- ret[[1]]
        args[[2]] <- ret[[1]] #in all cases, the lhs will not be transformed
    } else if (fun_name == "[") { 
        #e.g. x[1] ('['(x,1) )or x[1,2]
        dimsret <- integer(length(args))
        for (i in seq_along(args)) {
            ret <- veccmp(args[[i]], cntxt)
            cntxt <- ret[[2]] #update local context
            args[[i]] <- ret[[1]] #note the second one is the dim size
            dimsret[[i]] <- ret[[3]]
        }
        
        if(dimsret[[1]]) { #val is vectorized
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
    } else if (fun_name == "{") {
        #vector space, { function call only consider the last arg's result a vecFlag's result
        vecFlag <- 0L #by default it is sequential
        for (i in seq_along(args)) {
            ret <- veccmp(args[[i]], cntxt)
            args[[i]] <- ret[[1]] #note the second one is the dim size
            cntxt <- ret[[2]]
            vecFlag <- ret[[3]]
        }
    } else if (fun_name == "if" && isBaseVar("if", cntxt)) {
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
    else {
        #in vectorization situation. This will not handle lapply anymore right now
        # the algorithm, just visit the args, if any dim is larger than 0, 
        # wrap all with vec data except the 
        # and vectorize the function.
        dimsret <- integer(length(args))
        for (i in seq_along(args)) {
            ret <- veccmp(args[[i]], cntxt)
            args[[i]] <- ret[[1]] #note the second one is the dim size
            dimsret[[i]] <- ret[[3]]
            controlCntxt <- list()
            if(fun_name == "for") {
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
        #TODO: merge control context's variable vector attributes
        
        vecFlag <- as.integer(any(dimsret))
        if(vecFlag) { #then every operand whose dims is scalar should be wrapped as vecdata
            fun <- genVecFunNode(fun, cntxt)
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

