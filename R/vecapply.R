# Author: Haichuan Wang (hwang154@illinois.edu)
###############################################################################


## Name convention
## va_*: all external exposure functions, including runtime functions and compiler user interface
## small character started camelCase names: compiler internal function names
##   veccmp*: Used for vectorization compiler
## Large character started CamelCase names: data structures used by compiler functions

############## Runtime functions ##############
# All VecApply runtime functions (exported) has a prefix "va_"

# Return the length of the vector
# v must be a List(SoA)       --> return the length of the list
#        or a multi-dim array --> return the first dim size
va_vecLen <- function(v) { #v must be vec, or SoA
    if(is.list(v)){
        vecLen(v[[1]])
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
    #TODO: do real transform, currently, just return the original function
    type <- typeof(clos)

    if (type == "closure") {
        body = body(clos)
        forms = formals(clos)
        cntxt <- make.toplevelContext(makeCenv(environment(clos)), options)
        ncntxt <- make.functionContext(cntxt, forms, body)
        #add all formals as vecvars
        addCenvVecvars(ncntxt$env, names(forms))
        ncntxt$dimvars <- c(ncntxt$dimvars, names(forms)[1]) #note the clos should have at least one form
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


############## Compiler functions ##############
# 
# Many functions are directly copied from or referenced to R byte-code compiler package
# The current referenced code is R-3.1.1

###### Code Copied from Byte-code Compiler #######
##
## Compiler options
##

compilerOptions <- new.env(hash = TRUE, parent = emptyenv())
compilerOptions$optimize <- 3
compilerOptions$suppressAll <- FALSE
compilerOptions$suppressUndefined <-
        c(".Generic", ".Method", ".Random.seed", ".self")

getCompilerOption <- function(name, options = NULL) {
    if (name %in% names(options))
        options[[name]]
    else
        get(name, compilerOptions)
}

##
## General Utilities
##

pasteExpr <- function(e, prefix = "\n    ") {
    de <- deparse(e)
    if (length(de) == 1) sQuote(de)
    else paste(prefix, deparse(e), collapse="")
}

dots.or.missing <- function(args) {
    for (i in 1:length(args)) {
        a <-args[[i]]
        if (missing(a)) return(TRUE) #**** better test?
        if (typeof(a) == "symbol" && a == "...") return(TRUE)
    }
    return(FALSE)
}

any.dots <- function(args) {
    for (i in 1:length(args)) {
        a <-args[[i]]
        if (! missing(a) && typeof(a) == "symbol" && a == "...")
            return(TRUE)
    }
    return(FALSE)
}

is.ddsym <- function(name) {
    (is.symbol(name) || is.character(name)) &&
            length(grep("^\\.\\.[0-9]+$", as.character(name))) != 0
}

## We need our own version of base::asS4 that differs only from the one
## in base by using methods:::as instead of methods::as.  This is needed
## to JIT compile methods as the lazy loading mechanism used there does
## it's thing during the first namespace load at a point where exports
## are not yet set up.
asS4 <- function(object, flag = TRUE, complete = TRUE) {
    flag <- methods:::as(flag, "logical")
    if(length(flag) != 1L || is.na(flag))
        stop("expected a single logical value for the S4 state flag")
    .Call("R_setS4Object", object, flag, complete, PACKAGE = "base")
}

missingArgs <- function(args) {
    val <- logical(length(args))
    for (i in seq_along(args)) {
        a <- args[[i]]
        if (missing(a))
            val[i] <- TRUE
        else
            val[i] <- FALSE
    }
    val
}


##
## Environment utilities
##

frameTypes <- function(env) {
    top <- topenv(env)
    empty <- emptyenv()
    nl <- 0
    while (! identical(env, top)) {
        env <- parent.env(env)
        nl <- nl + 1
        if (identical(env, empty))
            stop("not a proper evaluation environment")
    }
    nn <- 0
    if (isNamespace(env)) {
        while (! identical(env, .GlobalEnv)) {
            env <- parent.env(env)
            nn <- nn + 1
            if (identical(env, empty))
                stop("not a proper evaluation environment")
        }
    }
    ng <- 0
    while (! identical(env, empty)) {
        env <- parent.env(env)
        ng <- ng + 1
    }
    rep(c("local", "namespace", "global"), c(nl, nn, ng))
}

## Given a symbol name and a namespace environment (or a namespace
## imports environment) find the namespace in which the symbol's value
## was originally defined. Returns NULL if the symbol is not found via
## the namespace.
findHomeNS <- function(sym, ns) {
    if (! isNamespace(ns)) {
        ## As a convenience this allows for 'ns' to be the imports fame
        ## of a namespace. It appears that these now have a 'name'
        ## attribute of the form 'imports:foo' if 'foo' is the
        ## namespace.
        name <- attr(ns, "name")
        if (is.null(name))
            stop("'ns' must be a namespace or a namespace imports environment")
        ns <- getNamespace(sub("imports:", "", attr(ns, "name")))
    }
    if (exists(sym, ns, inherits = FALSE))
        ns
    else if (exists(".__NAMESPACE__.", ns, inherits = FALSE)) {
        imports <- get(".__NAMESPACE__.", ns)$imports
        for (i in rev(seq_along(imports))) {
            iname <- names(imports)[i]
            ins <- getNamespace(iname)
            if (identical(imports[[i]], TRUE)) {
                if (identical(ins, .BaseNamespaceEnv))
                    exports <- .BaseNamespaceEnv
                else
                    exports <- get(".__NAMESPACE__.", ins)$exports
                if (exists(sym, exports, inherits = FALSE))
                    return(findHomeNS(sym, ins))
            }
            else {
                exports <- imports[[i]]
                pos <- match(sym, names(exports), 0)
                if (pos) {
                    ## If renaming has been used things get too
                    ## confusing so return NULL. (It is not clear if
                    ## renaming this is still supported by the
                    ## namespace code.)
                    if (sym == exports[pos])
                        return(findHomeNS(sym, ins))
                    else
                        return(NULL)
                }
            }
        }
        NULL
    }
    else NULL
}

packFrameName <- function(frame) {
    fname <- attr(frame, "name")
    if (is.character(fname))
        sub("package:", "", fname)
    else if (identical(frame , baseenv()))
        "base"
    else ""
}

nsName <- function(ns) {
    if (identical(ns, .BaseNamespaceEnv))
        "base"
    else {
        name <- ns$.__NAMESPACE__.$spec["name"]
        if (is.character(name))
            as.character(name) ## strip off names
        else ""
    }
}


##
## Finding possible local variables
##

getAssignedVar <- function(e) {
    v <- e[[2]]
    if (missing(v))
        stop(gettextf("bad assignment: %s", pasteExpr(e)),
                domain = NA)
    else if (typeof(v) %in% c("symbol", "character"))
        as.character(v)
    else {
        while (typeof(v) == "language") {
            if (length(v) < 2)
                stop(gettextf("bad assignment: %s", pasteExpr(e)),
                        domain = NA)
            v <- v[[2]]
            if (missing(v))
                stop(gettextf("bad assignment: %s", pasteExpr(e)),
                        domain = NA)
        }
        if (typeof(v) != "symbol")
            stop(gettextf("bad assignment: %s", pasteExpr(e)),
                    domain = NA)
        as.character(v)
    }
}

findLocals1 <- function(e, shadowed = characbter(0)) {
    if (typeof(e) == "language") {
        if (typeof(e[[1]]) %in% c("symbol", "character")) {
            v <- as.character(e[[1]])
            switch(v,
                    "=" =,
                    "<-" = unique(c(getAssignedVar(e),
                                    findLocalsList1(e[-1], shadowed))),
                    "for" = unique(c(as.character(e[2]),
                                    findLocalsList1(e[-2], shadowed))),
                    "delayedAssign" =,
                    "assign" = if (length(e) == 3 &&
                                    is.character(e[[2]]) &&
                                    length(e[[2]]) == 1)
                                c(e[[2]], findLocals1(e[[3]], shadowed))
                            else findLocalsList1(e[1], shadowed),
                    "function" = character(0),
                    "~" = character(0),
                    "local" = if (! v %in% shadowed && length(e) == 2)
                                character(0)
                            else findLocalsList1(e[-1], shadowed),
                    "expression" =,
                    "quote" = if (! v %in% shadowed)
                                character(0)
                            else findLocalsList1(e[-1], shadowed),
                    findLocalsList1(e[-1], shadowed))
        }
        else findLocalsList1(e, shadowed)
    }
    else character(0)
}

findLocalsList1 <- function(elist, shadowed)
    unique(unlist(lapply(elist, findLocals1, shadowed)))

findLocals <- function(e, cntxt)
    findLocalsList(list(e), cntxt)

findLocalsList <- function(elist, cntxt) {
    initialShadowedFuns <- c("expression", "local", "quote")
    shadowed <- Filter(function(n) ! isBaseVar(n, cntxt), initialShadowedFuns)
    specialSyntaxFuns <- c("~", "<-", "=", "for", "function")
    sf <- initialShadowedFuns
    nsf <- length(sf)
    repeat {
        vals <- findLocalsList1(elist, sf)
        redefined <- sf %in% vals
        last.nsf <- nsf
        sf <- unique(c(shadowed, sf[redefined]))
        nsf <- length(sf)
        ## **** need to fix the termination condition used in codetools!!!
        if (last.nsf == nsf) {
            rdsf <- vals %in% specialSyntaxFuns
            if (any(rdsf)) {
                msg <- ngettext(sum(rdsf),
                        "local assignment to syntactic function: ",
                        "local assignments to syntactic functions: ")
                warning(msg, paste(vals[rdsf], collapse = ", "),
                        domain = NA)
            }
            return(vals)
        }
    }
}

languageFuns <- c("^", "~", "<", "<<-", "<=", "<-", "=", "==", ">", ">=",
        "|", "||", "-", ":", "!", "!=", "/", "(", "[", "[<-", "[[",
        "[[<-", "{", "@", "$", "$<-", "*", "&", "&&", "%/%", "%*%",
        "%%", "+",
        "::", ":::", "@<-",
        "break", "for", "function", "if", "next", "repeat", "while",
        "local", "return", "switch")

getInlineInfo <- function(name, cntxt) {
    optimize <- cntxt$optimize
    if (optimize > 0) {
        info <- findCenvVar(name, cntxt$env)
        if (is.null(info))
            NULL
        else {
            ftype <- info$ftype
            frame <- info$frame
            if (ftype == "namespace") {
                if (! isNamespace(frame)) {
                    ## should be the import frame of the current topenv
                    top <- topenv(cntxt$env$env)
                    if (! isNamespace(top) ||
                            ! identical(frame, parent.env(top)))
                        cntxt$stop(gettext("bad namespace import frame"))
                    frame <- top
                }
                info$package <- nsName(findHomeNS(name, frame))
                info
            }
            else if (ftype == "global" &&
                    (optimize >= 3 ||
                        (optimize >= 2 && name %in% languageFuns))) {
                info$package <- packFrameName(frame)
                info
            }
            else NULL
        }
    }
    else NULL
}

##
## Compilation environment implementation
##

## Create a new compiler environment
## **** need to explain the structure
makeCenv <- function(env) {
    structure(list(extra = list(character(0)),
                    vecvarenv = new.env(),
                    localdefs = list(), #local Basic Block's conservertive bindings
                    env = env,
                    ftypes = frameTypes(env)),
            class = "compiler_environment")
}

## Add vars to the top compiler environment frame
addCenvVars <- function(cenv, vars) {
    cenv$extra[[1]] <- union(cenv$extra[[1]], vars)
    cenv
}

## Add vecvars
addCenvVecvars <- function(cenv, vecvars) {
    for(vecvar in vecvars) {
        assign(vecvar, TRUE, envir=cenv$vecvarenv) #mark as true
    }
}

## Add a new frame to a compiler environment
addCenvFrame <- function(cenv, vars) {
    cenv$extra <- c(list(character(0)), cenv$extra)
    cenv$vecvarenv <- new.env(parent = cenv$vecvarenv) #note use environment object to record
    cenv$localdefs <- list() #not use any parent's information
    cenv$env <- new.env(parent = cenv$env)
    cenv$ftypes <- c("local", cenv$ftypes)
    if (missing(vars))
        cenv
    else
        addCenvVars(cenv, vars)
}

## Find binding information for a variable (character or name).
## If a binding is found, return a list containing components
##   ftype -- one of "local", "namespace", "global"
##   value -- current value if available
##   frame -- frame containing the binding (not useful for "local" variables)
##   index -- index of the frame (1 for top, 2, for next one, etc.)
## Return NULL if no binding is found.
## **** drop the index, maybe value, to reduce cost? (query as needed?)
findCenvVar <- function(var, cenv) {
    if (typeof(var) == "symbol")
        var <- as.character(var)
    extra <- cenv$extra
    env <- cenv$env
    frame <- NULL
    for (i in seq_along(cenv$extra)) {
        if (var %in% extra[[i]] || exists(var, env, inherits = FALSE)) {
            frame <- env
            break
        }
        else
            env <- parent.env(env)
    }
    if (is.null(frame)) {
        empty <- emptyenv()
        while (! identical(env, empty)) {
            i <- i + 1
            if (exists(var, env, inherits = FALSE)) {
                frame <- env
                break
            }
            else
                env <- parent.env(env)
        }
    }
    if (! is.null(frame)) {
        if (exists(var, frame, inherits = FALSE) && var != "...") {
            value <- new.env(parent = emptyenv())
            delayedAssign("value", get(var, frame, inherits = FALSE),
                    assign.env = value)
        }
        else
            value <- NULL
        list(frame = frame, ftype = cenv$ftypes[i], value = value, index = i)
    }
    else
        NULL
}

## Only find a var inside a frame, and report whether it is vectorized.
## If yes, also return the expand dimension for future use
findVecvar <- function(var, cntxt) {
    if (typeof(var) == "symbol")
        var <- as.character(var)
    cenv <- cntxt$env
    info <- findCenvVar(var, cenv)
    # cat("[findVecvar->findCenvVar]", as.character(info), '\n'); 
    if (! is.null(info)) {
        isVec = exists(var, envir= cenv$vecvarenv, inherits=FALSE)
        list(isVec = isVec) #, dim = cenv$dim[i]
    } else {
        NULL #just return NULL
    }
}

isVecVar <- function(var, cntxt) {
    info <- findVecvar(var, cntxt)
    if (!is.null(info) && info$isVec) {
        TRUE
    } else {
        FALSE
    }
}


isBaseVar <- function(var, cntxt) {
    info <- getInlineInfo(var, cntxt)
    (! is.null(info) &&
                (identical(info$frame, .BaseNamespaceEnv) ||
                    identical(info$frame, baseenv())))
}

## augment compiler environment with function args and locals
funEnv <- function(forms, body, cntxt) {
    cntxt$env <- addCenvFrame(cntxt$env, names(forms))
    locals <- findLocalsList(c(forms, body), cntxt)
    addCenvVars(cntxt$env, locals)
}

## test whether a local version of a variable might exist
findLocVar <- function(var, cntxt) {
    cenv <- cntxt$env
    info <- findCenvVar(var, cenv)
    ! is.null(info) && info$ftype == "local"
}

## **** should this check for local functions as well?
findFunDef <- function(fun, cntxt) {
    cenv <- cntxt$env
    info <- findCenvVar(fun, cenv)
    if (! is.null(info$value) && is.function(info$value$value))
        info$value$value
    else
        NULL
}

findVar <- function(var, cntxt) {
    cenv <- cntxt$env
    info <- findCenvVar(var, cenv)
    ! is.null(info)
}


##
## Compiler contexts
##

make.toplevelContext <- function(cenv, options = NULL)
    structure(list(tailcall = TRUE,
                    needRETURNJMP = FALSE,
                    env = cenv,
                    optimize = getCompilerOption("optimize", options),
                    suppressAll = getCompilerOption("suppressAll", options),
                    suppressUndefined = getCompilerOption("suppressUndefined",
                            options),
                    call = NULL,
                    dimvars = character(0), #record the primary variables used to build the vec
                    stop = function(msg, cntxt)
                        stop(simpleError(msg, cntxt$call)),
                    warn = function(x, cntxt) cat(paste("Note:", x, "\n"))),
            class = "compiler_context")

make.callContext <- function(cntxt, call) {
    cntxt$call <- call
    cntxt
}

make.promiseContext <- function(cntxt) {
    cntxt$tailcall <- TRUE
    cntxt$needRETURNJMP <- TRUE
    if (! is.null(cntxt$loop))
        cntxt$loop$gotoOK <- FALSE
    cntxt
}

make.functionContext <- function(cntxt, forms, body) {
    nenv <- funEnv(forms, body, cntxt)
    ncntxt <- make.toplevelContext(nenv)
    ncntxt$optimize <- cntxt$optimize
    ncntxt$suppressAll <- cntxt$suppressAll
    ncntxt$suppressUndefined <- cntxt$suppressUndefined
    ncntxt
}

make.nonTailCallContext <- function(cntxt) {
    cntxt$tailcall <- FALSE
    cntxt
}

make.argContext <- function(cntxt) {
    cntxt$tailcall <- FALSE
    if (! is.null(cntxt$loop))
        cntxt$loop$gotoOK <- FALSE
    cntxt
}

make.noValueContext <- function(cntxt) {
    cntxt$tailcall <- FALSE
    cntxt
}

make.loopContext <- function(cntxt, loop.label, end.label) {
    ncntxt <- make.noValueContext(cntxt)
    ncntxt$loop <- list(loop = loop.label, end = end.label, gotoOK = TRUE)
    ncntxt
}


##
## Compiler warnings
##

suppressAll <- function(cntxt)
    identical(cntxt$suppressAll, TRUE)

suppressUndef <- function(name, cntxt) {
    if (identical(cntxt$suppressAll, TRUE))
        TRUE
    else {
        suppress <- cntxt$suppressUndefined
        if (is.null(suppress))
            FALSE
        else if (identical(suppress, TRUE))
            TRUE
        else if (is.character(suppress) && as.character(name) %in% suppress)
            TRUE
        else FALSE
    }
}

notifyLocalFun <- function(fun, cntxt) {
    if (! suppressAll(cntxt))
        NULL
}

notifyUndefFun <- function(fun, cntxt) {
    if (! suppressUndef(fun, cntxt)) {
        msg <- gettextf("no visible global function definition for '%s'",
                as.character(fun))
        cntxt$warn(msg, cntxt)
    }
}

notifyUndefVar <- function(var, cntxt) {
    if (! suppressUndef(var, cntxt)) {
        msg <- gettextf("no visible binding for global variable '%s'",
                as.character(var))
        cntxt$warn(msg, cntxt)
    }
}

notifyNoSuperAssignVar <- function(symbol, cntxt) {
    if (! suppressAll(cntxt)) {
        msg <- gettextf("no visible binding for '<<-' assignment to '%s'",
                as.character(symbol))
        cntxt$warn(msg, cntxt)
    }
}

notifyWrongArgCount <- function(fun, cntxt) {
    if (! suppressAll(cntxt))
        cntxt$warn(gettextf("wrong number of arguments to '%s'",
                        as.character(fun)),
                cntxt)
}

notifyWrongDotsUse <- function(var, cntxt) {
    if (! suppressAll(cntxt))
        cntxt$warn(paste(var, "may be used in an incorrect context"), cntxt)
}

notifyWrongBreakNext <- function(fun, cntxt) {
    if (! suppressAll(cntxt)) {
        msg <- paste(fun, "may be used in wrong context: no loop is visible")
        cntxt$warn(msg, cntxt)
    }
}

notifyBadCall <- function(w, cntxt) {
    if (! suppressAll(cntxt))
        cntxt$warn(w, cntxt)
}

notifyBadAssignFun <- function(fun, cntxt) {
    if (! suppressAll(cntxt))
        cntxt$warn(gettext("invalid function in complex assignment"))
}

notifyMultipleSwitchDefaults <- function(ndflt, cntxt)
    if (! suppressAll(cntxt))
        cntxt$warn(gettext("more than one default provided in switch() call"),
                cntxt)


###### Vec Compiler Specific Code #######


## Utility functions for AST manupulation

#The only way to get a missing argument
# http://stackoverflow.com/questions/20904827/the-representation-of-an-empty-argument-in-a-call
EmptySymbol <- function() (quote(f(,)))[[2]]

#input ret[[4]], 
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

getVecVal <- function(listVal, isData = TRUE) {
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

## Recursive compile structure - The entrance of the compilation
## e: expr
## cntxt: compile context
## This is the top level visiting function. The value returned is a list of 4
## ret[[1]] the transformed object
## ret[[2]] The object is belong to the vector world or scalar world
##    0: scalar world; 1: vector with 1 dim; 2: vector with two dim space
## ret[[3]] boolean: the object is transformed into vector representation
##    we may need to add va_vec2list to the result if the usage of ret[[1]] is original space
## ret[[4]] the local variable binding relationship. It's a list
##    Need to track the binding in a single basic block. 
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
        sum = "rowSums"
        
        )
#These function by default support replication expansion. So no need do manu repExpand        
ImplicitRepFuns <- c("+", "-", "*", "/", "%/%","^", "%%", ">", ">=", "<", "<=", "!=", "==")

#Used for building the simple Basic Block's defs binding calculation
ControlFlowFuns <- c("for", "if", "repeat", "while", "return", "switch")



# Vectorize the function (symbol or real object)
# only expand one dimension right now
# 
# if fun is a symbol
#  if the symbol is belong to language symbol, return directly
#  otherwise return a wrapper va_vecFun(fun) which will trigger a runtime call
# if fun is a closure, just call the va_vecFun(closure)
veccmpFun <- function(fun, cntxt) {
    
    if (typeof(fun) == 'symbol') {
        #either return a wrapper, or return the real object
        name <- as.character(fun)
        if (name %in% languageFuns) { 
            fun
        } else if(!is.null(BuiltinVecFuns[[name]])) {
            as.symbol(BuiltinVecFuns[[name]])
        }
        else {
            getVecVal(fun, FALSE) #get the wrapper va_vecClosure() with runtime binding
        }
    } else if (typeof(fun) == 'closure') {
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
    cat("[Visiting]Symbol:", sym, '\n')
    if(identical(sym, EmptySymbol())) { return(list(sym, 0L, FALSE, cntxt$env$localdefs)) }
    #try to search this symbol in the context's environment.
    #If it is a vector var, return dim = 1, otherwise return 0
    info <- findVecvar(sym, cntxt)
    if (!is.null(info) && info$isVec) {
        list(sym, 1L, FALSE, cntxt$env$localdefs) # a vectorized one
    } else {
        list(sym, 0L, FALSE, cntxt$env$localdefs)  # a scala one
    }
    
}

veccmpConst <- function(val, cntxt) {
    #constant definetely 
    cat("[Visiting]Constant:", format(val), '\n')
    list(val, 0L, FALSE, cntxt$env$localdefs) #the second is the dim of this constant
}

veccmpFormals <- function(forms, cntxt) {
    cat("[Visiting]Formals:", names(forms), " <-> ", as.character(forms), '\n')
    list(forms, 0L, FALSE, cntxt$env$localdefs) #not change forms right now. maybe need vectorize the binding values
}


#There are basically two types of compile call actions
#  In scalar space: identify lapply, and transform it into direct function call
#  In the vector space, follow the formal's def-use chain, transform all operations into vector form
veccmpCall <- function(call, cntxt) {
    cat("[Visiting]Call:", format(call))
    
    cntxt <- make.callContext(cntxt, call)
    localdefs <- NULL # cntxt$env$localdefs #get the accumulated pre local defs
    isVecSpace <- length(cntxt$dimvars) > 0

    fun <- call[[1]]; fun_name <- as.character(fun)
    args <- call[-1]
    
    isControlFun <- fun_name %in% ControlFlowFuns
    isVecData <- FALSE #only lapply will change it
    
    if (!isVecSpace) {
        cat(" ==> Scalar Space Transform!\n")
        #sequential transformation
        if("lapply" == fun_name && isBaseVar("lapply", cntxt) && length(args) == 2) {
            #in this case, the lapply's argument, input maybe another lapply expr or symbol
            #stop visiting at this point
            #now start rewrite, 
            ret <-  veccmp(args[[1]], cntxt)
            l <- getRetVal(ret)
            localdefs <- ret[[4]] #update local context
            f <- args[[2]]
            # no matter which situation, just wrap f with va_vecFun
            vf <- veccmpFun(f, cntxt)
            v <- getVecVal(l, TRUE)
            call <- as.call(list(vf, v))
            isVecData <- TRUE #the only place change the list rep into vec rep
        } else if(fun_name == "Reduce" && isBaseVar("Reduce", cntxt) 
                && args[[1]] == '+' #only support Reduce add
                && (length(args) == 2 || length(args) == 3 && args[[3]] == 0)) {
           #no need transform args[[1]]
           #transform args[[2]]
           ret <- veccmp(args[[2]], cntxt) 
           l <- getRetVal(ret) #may be wrapped 
           localdefs <- ret[[4]] #update local context
           vecval <- NULL
           #case 1. l is a symbol and bound to an expression and the expression is DelayedAssign with list
           if(typeof(l) == 'symbol') {
               listExpr = localdefs[[as.character(l)]]
               if(!is.null(listExpr) && typeof(listExpr) == "language"
                  && as.character(listExpr[[1]]) == "va_vec2list") {
                  vecval = listExpr[[2]] #should be a symbol with .va. prefix
               }
               
           }
           #case 2. l is just a vec2list, 
           if(typeof(l) == "language" && as.character(l[[1]]) == "va_vec2list") {
               vecval = l[[2]]
           }
           
           if(is.null(vecval)) {
               call <- as.call(c(fun, '+', l))
           } else {
               call <- as.call(c(quote(va_reduceSum), vecval))
               #replace with va_reduceSum(vecval)
           }
            
        } else if(fun_name == "<-" || fun_name == "=") {
            #only handle simple case
            #the police is if the ret[[3]] is TRUE, then we need create an wrapper
            #  { .va.left <- noWrapVector, change the assign to delayedAssign}
            
            lhs <- getAssignedVar(call)
            ret <- veccmp(args[[2]], cntxt)
            rhs <- getRetVal(ret)
            localdefs <- ret[[4]]
            if (ret[[3]]) { 
                #create a temp variable
                tmpVarName <- paste(".va", lhs, sep=".") #.va.lhs name
                tmpAssignStmt <- as.call(c(quote(`<-`), as.symbol(tmpVarName), ret[[1]]))
                delayAssignRHS <- as.call(c(quote(va_vec2list), as.symbol(tmpVarName)))
                delayAssignStmt <- as.call(c(quote(delayedAssign), lhs, delayAssignRHS))
                #finally the wrapper "{"
                call <- as.call(c(quote(`{`), tmpAssignStmt, delayAssignStmt))
                #and the binding is 
                localdefs[[lhs]] <- delayAssignRHS
            } else {
                localdefs[[lhs]] <- rhs #add one binding
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
            ret <- veccmp(args[[2]], cntxt) #the expr
            rhs <-getRetVal(ret)
            localdefs <- ref[[4]]
            localdefs[[lhs]] = rhs
            call <- as.call(c(fun, args[[1]], rhs))
        }
        else { #other general function, not control
            #must visit each args, but not the call
            for (i in seq_along(args)) {
                ret <- veccmp(args[[i]], cntxt)
                args[[i]] = getRetVal(ret) #note the second one is the dim size
                if(!isControlFun){
                    localdefs <- ret[[4]]
                    cntxt$env$localdefs = localdefs #should pass localdefs to the next call
                }
            }
            call <- as.call(c(fun, as.list(args)))
            #clean the localdefs because of the contro flow
            #TODO: a better way is to do localdefs merge. if there are different binding, remove that binding
            if(isControlFun) { localdefs <- list()} 
        }
        list(call, 0L, isVecData, localdefs)
    } else {
        cat(" ==> Vector Space Transform!\n")
        
        #will distinguish it is an assign.
        # If yes, transfor RHS, 
        #    If RHS is vec, add the LHS into vector list
        #
        # If no. normal transform
        if (fun_name == '=' || fun_name == '<-') {
            #should handle lhs is a symbol or lhs is a call. e.g. x[1]
            if(!is.symbol(args[[1]])) {
                #should transform the lhs
                ret <- veccmp(args[[1]], cntxt)
                localdefs <- ret[[4]] #update local context
                cntxt$env$localdefs <- localdefs
                args[[1]] <- ret[[1]]
            } 
            
            #transform RHS
            ret <- veccmp(args[[2]], cntxt)
            localdefs <- ret[[4]] #update local context
            cntxt$env$localdefs <- localdefs
            
            vecFlag <- ret[[2]] #from the assign
            args[[2]] <- getRetVal(ret) 
            #now build the link if required
            lhs <- getAssignedVar(call)
            if(is.symbol(args[[1]])) { #lhs is a call

                localdefs[[lhs]] = args[[2]] #note here the link is built with the wrapper
                cntxt$env$localdefs <- localdefs
                if(vecFlag) { addCenvVecvars(cntxt$env, lhs) }
            } else {
                # handle a speical case, the rhs is vecFlag = FALSE, but lhs var is a vector
                if(!vecFlag && isVecVar(lhs, cntxt)) {
                    refvar <- as.symbol(cntxt$dimvars[1])
                    args[[2]] <- as.call(list(quote(va_repVecData), args[[2]], refvar))
                    vecFlag <- 1L
                }
            }

        } else if (fun_name == "assign" && isBaseVar("assign", cntxt)
                || fun_name == "delayedAssign" && isBaseVar("delayedAssign", cntxt)) {
            if(length(args) != 2 || !is.character(args[[1]]) || length(args[[1]]) != 1){
                cntxt$stop(gettext("cannot vec an assign or delayedAssign function"),
                    cntxt)
            }
            lhs <- args[[1]]
            ret <- veccmp(args[[2]], cntxt)
            localdefs <- ret[[4]] #update local context
            cntxt$env$localdefs <- localdefs
            if(ret[[2]]) { #now the LHS should be added into the vecVars
                vecFlag <- 1L
                addCenvVecvars(cntxt$env, lhs)
            } else {
                vecFlag <- 0L
            }
            localdefs[[lhs]] = getRetVal(ret)
            cntxt$env$localdefs <- localdefs
            args[[2]] <- getRetVal(ret) #in all cases, the lhs will not be transformed
        } else if (fun_name == "[") { 
            #e.g. x[1] ('['(x,1) )or x[1,2]
            dimsret <- integer(length(args))
            for (i in seq_along(args)) {
                ret <- veccmp(args[[i]], cntxt)
                localdefs <- ret[[4]] #update local context
                cntxt$env$localdefs <- localdefs
                args[[i]] <- getRetVal(ret) #note the second one is the dim size
                dimsret[[i]] <- ret[[2]]
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
                args[[i]] <- getRetVal(ret) #note the second one is the dim size
                vecFlag <- ret[[2]]
                localdefs <- ret[[4]]
                cntxt$env$localdefs = localdefs
            }
        } else if (fun_name == "if" && isBaseVar("if", cntxt)) {
            #control flow basics.
            # algorithm. transform cond. 
            #if isVec(cond) -> transform (trueStmt), false(stmt). should be scalar. otherwise stop
            #if notVec(cond) -> transform trueStmt, falseStmt. if any isVec, wrap the other one and return.
            vecFlag <- 0L
            condRet <- veccmp(args[[1]], cntxt)
            localdefs <- condRet[[4]]
            cntxt$env$localdefs = localdefs
            trueRet <- veccmp(args[[2]], cntxt)
            localdefs <- trueRet[[4]]
            cntxt$env$localdefs = localdefs
            falseRet <- veccmp(args[[3]], cntxt)
            localdefs <- falseRet[[4]]
            cntxt$env$localdefs = localdefs
            
            if(condRet[[2]]) { #condition is vectorized
                #change to ifelse function call
                fun <-quote(ifelse)
                args[[1]] <- condRet[[1]]
                args[[2]] <- trueRet[[1]]
                args[[3]] <- falseRet[[1]]
                vecFlag <- 1L
            } else {
                #if is istill if
                args[[1]] <- condRet[[1]]
                refvar <- as.symbol(cntxt$dimvars[1])
                if(trueRet[[2]]) {
                    vecFlag <- 1L
                    args[[2]] <- trueRet[[1]]
                    if(falseRet[[2]]) {
                        args[[3]] <- falseRet[[1]]
                    } else { #span false
                        args[[3]] <- as.call(list(quote(va_repVecData), falseRet[[1]], refvar))
                    }
                } else {
                    args[[3]] <- falseRet[[1]]
                    if(falseRet[[2]]){
                        vecFlag <- 1L
                        args[[2]] <- as.call(list(quote(va_repVecData), trueRet[[1]], refvar))
                    } else {
                        args[[2]] <- trueRet[[1]]
                    }
                }
            }
        }
        else {
            #in vectorization situation. This will not handle lapply anymore right now
            # the algorithm, just visit the args, if any dim is larger than 0, 
            # wrap all with vec data except the 
            # and vectorize the function.
            dimsret <- integer(length(args))
            for (i in seq_along(args)) {
                ret <- veccmp(args[[i]], cntxt)
                args[[i]] <- getRetVal(ret) #note the second one is the dim size
                dimsret[[i]] <- ret[[2]]
                if(!isControlFun){
                    localdefs <- ret[[4]]
                    cntxt$env$localdefs = localdefs
                }
            }
            if(isControlFun) { localdefs <- list()}
            
            vecFlag <- as.integer(any(dimsret))
            if(vecFlag) { #then every operand whose dims is scalar should be wrapped as vecdata
                fun <- veccmpFun(fun, cntxt)
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
        list(call, vecFlag, isVecData, localdefs)
    }
}


############### Client Interface of the compiler ###################

va_compile <- function(e, env = .GlobalEnv, options = NULL) {
    cenv <- makeCenv(env)
    cntxt <- make.toplevelContext(cenv, options)
    cntxt$env <- addCenvVars(cenv, findLocals(e, cntxt))
    
    #now identify the lapply expression
    ret <- veccmp(e, cntxt)
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
        
        ret <- veccmp(body, ncntxt)
        body(f) <- ret[[1]]
        f
    }
    else if (typeof(f) == "builtin" || type == "special")
        f
    else stop("cannot compile a non-function")    
}

