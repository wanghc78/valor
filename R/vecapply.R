# Author: Haichuan Wang (hwang154@illinois.edu)
###############################################################################


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
    if(is.null(dim(tmp))) {
        tmp
    } else { #matrix
        t(tmp)
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
va_vecFun <- function(fDef, dim = 1) {
    #TODO: do real transform, currently, just return the original function
    body <- body(fDef)
    args <- names(formals(fDef))
    
    #should reconstruct the content
    body(fDef) <- body #quote(x+2)
    fDef
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

findLocals1 <- function(e, shadowed = character(0)) {
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
                    env = env,
                    ftypes = frameTypes(env)),
            class = "compiler_environment")
}

## Add vars to the top compiler environment frame
addCenvVars <- function(cenv, vars) {
    cenv$extra[[1]] <- union(cenv$extra[[1]], vars)
    cenv
}

## Add a new frame to a compiler environment
addCenvFrame <- function(cenv, vars) {
    cenv$extra <- c(list(character(0)), cenv$extra)
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

## Recursive compile structure - The entrance of the compilation
## e: expr
## cntxt: compile context
veccmp <- function(e, cntxt) {
    if (typeof(e) == "language")
        veccmpCall(e, cntxt)
    else if (typeof(e) == "symbol")
        veccmpSym(e, cntxt)
    else if (typeof(e) == "bytecode")
        cntxt$stop(gettext("cannot vec compile byte code literals in code"),
                cntxt)
    else if (typeof(e) == "promise")
        cntxt$stop(gettext("cannot vec compile promise literals in code"),
                cntxt)
    else
        veccmpConst(e, cntxt)
}

veccmpCall <- function(call, cntxt) {
    cntxt <- make.callContext(cntxt, call)
    fun <- call[[1]]
    args <- call[-1]
    
    if("lapply" == as.character(fun) && isBaseVar("lapply", cntxt)) {
        #case 1: if the call is a lapply style
        cat("[Info]lapply function found in:")
        print(call)
        
        #now start rewrite, 
        l <-  args[[1]]
        f <- args[[2]]
        # no matter which situation, just wrap f with va_vecFun
        vf <- as.call(list(quote(va_vecFun), f))
        v <- as.call(list(quote(va_list2vec), l))
        vcall <- as.call(list(vf, v))
        #finally change back to the original list
        as.call(list(quote(va_vec2list), vcall))
    } else {
        call
    }
}


veccmpSym <- function(sym, cntxt) {
    # TODO: insert _vecexpand
    sym
}

veccmpConst <- function(val, cntxt) {
    # TODO: insert _vecexpand
    val
}


va_compile <- function(e, env = .GlobalEnv, options = NULL) {
    cenv <- makeCenv(env)
    cntxt <- make.toplevelContext(cenv, options)
    cntxt$env <- addCenvVars(cenv, findLocals(e, cntxt))
    
    #now identify the lapply expression
    veccmp(e, cntxt)
}
