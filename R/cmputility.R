# TODO: Add comment
# 
# Author: Administrator
###############################################################################


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
                    env = env,
                    ftypes = frameTypes(env)),
            class = "compiler_environment")
}

## Add vars to the top compiler environment frame
addCenvVars <- function(cenv, vars) {
    cenv$extra[[1]] <- union(cenv$extra[[1]], vars)
    cenv
}

## Add vecvars to current context, and return the new context, must 
addCenvVecvars <- function(cntxt, vecvars) {
    cntxt$vecvars <- union(cntxt$vecvars, vecvars)
    cntxt
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

## Only find a var inside a frame, and report whether it is vectorized.
## If yes, also return the expand dimension for future use
findVecvar <- function(var, cntxt) {
    if (typeof(var) == "symbol")
        var <- as.character(var)
    cenv <- cntxt$env
    info <- findCenvVar(var, cenv)
    if (! is.null(info)) {
        list(isVec = (var %in% cntxt$vecvars))
    } else {
        NULL #just return NULL
    }
}

#TODO: simplify the function call
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
## Different to the original byte-code context, here I added localbindings to track local scope's var to expr binding
make.toplevelContext <- function(cenv, options = NULL)
    structure(list(tailcall = TRUE,
                    needRETURNJMP = FALSE,
                    env = cenv,
                    optimize = getCompilerOption("optimize", options),
                    suppressAll = getCompilerOption("suppressAll", options),
                    suppressUndefined = getCompilerOption("suppressUndefined",
                            options),
                    call = NULL,
                    localbindings = list(), #local Basic Block's conservertive bindings
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

# This function is used to track vector space's context
make.vecfunctionContext <- function(cntxt, forms, body) {
    ncntxt <- make.functionContext(cntxt, forms, body)
    #do two more modificaitons
    ncntxt$vecvars <- names(forms)
    ncntxt$dimvars <- c(names(forms)[1], cntxt$dimvars) #note the clos should have at least one form
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


# Find an expression's all symbol reference
findExprSymbols <- function(e, cntxt) {
    if (typeof(e) == "language") {
        syms <- list()
        for(i in seq_along(e)) { syms <- union(syms, findExprSymbols(e[[i]], cntxt)) }
        syms
    }
    else if (typeof(e) == "symbol") {
        list(as.character(e))
    }
    else if (typeof(e) == "pairlist") {
        #formals
        syms <- list()
        for(i in seq_along(e)) { 
            if(EmptySymbol() != e[[i]]) {
                syms <- union(syms, findExprSymbols(e[[i]], cntxt)) 
            }
        }
        syms
    }
    else if (typeof(e) == "bytecode") {
        library(compiler)
        dbe <- invisible(disassemble(e))
        syms <- list()
        for(const in dbe[[3]]){
            if(typeof(const) == "symbol") {
                syms <- c(syms, as.character(const))
            }
        }
        syms
    }
    else if (typeof(e) == "promise")
        cntxt$stop(gettext("cannot vec compile promise literals in code"),
                cntxt)
}

findExprVecVarUse <- function(e, cntxt) {
    syms <- findExprSymbols(e, cntxt)
    #check each of them in the vec var enviroment, decide the usage
    vsyms <- list()
    for(var in syms) {
        if(var %in% cntxt$vecvars) {
            vsyms <- c(vsyms, as.symbol(var))
        }        
    }
    vsyms
}

#TODO: use substitude to replace it
wrapExprToFun <- function(formSyms, bodyExpr) {
    template <- quote(function(a)b)
    formList <- list()
    for(sym in formSyms) {
        formList[[as.character(sym)]] <- EmptySymbol()
    }
    
    template[[2]] <- as.pairlist(formList)
    template[[3]] <- bodyExpr
    template
}

