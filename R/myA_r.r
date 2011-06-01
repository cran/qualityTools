.myADTest = function(x, distribution, ...) {
    if (missing(distribution)) 
        distribution = "normal"
    data.name = deparse(substitute(x))
    dots = list(...)
    parameter = NULL
    smaller = TRUE
    pFun = NULL
    tableValue = FALSE
    A = 0
    x <- sort(x[complete.cases(x)])
    n = length(x)
    if (n < 8) 
        stop("sample size must be greater than 7")
    if (n > 40) 
        warning("sample size is greater than 40")
    if (is.character(distribution)) {
        pFun = .charToDistFunc(distribution, type = "p")
        distribution = tolower(distribution)
        if (is.null(pFun)) 
            stop(paste(deparse(substitute(distribution)), " is not supported!"))
    }
    else {
        pFun = match.fun(distribution)
    }
    if (length(dots) == 0) {
        fittedDistr = fitdistr(x, distribution)
        parameter = fittedDistr$estimate
        if (distribution == "normal") {
            parameter["mean"] = mean(x)
            parameter["sd"] = sd(x)
        }
        p = do.call(pFun, c(list(x), as.list(parameter)))
    }
    else {
        p = pFun(x, ...)
    }
    h = (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
    A = -n - mean(h)
    AA = (1 + 0.75/n + 2.25/n^2) * A
    if (AA < 0.2) {
        pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
    }
    else if (AA < 0.34) {
        pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
    }
    else if (AA < 0.6) {
        pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
    }
    else {
        pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
    }
    if (identical(distribution, "weibull")) {
        AWei = A * (1 + 1/sqrt(n))
        tableValue = TRUE
        smaller = TRUE
        if (AWei < 0.474) {
            pval = 0.25
            smaller = FALSE
        }
        if (AWei >= 0.474) 
            pval = 0.25
        if (AWei >= 0.637) 
            pval = 0.1
        if (AWei >= 0.757) 
            pval = 0.05
        if (AWei >= 0.877) 
            pval = 0.025
        if (AWei >= 1.038) 
            pval = 0.01
    }
    if (identical(distribution, "exponential")) {
        tableValue = TRUE
        return(paste("Sorry, not yet supported!"))
    }
    if (identical(distribution, "logistic")) {
        ALogist = A * (1 + 0.25/n)
        tableValue = TRUE
        smaller = TRUE
        if (ALogist < 0.426) {
            pval = 0.25
            smaller = FALSE
        }
        if (ALogist >= 0.426) {
            pval = 0.25
        }
        if (ALogist >= 0.563) {
            pval = 0.1
        }
        if (ALogist >= 0.66) {
            pval = 0.05
        }
        if (ALogist >= 0.769) {
            pval = 0.025
        }
        if (ALogist >= 0.906) {
            pval = 0.01
        }
        if (ALogist >= 1.1) {
            pval = 0.005
        }
    }
    if (identical(distribution, "gamma")) {
        tableValue = TRUE
        print("TODO: gamma function")
        pval = NA
        smaller = FALSE
    }
    out = list()
    out$data.name = data.name
    out$statistic = as.vector(data.frame(A = A))
    out$parameter = parameter
    out$p.value = as.vector(data.frame(p = pval))
    out$smaller = smaller
    out$tableValue = tableValue
    out$conf.int = NULL
    out$estimate = NULL
    temp = NULL
    if (is.character(distribution)) 
        temp = as.vector(distribution)
    else temp = deparse(substitute(distribution))
    names(temp) = "distribution"
    out$null.value = temp
    out$method = paste("Anderson Darling Test for", temp, "distribution")
    class(out) = "adtest"
    return(out)
}
print.adtest = function(x, digits = 4, quote = TRUE, prefix = "", ...) {
    cat("\n")
    cat(strwrap(x$method, prefix = "\t"), sep = "\n")
    cat("\n")
    cat("data: ", x$data.name, "\n")
    out <- character()
    if (!is.null(x$statistic)) 
        out <- c(out, paste(names(x$statistic), "=", format(round(x$statistic, 4))))
    if (!is.null(x$parameter)) 
        out <- c(out, paste(names(x$parameter), "=", format(round(x$parameter, 3))))
    if (!is.null(x$p.value)) {
        fp <- format.pval(x$p.value, digits = digits)
        if (is.null(x$smaller)) 
            out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "<") fp else paste("=", fp)))
        if (x$tableValue) {
            if (x$smaller) 
                out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "<") fp else paste("<=", fp)))
            else out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "=") fp else paste(">", fp)))
        }
        else {
            out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "<") fp else paste("=", fp)))
        }
    }
    cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
    cat("alternative hypothesis: ")
    if (!is.null(x$null.value)) {
        if (length(x$null.value) == 1) {
            cat("true", names(x$null.value), "is not equal to", x$null.value, "\n")
        }
        else {
            cat(x$alternative, "\nnull values:\n")
            print(x$null.value, ...)
        }
    }
    if (!is.null(x$conf.int)) {
        cat(format(100 * attr(x$conf.int, "conf.level")), "percent confidence interval:\n", format(c(x$conf.int[1L], x$conf.int[2L])), "\n")
    }
    if (!is.null(x$estimate)) {
        cat("sample estimates:\n")
        print(x$estimate, ...)
    }
    cat("\n")
    invisible(x)
} 
