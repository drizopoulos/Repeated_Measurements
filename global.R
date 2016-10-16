library("shiny")
library("lattice")
library("nlme")
library("lme4")
library("geepack")
library("MCMCglmm")
library("splines")
library("corrplot")
library("MASS")
con <- url("https://raw.github.com/drizopoulos/Repeated_Measurements/master/Data.RData")
load(con)
close(con)
options(shiny.maxRequestSize = 1000*1024^2)

htmlPrint <- function (x, comment = NULL) {
    out <- capture.output(x)
    out <- c('<pre><code class="r">', comment, out, 
             '</code></pre>')
    n <- length(out)
    cat(c(out[1], sapply(out[-c(1, n)], function (x) c(x, '\n')),
          out[n]))
}

htmlPrint2 <- function (...) {
    lis <- list(...)
    strings <- sapply(lis, is.character)
    out <- vector("list", length(lis))
    out[strings] <- lis[strings]
    out[!strings] <- lapply(lis[!strings], capture.output)
    out <- unlist(out, use.names = FALSE)
    out <- c('<pre><code class="r">', out, '</code></pre>')
    n <- length(out)
    cat(c(out[1], sapply(out[-c(1, n)], function (x) c(x, '\n')),
          out[n]))
}

actionButton <- function(inputId, label, style = "" , additionalClass = "") {
    if (style %in% c("primary","info","success","warning","danger","inverse","link")) {
        class.style <- paste("btn",style,sep="-")
    } else class.style = ""
    
    tags$button(id=inputId, type="button", 
                class=paste("btn action-button", class.style, additionalClass), label)
}



testCS <- function (cs, value) {
    d <- data.frame(id = rep(1, 6), time = c(0, 0.5, 1, 1.2, 5, 7.5))
    cs1 <- cs(form = ~ time | id, value = value)
    cs1 <- Initialize(cs1, data = d)
    out <- corMatrix(cs1)
    dimnames(out) <- list(paste0("t=", c(0, 0.5, 1, 1.2, 5, 7.5)),
                          paste0("t=", c(0, 0.5, 1, 1.2, 5, 7.5)))
    round(out, 3)
}

testRES <- function (type, params, cor = FALSE) {
    d <- data.frame(id = rep(1, 6), time = c(0, 0.5, 1, 1.2, 5, 7.5))
    Z <- switch(type,
                "intercepts" = model.matrix(~ 1, data = d),
                "intercepts & slopes" = model.matrix(~ time, data = d),
                "intercepts, slopes & slopes^2" = model.matrix(~ time + I(time^2), data = d))
    sigma2 <- params$sigma2
    D <- params$D
    eS <- eigen(D, symmetric = TRUE, only.values = TRUE)
    ev <- eS$values
    if (!all(ev >= - 1e-06 * abs(ev[1L]))) {
        D <- nearPD(D)
    }
    V <- Z %*% D %*% t(Z) + sigma2 * diag(nrow(d))
    out <- if (cor) cov2cor(V) else V
    dimnames(out) <- list(paste0("t=", c(0, 0.5, 1, 1.2, 5, 7.5)),
                          paste0("t=", c(0, 0.5, 1, 1.2, 5, 7.5)))
    round(out, 3)
}

nearPD <- function (M, eig.tol = 1e-06, conv.tol = 1e-07, posd.tol = 1e-08, 
          maxits = 100) {
    if (!(is.numeric(M) && is.matrix(M) && identical(M, t(M)))) 
        stop("Input matrix M must be square and symmetric.\n")
    inorm <- function(x) max(rowSums(abs(x)))
    n <- ncol(M)
    U <- matrix(0, n, n)
    X <- M
    iter <- 0
    converged <- FALSE
    while (iter < maxits && !converged) {
        Y <- X
        T <- Y - U
        e <- eigen(Y, symmetric = TRUE)
        Q <- e$vectors
        d <- e$values
        D <- if (length(d) > 1) 
            diag(d)
        else as.matrix(d)
        p <- (d > eig.tol * d[1])
        QQ <- Q[, p, drop = FALSE]
        X <- QQ %*% D[p, p, drop = FALSE] %*% t(QQ)
        U <- X - T
        X <- (X + t(X))/2
        conv <- inorm(Y - X)/inorm(Y)
        iter <- iter + 1
        converged <- conv <= conv.tol
    }
    X <- (X + t(X))/2
    e <- eigen(X, symmetric = TRUE)
    d <- e$values
    Eps <- posd.tol * abs(d[1])
    if (d[n] < Eps) {
        d[d < Eps] <- Eps
        Q <- e$vectors
        o.diag <- diag(X)
        X <- Q %*% (d * t(Q))
        D <- sqrt(pmax(Eps, o.diag)/diag(X))
        X[] <- D * X * rep(D, each = n)
    }
    (X + t(X))/2
}

naf <- function (x) {
    if (!length(x)) {
        !is.null(x) 
    } else {
        !is.na(x)
    }
}

myRadioButtons <- function(inputId, label, choices, selected = NULL, inline = FALSE, 
                           width = NULL, colors){
    buttons <- radioButtons(inputId, label, choices, selected, inline, width)
    for (i in seq_along(colors)) {
        chs_default <- paste0('<span>', names(choices)[i], '</span>')
        replc <- paste0("<span style='color:", colors[i], "'>", names(choices)[i], 
                        "</span>")
        if (colors[i] == "green")
            replc <- paste0("<strong>", replc, "</strong>")
        buttons <- gsub(chs_default, replc, buttons, fixed = TRUE)
    }
    HTML(buttons)
}

