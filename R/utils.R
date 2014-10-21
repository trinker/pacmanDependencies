object_check <- function(x){
    !inherits(try(x, silent = TRUE), "try-error")
}


p_un_R <- function(x) {
    x[["Depends"]] <- x[["Depends"]][!x[["Depends"]] %in% "R"]
    x
}

list2df <- 
function (list.object, col1 = "X1", col2 = "X2") {
    if (is.null(list.object)) return(NULL)
    if (is.null(names(list.object))) {
        names(list.object) <- seq_along(list.object)
    }
    dat <- data.frame(x = unlist(list.object, , FALSE), y = rep(names(list.object), 
        sapply(list.object, length)), stringsAsFactors = FALSE, 
        check.names = FALSE, row.names = NULL)
    colnames(dat) <- c(col1, col2)
    dat
}

list_df2df <- 
function (list.df.object, col1 = "X1") {
    if (is.null(names(list.df.object))) {
        names(list.df.object) <- paste0("L", pad(1:length(list.df.object)))
    }
    list.names <- rep(names(list.df.object), sapply(list.df.object, 
        nrow))
    out <- data.frame(list.names, do.call(rbind, list.df.object), 
        row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
    colnames(out)[1] <- col1
    out
}

reshape_dependencies <- function(x, ...){    
    x <- p_un_R(x)
    x_deps <- unlist(x, use.names = FALSE) 

    p_load(char=unlist(x))

    deps <- lapply(x, function(y) {
        y2 <- setNames(lapply(y, function(z) {
            m <- list2df(p_un_R(p_dependencies(z)), "Dependency", "Type")
            m <- m[m[["Dependency"]] %in% x_deps, ] 
            m
        }), y)
        y2 <- y2[!sapply(y2, is.null)]
        list_df2df(y2[sapply(y2, nrow) > 0], "Package")
    })

    dat <- data.frame(
        Package = attributes(x)[["package"]],
        list2df(x, "Dependency", "Type")
    )
    list_df2df(list(Main = dat, Depends = data.frame(do.call(rbind, deps), 
        row.names=NULL)), "Relationship")
}