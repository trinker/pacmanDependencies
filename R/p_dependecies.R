#' Package Dependencies
#' 
#' List of package dependencies.
#' 
#' @param package Character vector containing packages to load.
#' @return Returns a list of package dependencies.
#' @export
#' @examples
#' \dontrun{
#' pacman::p_load(pacman)
#' (x<- p_dependencies(pacman))
#' plot(x)
#' 
#' pacman::p_load(qdap)
#' (m <- p_dependencies(qdap))
#' plot(m)
#' }
p_dependencies <- function(package) {
    if (!object_check(package) || !is.character(package)) {
        package <- as.character(substitute(package))
    }
    y <- pacman::p_info(package, fields = c("Depends", "Imports", "Suggests", "Enhances"))
    if (is.null(y)) return(y)
    y <- lapply(y, p_extract, FALSE)
    class(y) <- c("p_dependencies", class(y))
    attributes(y)[["package"]] <- package
    y
}

#' Prints an p_dependencies Object
#' 
#' Prints an p_dependencies object.
#' 
#' @param x The p_dependencies object.
#' @param \ldots ignored
#' @method print p_dependencies
#' @export
print.p_dependencies <- function(x, ...){
    class(x) <- "list"
    print(x)
}

#' Plots a p_dependencies Object
#' 
#' Plots a p_dependencies object.
#' 
#' @param x The p_dependencies object.
#' @param legend logical.  If \code{TRUE} a legend is plotted corresponding to 
#' the dependency types.
#' @param legend.x the x co-ordinate to be used to position the legend. Can be 
#' specified by keyword or in any way which is accepted by 
#' \code{\link[grDevices]{xy.coords}}.
#' @param legend.y the y co-ordinate to be used to position the legend. Can be 
#' specified by keyword or in any way which is accepted by 
#' \code{\link[grDevices]{xy.coords}}.
#' @param legend.cex Character expansion factor relative to current 
#' \code{par("cex")}.
#' @param title The title of the plot.  Use \code{NULL} to not include a title.
#' @param \ldots Arguments passed to \code{\link[graphics]{legend}}.
#' @references Adapted from mran's Dependencies Graphs \url{http://mran.revolutionanalytics.com/}
#' @method plot p_dependencies
#' @export
plot.p_dependencies <- function(x, legend = TRUE, legend.x=-1.5, 
    legend.y=-1.05, legend.cex = .8, 
    title = paste("Dependencies for the", attributes(x)[["package"]], 
    "Package"), ...){

    dat <- reshape_dependencies(x) 
    colnames(dat)[2:3] <- c("from", "to")
    dat <- dat[, c("to", "from", "Type", "Relationship")]
    key <- data.frame(
       Type = c("Depends", "Imports", "Suggests", "Enhances"), 
       Color = c("orange", "red", "grey60", "blue"), 
       stringsAsFactors = FALSE)

    dat[["edge.color"]] <- qdapTools::lookup(dat[["Type"]], key)
    myplot <- igraph::graph.data.frame(dat, directed=TRUE)
    igraph::V(myplot)$color <- NA
    igraph::V(myplot)$color[igraph::V(myplot)$name %in% attributes(x)[["package"]]] <- "orange"
    igraph::V(myplot)$color[igraph::V(myplot)$name %in% attributes(x)[["package"]]] <- "orange"
    igraph::V(myplot)$frame.color <- NA
    igraph::V(myplot)$frame.color[igraph::V(myplot)$name %in% attributes(x)[["package"]]] <- "black"
    igraph::V(myplot)$label.color <- "black"
    igraph::V(myplot)$label.family <- "sans"
    igraph::V(myplot)$label.cex <- .85
    igraph::V(myplot)$label.dist <- .01
    igraph::E(myplot)$arrow.size <- .5
    igraph::E(myplot)$color <- dat[["edge.color"]]
    igraph::plot.igraph(myplot)
    if (legend) {
        legend(x=legend.x, y=legend.y, cex=legend.cex, 
            legend = key[["Type"]], fill=key[["Color"]], ...)
    }
    if (!is.null(title)) {
        mtext(title, padj=-1)
    }
}

