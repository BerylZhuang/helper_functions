#' multiplot()
#' 
#' plot multiple ggplots in one plot
#' Multiple plot function # from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' 
#' @note
#' @param ... a list of ggplot objects, e.g. p1, p2
#' @param plotlist a list of ggplot objects, e.g. list(p1, p2)
#' @param cols (integer) specify how many columns in the output plot
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored. e.g matrix(c(1,2,3,4), nrow=2, byrow=TRUE)
#' @param order (bool) default T
#'        if T, the order is from top to bottom, then left to right
#'        if F, the order is from left to right then top to bottom 
#'        
#' @return a plot with all the input plots
#' @note 
#' If the layout is something like matrix(c(1,2,3,4), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.The same plot can be repeated
#' @export
#' @examples
# p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
# p2 <- ggplot(mtcars, aes(x = gear, y = mpg)) + geom_point()
# p3 <- ggplot(mtcars, aes(x = carb, y = mpg)) + geom_point()
# multiplot(p1, p2, p3)
# 
# multiplot(plotlist = list(p1, p2, p3), cols = 3)

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, order =T, sequence=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout, else use the layout
    if (is.null(layout)) { ## else will use the layout
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols		
        
        
        if (numPlots==1) { ## if only 1 plot
            print(plots[[1]])
            
        } else { ## if multiplot
            sequence <- seq(1, cols * ceiling(numPlots/cols))
            if (order == T){ # order from top to bottom then left to right
                (layout <- matrix(sequence,
                                  ncol = cols, nrow = ceiling(numPlots/cols)))
            }else{ ## order from left to right then top to bottom
                (layout <- t(matrix(sequence,
                                  nrow = cols, ncol = ceiling(numPlots/cols))))
            }
         }
    }
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
    }
}

