# library("ggplot2")
# library("plyr")
# library("dplyr")
# library("VennDiagram")
# library("knitr")
# library("RColorBrewer")


#' noWarnings()
#' 
#' @description
#' Turn off the warnings
#' 
#' @param x: An script
#' @export
#' @examples
#' noWarnings(dir.create(x))
noWarnings <- function(x){
    suppressMessages(suppressWarnings(x))
}


#' writeTable()
#' 
#' @description
#' wrtie table with msg 
#' 
#' @param f_out: outdir
#' @param file_append: default F. T to append to existing file
#' @param ... other param in write.table()
#' @export
#' @examples
#' writeTable(df, f_out, msg="", quote = F, sep ='\t', row.names =F,...)
writeTable <- function(df, f_out, msg="", quote = F, sep ='\t',
                       row.names =F, file_append = F, ...){
    if(msg !=""){
        sink(f_out, type="output", append =file_append)
        writeLines(msg)
        sink()
        noWarnings(write.table(df, file = f_out, quote = quote, sep = sep,append = T,
                               row.names = row.names, ...))
    }else if(is.null(df)){
        sink(f_out, type="output")
        writeLines(msg)
        sink()
    }else{
        write.table(df, file = f_out, quote = quote, sep = sep,
                    row.names = row.names, append =file_append, ...)
    }
}

#' rbindDF()
#' 
#' @description
#' rbind rows of multiple dataframes, if one or more of the input is not class data.frame. these inputs will be ignored.
#' only data.frames will be rbind.
#' 
#' @param df1: input data.frame 1
#' @param df2: input data.frame 2
#' @param ... more input data.frames
#' @export
#' @examples
#' rbindDF(df1,df2,df3,...)

rbindDF <- function(df1, df2, ...){
    df_ls <- list(df1, df2, ...)
    
    ## check the class of the input df
    class_ls <- vector('character')
    for(i in 1:length(df_ls)){
        class_ls[i] <- class(df_ls[[i]])
    }
    
    
    # if any of them is a data.frame
    (index <- which(class_ls == "data.frame"))
    
    if(length(index) == 0){ # no dataframes
        df <- NULL
    }else if(length(index) == 1){ #only 1 data.frames
        df <- df_ls[[index]]
    }else{ # more than 1 data.frames
        df <- Reduce(rbind, df_ls[index])
    }
    
    return(df)
}


#' filterContain()
#' 
#' Table function: filter a df by column contains a list of specified values
#' 
#' @note
#' only to match the full words in the list (if the value contains two or more, it is a match)
#' @param df A data.frame to be filtered
#' @param column (str) column to be filtered
#' @param value (str list) a list of strs to match with the value column. If a match is found, the record is returned
#' @param verbose [optional] (bool) print if a match is found, and how many records returned. default = F
#' @param ... more parameters in the grepl() function, eg. ignore.case = T
#' @return A data.frame with matched records, including empty df if no match is found
#' @export
#' @examples
#' filterContain(df, column, value, verbose = F, ...)
#' 
filterContain <- function(df, column, value, verbose = F, ...){
    #'  filter a df by column contains a list of specified values
    #'  only to match the full words in the list (if the value contains two or more, will match)
    #'  ... more parameters in the grepl() function, eg. ignore.case = T
    index <- NULL
    for(i in value){
        new_value <- paste0("\\b", i, "\\b") # match the full word in the string
        if(grepl("(|)", value)){
            value <- gsub("\\(", "\\\\(", value)
            value <- gsub("\\)", "\\\\)", value)
        }
        new_index <- which(grepl(new_value, df[, column], ...) == T)
        if(length(new_index) != 0){
            index <- c(index, new_index)
        }else if(verbose){
            print(paste0("no match found for ", i))
        }
    }
    # rm duplicate index
    index <- unique(index)
    
    # return final df
    df <- df[index, ]%>% droplevels()
    if(verbose){
        print(paste0(nrow(df), " returned"))
    }
    return(df)
}

#' filterMatch()
#' 
#' Table function: filter a df by column with exact match of specified values
#' 
#' @note
#' only exact match is returned
#' 
#' @param df A data.frame to be filtered
#' @param column (str) column to be filtered
#' @param value (str) a str to match with the value column.
#' @return A data.frame with matched records, including empty df if no match is found
#' @export
#' @examples
#' column <- "date"
#' value <- "2014-01-01"
#' filterMatch(df, column, value)
#' 
filterMatch <- function(df, column, value){
    #  filter a df by column with exact match of specified values
    index <- which(df[, column] %in% value)
    df <- df[index, ]%>% droplevels()
    return(df)
}

#' filterLess()
#' 
#' Table function: filter a df by a specified date, return df with dates prior or on the date
#' 
#' @param df A data.frame to be filtered
#' @param column (str) the date column to be filtered
#' @param value (date) date in format yyyy-dd-mm
#' @return A data.frame with record date before or equal to the specified date, including empty df if no match is found
#' @export
#' @examples
#' column <- "date"
#' value <- "2014-01-01"
#' filterLess(df, column, value)
filterLess <- function(df, column, value){
    ## filter date by given value, get all records <= value (if using date:format is yyyy-dd-mm)
    new_df_index <- which(df[, column] <= value)
    new_df <- df[new_df_index, ]
}

#' excludeMatch()
#' 
#' Table function: remove rows of column with exact match of a specified value
#' 
#' @note
#' only exact match is removed
#' 
#' @param df A data.frame to be filtered
#' @param column (str) column to be filtered
#' @param value (str) a str to match with the value column, match of the value is removed.
#' @return A data.frame with matched records removed, including empty df if all records matched
#' @export
#' @examples
#' column <- "date"
#' value <- "2014-01-01" # remove records with date 2014-01-01
#' excludeMatch(df, column, value)
excludeMatch <- function(df, column, value){
    #  remove rows of column with exact match of  specified value
    index <- which(df[, column] %in% value)
    if(length(index) != 0){
        df <- df[-index, ] %>% droplevels()
    } else{
        print(paste0("data frame does not contain value ", value, 
                     " in column ", column, ". dataframe not filtered."))
    }
    return(df)
}

#' rbindAllColumns()
#' 
#' Table Functions: rbind two data frames x, y and include all columns from both data frame
#' 
#' @param x (df), if NULL, then return the other df.
#' @param y (df), if NULL, then return the other df.
#' @note
#' If one of the input is null, then return the non-NULL df. 
#' No mapping of the same rows. All rows are added. NA to missing values.
#' If mapping is needed, use mapBindAllColumns()
#' @return a combined df with all the columns from x and y.
#' @export
#' @examples
#' rbindAllColumns(x, y)

rbindAllColumns <- function(x, y) {
    
    ## check if there's a null input
    (index <- which(c(is.null(x), is.null(y))))
    if(length(index) ==1){ ## if one is null
        non_null_index <- setdiff(1:2, index)
        return(list(x,y)[[non_null_index]])
    }else if(length(index) ==2){ ## both are null
        return(NULL)
    }
    # rbind two data frames x, y and include all columns from both data frame
    ## no mapping of the same rows
    
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    
    x[, c(as.character(y.diff))] <- NA
    
    y[, c(as.character(x.diff))] <- NA
    
    return(rbind(x, y))
}



#' mapBindAllColumns()
#' 
#' Table Functions: rbind two data frames x, y and include all columns from both data frame
#' 
#' @param x (df), if NULL, then return the other df.
#' @param y (df), if NULL, then return the other df.
#' @param id_col [optional] (str) a column that are shared between x and y and must have unique values within each df. 
#' If not specified, the row.names of x and y are used for the match
#' @note
#' If row.names are not meaningful (default as numerical index) 
#'        and id_col is not specified, the combined df is not meaningful 
#'        because the rows are combined based on the numerical order
#' If a column name in y is also in x (except for the id_col), the column name in y is suffixed "_2"
#' @return A dataframe that either 1. match the row.names of x and y, and combine the columns from x and y (id_col='')
#' 2. match the content of id_col, and combine the rest of the columns of x and y
#' @export
#' @examples
#' mapBindAllColumns(x, y, id_col = "match_col")

mapBindAllColumns <- function(x, y, id_col='') {
    ## INPUT:
    ##    -x, y are dataframes
    ##    - id_col : a column that are shared between x and y and must be unique value within each df
    ## OUTPUT:
    ##    - a dataframe that either 1. match the row.names of x and y, and combine the columns from x and y (id_col='')
    ##                              2. match the content of id_col, and combine the rest of the columns of x and y
    ## Notes: if row.names are not meaningful (default as numerical index) 
    ##        and id_col is not specified, the combined df is not meaningful
    
    
    ## check if there's a null input
    (index <- which(c(is.null(x), is.null(y))))
    if(length(index) ==1){ ## if one is null
        non_null_index <- setdiff(1:2, index)
        return(list(x,y)[[non_null_index]])
    }else if(length(index) ==2){ ## both are null
        return(NULL)
    }
    
    
    if(!id_col == ""){
        if(!(id_col %in% colnames(x))){
            stop(paste0('x does not contain column, ', id_col))
        }
        if(!(id_col %in% colnames(y))){
            stop(paste0('y does not contain column, ', id_col))
        }
        rownames(x) <- x[, id_col]
        rownames(y) <- y[, id_col]
    }
    
    index <- union(rownames(x), rownames(y))
    
    ## add rows that's not in x, or y
    index_x <- setdiff(index, rownames(x))
    x[index_x, ] <- NA
    index_y <- setdiff(index, rownames(y))
    y[index_y, ] <- NA
    
    ## add the content back to the id col if specified
    if(!id_col == ""){
        x[index_x, id_col] <- index_x
        y[index_y, id_col] <- index_y
    }
    
    ## reorder the rows of x, y respectively for cbind
    if(ncol(x)==1){
        x2 <- as.data.frame(x[index, ])
        colnames(x2) <- colnames(x)
        rownames(x2) <- index
        x <- x2
    }else {x <- x[index, ]}
    
    if(ncol(y)==1){
        y2 <- as.data.frame(y[index, ])
        colnames(y2) <- colnames(y)
        rownames(y2) <- index
        y <- y2
    }else {y <- y[index, ]}
    
    
    ## check if col names in y is duplicated in x. must change to a different (cbind don't work with column names of the same from two dfs)
    (col_overlap <- setdiff(intersect(colnames(x), colnames(y)), id_col))
    # change the col name if there's a overlap with x
    if(length(col_overlap) !=0){
        (index <- grep(paste(col_overlap,collapse="|"), colnames(y)))  # get the index of colnames of all duplicated col names in y
        colnames(y)[index] <- paste0(colnames(y)[index], "_2")
    }
    
    
    ## combine the columns
    # get col names
    if(!id_col == ""){ 
        y_diff <- setdiff(colnames(y), id_col)  # if id col specified (rm col in y for cbind)
    }else{
        y_diff <- colnames(y)  # if use row names (cbind all y cols)
    }
    
    if(length(y_diff) == 1){  # for df with 1 column or df with 1 col + 1 id_col (rename the y column)
        df <- cbind(x, y)   ## x and y 
    }else{
        df <- cbind(x, y[, c(as.character(y_diff))])
    }
    return(df)
}


#' convertToFactors()
#' 
#' convert all character columns of a df to factors
#' 
#' @param x (df)
#' @return A df with all character columns as factors
#' @export

convertToFactors <- function(x){
    # x is a dataframe, convert all character columns to factors
    df <- as.data.frame(unclass(x))
    return(df)
}

#' ggColorHue()
#' 
#' return a list of colors simulate ggplot default color
#' 
#' @param n numeric, number of colors for output
#' @return a list of colors simulate ggplot default color
#' @export
ggColorHue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' plotHisto()
#' 
#' plot histogram(ggplot style)
#' 
#' @note use palette to define color for the factor levels. The palette list is a named list of colors. The names are corresponding to the factor levels
#'     e.g. palette <- c(red = "#ff0000", blue="#333BFF", pink ="#DD1C77", orange = "#CC6600", purple ="#9633FF",l_blue = "#00ffff", l_green = "#9bbc57", yellow = "#ffe700", salmon = "#ff7856",l_purple = "#e47fff")
#' 
#' @param df A data.frame
#' @param column (str) column to plot (column name)
#' @param palette_defined (bool) indicate whether to use as named palette list as defined in palette.
#' default = F: use ggplot default colour scheme
#' @param palette [optional] (named str) define the colour of the factor levels
#' @param plt_alpha [optional] (num) plot alpha (transparency), default = 0.4
#' @param group [optional] (str) fill colour by a column (column name), default is do not group by any column
#' @param title [optional] (str) title of the plot, default none
#' @param facet_gp [optional] (str) the column name for the facet group (as in facet_wrap())
#' @param x_angle [optional] (num) angle of x axis labels, default = 45
#' @param ...  other param in the ggplot2::geom_histogram()
#' @return a ggplot histogram
#' @export

plotHisto <- function(df, column, palette_defined = F, palette = "", plt_alpha = 0.4, 
                      group="", title="", 
                      facet_gp ="",x_angle=45, ...){
    # palette_defined, if levels are defined with a specific color, as defined in default color, 
    # than palette_defined = T to keep all color consistent for factors
    if(group == ""){
        p <- ggplot(df, aes_string(x = column))
    } else {
        p <- ggplot(df, aes_string(x = column, fill = group))
        if (palette_defined & palette != ""){
            p <- p+
                scale_colour_manual(values = palette) +  # customize color
                scale_fill_manual(values = palette)
        }
    }
    # histogram of specified column, filled by group
    p <- p + 
        # geom_histogram(alpha = plt_alpha, position="identity", ...) +
        geom_histogram(alpha = plt_alpha,stat = 'bin', position="identity", ...) +
        ggtitle(title) +
        theme_bw() +
        theme(title = element_text(size = 8, colour = 'black'),
              axis.text.x = element_text(angle = x_angle, hjust = 1))
    if(facet_gp !=""){
        p <- p + facet_wrap(as.formula(paste("~", facet_gp)))    # facet by 
    }
    return(p)
}


#' plotDens()
#' 
#' plot density (ggplot style)
#' 
#' @note use palette to define color for the factors. The palette list is a named list of colors. The names are corresponding to the factor levels
#'     e.g. palette <- c(red = "#ff0000", blue="#333BFF", pink ="#DD1C77", orange = "#CC6600", purple ="#9633FF",l_blue = "#00ffff", l_green = "#9bbc57", yellow = "#ffe700", salmon = "#ff7856",l_purple = "#e47fff")
#' 
#' @param df A data.frame
#' @param column (str) column to plot (column name)
#' @param palette_defined (bool) indicate whether to use as named palette list as defined in palette.
#' default = F: use ggplot default colour scheme
#' @param palette [optional] (named str) define the colour of the factor levels
#' @param plt_alpha [optional] (num) plot alpha (transparency), default = 0.4
#' @param group [optional] (str) fill colour by a column (column name), default is do not group by any column
#' @param title [optional] (str) title of the plot, default none
#' @param facet_gp [optional] (str) the column name for the facet group (as in facet_wrap())
#' @param x_angle [optional] (num) angle of x axis labels, default = 45
#' @param text_size (num) plot title size, default = 8
#' @param log_x (bool) If True, log 10 scale x axis. default = F
#' @param log_y (bool) If True, log 10 scale y axis. default = F
#' @param line_size (num) line size of the density lines. default = 1
#' @param ...  other param in the ggplot2::geom_density()
#' @return A ggplot density
#' @export

plotDens <- function(df, column, palette_defined = F, palette = "", group="", title="",facet_gp="",
                     x_angle=45, text_size=8, log_x=F, log_y=F, 
                     plt_alpha = 0.4, line_size = 1, ...){
    p <- ggplot(df, aes_string(x = column))
    if(group == ""){
        p <- p + geom_density(...)
    } else {
        p <- p + geom_density(aes_string(group=group, colour=group), 
                              size = line_size, alpha = plt_alpha, ...)
        if (palette_defined & palette != ""){
            p <- p+
                scale_colour_manual(values = palette) +  # customize color
                scale_fill_manual(values = palette)
        }
    }
    p <- p + 
        ggtitle(title) +
        theme_bw() +
        theme(title = element_text(size = text_size, colour = 'black'),
              axis.text.x = element_text(angle = x_angle, hjust = 1))
    if(log_x == T){
        p <- p + scale_x_log10()
    }
    if(log_y == T){
        p <- p + scale_y_log10()
    }
    if(facet_gp !=""){
        p <- p + facet_wrap(as.formula(paste("~", facet_gp)))    # facet by 
    }
    
    return(p)
}




#' plotXY()
#' 
#' plot XY (ggplot style)
#' 
#' @note use palette to define color for the factors. The palette list is a named list of colors. The names are corresponding to the factor levels
#'     e.g. palette <- c(red = "#ff0000", blue="#333BFF", pink ="#DD1C77", orange = "#CC6600", purple ="#9633FF",l_blue = "#00ffff", l_green = "#9bbc57", yellow = "#ffe700", salmon = "#ff7856",l_purple = "#e47fff")
#' 
#' @param df A data.frame
#' @param x (str) column to plot x (column name)
#' @param y (str) column to plot y (column name)
#' @param palette_defined (bool) indicate whether to use as named palette list as defined in palette.
#' default = F: use ggplot default colour scheme
#' @param palette [optional] (named str) define the colour of the factor levels
#' @param plt_alpha [optional] (num) plot alpha (transparency), default = 0.4
#' @param group [optional] (str) fill colour by a column (column name), default is do not group by any column
#' @param title [optional] (str) title of the plot, default none
#' @param facet_gp [optional] (str) the column name for the facet group (as in facet_wrap())
#' @param x_angle [optional] (num) angle of x axis labels, default = 45
#' @param text_size (num) plot title size, default = 8
#' @param log_x (bool) If True, log 10 scale x axis. default = F
#' @param log_y (bool) If True, log 10 scale y axis. default = F
#' @param ...  other param in the ggplot2::geom_point()
#' @return A ggplot XY plot
#' @export

plotXY <- function(df, x, y, palette_defined = F, palette = "", group="", 
                   title="",facet_gp="",facet_grid="", 
                   x_angle=45, text_size=8, log_x=F, log_y=F, 
                   plt_alpha = 0.4, ...){
    # scatter plot of x and y
    p <- ggplot(df, aes_string(x = x, y = y))
    if(group == ""){
        p <- p + geom_point(alpha = plt_alpha, ...)
    } else {
        p <- p + 
            geom_point(aes_string(group=group, colour=group, fill = group), alpha = plt_alpha, ...)
        if (palette_defined & palette != ""){
            p <- p+
                scale_colour_manual(values = palette) +  # customize color
                scale_fill_manual(values = palette)
        }
    }
    p <- p + 
        ggtitle(title) +
        theme_bw() +
        theme(title = element_text(size = text_size, colour = 'black'),
              axis.text.x = element_text(angle = x_angle, hjust = 1))
    if(log_x == T){
        p <- p + scale_x_log10()
    }
    if(log_y == T){
        p <- p + scale_y_log10()
    }
    if(facet_gp !=""){
        p <- p + facet_wrap(as.formula(paste(". ~", facet_gp)))    # facet
    }
    if(facet_grid !=""){
        p <- p + facet_grid(as.formula(paste(facet_grid, "~ .")))    # facet (rows)
    }
    return(p)
}



#' plotBox()
#' 
#' Boxplot (ggplot style)
#' 
#' @note use palette to define color for the factors. The palette list is a named list of colors. The names are corresponding to the factor levels
#'     e.g. palette <- c(red = "#ff0000", blue="#333BFF", pink ="#DD1C77", orange = "#CC6600", purple ="#9633FF",l_blue = "#00ffff", l_green = "#9bbc57", yellow = "#ffe700", salmon = "#ff7856",l_purple = "#e47fff")
#' 
#' @param df A data.frame
#' @param x (str) column to plot x (column name), fill colour by x
#' @param y (str) column to plot y (column name)
#' @param palette_defined (bool) indicate whether to use as named palette list as defined in palette.
#' default = F: use ggplot default colour scheme
#' @param palette [optional] (named str) define the colour of the factor levels
#' @param title [optional] (str) title of the plot, default none
#' @param x_angle [optional] (num) angle of x axis labels, default = 30
#' @param text_size (num) plot title size, default = 8
#' @param log_scale (bool) If True, log 10 scale y axis. default = F
#' @param ...  other param in the ggplot2::geom_boxplot()
#' @return A ggplot boxplot
#' @export
#' 
plotBox <- function(df, x, y, palette_defined = F, palette = "", title="", log_scale = F, 
                    x_angle=30, text_size=8, ...){
    # box of specified column, filled by group
    p <- ggplot(df, aes_string(x = x, y = y)) + 
        geom_boxplot(aes_string(fill=x), ...)
    if (palette_defined & palette != ""){
        p <- p+
            scale_colour_manual(values = palette) +  # customize color
            scale_fill_manual(values = palette)
    }
    if(log_scale == T){
        p <- p + scale_y_log10() + ylab(paste0(y, '(log10 scale)'))
    }
    p <- p + 
        ggtitle(title) +
        theme_bw() +
        theme(title = element_text(size = text_size, colour = 'black'),
              axis.text.x = element_text(angle = x_angle, hjust = 1))
    
    return(p)
}


#' plotBar()
#' 
#' bar plots(ggplot style)
#' 
#' @note use palette to define color for the factors. The palette list is a named list of colors. The names are corresponding to the factor levels
#'     e.g. palette <- c(red = "#ff0000", blue="#333BFF", pink ="#DD1C77", orange = "#CC6600", purple ="#9633FF",l_blue = "#00ffff", l_green = "#9bbc57", yellow = "#ffe700", salmon = "#ff7856",l_purple = "#e47fff")
#' 
#' @param df A data.frame
#' @param x (str) column to plot x (column name), categorial value
#' @param y (str) column to plot y (column name), numerical value
#' @param palette_defined (bool) indicate whether to use as named palette list as defined in palette.
#' default = F: use ggplot default colour scheme
#' @param palette [optional] (named str) define the colour of the factor levels
#' @param title [optional] (str) title of the plot, default none
#' @param x_angle [optional] (num) angle of x axis labels, default = 45
#' @param text_size (num) plot title size, default = 8
#' @param log_y (bool) If True, log 10 scale y axis. default = F
#' @param ...  other param in the ggplot2::geom_bar()
#' @return A ggplot bar plot
#' @export
#' 
plotBar <- function(df, x, y, palette_defined = F, palette = "",title="", 
                    x_angle=45, text_size=8, log_y=F, ...){
    # x is categorical, y is numerical
    p <- ggplot(df, aes_string(x = x, y = y)) +
        geom_bar(aes_string(fill=x), stat="identity", ...) 
    if (palette_defined & palette != ""){
        p <- p+
            scale_colour_manual(values = palette) +  # customize color
            scale_fill_manual(values = palette)
    }
    
    p <- p + 
        ggtitle(title) +
        theme_bw() +
        theme(title = element_text(size = text_size, colour = 'black'),
              axis.text.x = element_text(angle = x_angle, hjust = 1)) +
        xlab("") +
        ylab(column) 
    if(log_y == T){
        p <- p + scale_y_log10()
    }
    return(p)
}



#' mapValue()
#' 
#' Given a list x, and a named list of y. Map the values of y to x based on the names of y.
#' 
#' @note The named list of y must contain all values of x in names(y). The returned list is the same order as x.
#' @param x A list with values to be mapped
#' @param y A named list, where names(y) correponding to the value in x, and values 
#' @export
#' @return
#' A named list of the new mapped values
#' @examples
#' x <- c(rep("WT", 3), rep("Rasgrf1_KO", 3))
#' y <- c(Rasgrf1_KO = "#2", WT = "#1", WT2 = "#3")
#' mapValue(x,y)

mapValue <- function(x, y, verbose = F){
  ## check if all values in x has a match named value in y
  if(!all( unique(x) %in% names(y))){
    msg <- paste0("Not all values of x have a match in y. Number of missing values in y : ", 
                  length(setdiff(unique(x), names(y))), "; values: ",
                  paste0(setdiff(unique(x), names(y)), collapse = ", "))
    if(verbose){
      stop(msg)
    }else {
      stop(paste0("Not all values of x have a match in y. Number of missing values in y : ", 
                  length(setdiff(unique(x), names(y)))))
    }
    
  }else{
    overlap <- intersect(unique(x), names(y))
    y <- y[overlap]
    tmp <- y[as.factor(x)]
    tmp <- tmp[x]  # reorder based on x
    return(tmp)
  }
}



#' densPlotMultiLines()
#' 
#' Plot density of a matrix, each line represent a column
#' 
#' @param s A matrix
#' @param main main title of the plot
#' @param xlab label for x
#' @export
#' @return
#' A denstiy plot of multiple lines, each line is colored (represent values in a column)

densPlotMultiLines <- function(s, main ="", xlab = ""){
    junk.x = NULL
    junk.y = NULL
    for(i in 1:dim(s)[2]) {
        junk.x = c(junk.x, density(s[, i])$x)
        junk.y = c(junk.y, density(s[, i])$y)
    }
    (xr <- range(junk.x))
    (yr <- range(junk.y))
    plot(density(s[,1]), xlim = xr, ylim = yr, main = main, xlab = xlab)
    for(i in 1:dim(s)[2]) {
        lines(density(s[, i]), xlim = xr, ylim = yr, col = i)
    }
}








#' vennDiagram2()
#' 
#' Wrapper for VennDiagram::venn.diagram() function
#' 
#' @note
#' the plot will overlap with the old plot, use graphics.off() to clear old plots
#' not recomended for over 5 lists (very messy with more than 5 lists)
#' @param input_ls A list, e.g. input_ls = list(list1 = c("A", "B", "C"), list2= c("A", "B", "D"))
#' @param title (str) plot title
#' @param palette (list of named colors) default is brewer.pal(8,"Accent")
#' @param ... more arguments in VennDiagram::venn.diagram()
#' @return a venn diagram
#' @export
#' @examples
#' graphics.off()
#' input_ls = list(list1 = c("A", "B", "C"), list2= c("A", "B", "D"))
#' pal <- c(list1 = "red", list2 = "blue")  ## if the colour is named the same as the list name, the colour will correponding to the specific list
#' vennDiagram2(input_ls, col_palette=pal)
#' 
#' graphics.off()
#' vennDiagram2(input_ls)
#'
vennDiagram2 <- function(input_ls, title="", col_palette = RColorBrewer::brewer.pal(9,"Set1"), ...){
    
    # color length needs to be the same as the number of lists
    if (length(input_ls) < length(col_palette)){
        col_palette <- col_palette[1:length(input_ls)]}
    
    p <- venn.diagram2(input_ls, 
                       filename = NULL,
                       fill = col_palette,
                       force.unique = T,
                       sep.dist = 0.03,
                       lty = "blank",
                       main = title,
                       euler.d =T,
                       scaled =T,
                       main.cex =2,
                       sub.cex = 1.5,
                       ...)
    grid.draw(p)
}



#' orderCol()
#' 
#' @description
#' reorder df by specified columns
#' 
#' @param df input df
#' @param cols column names in strings (add - if want to sort by descending order()only for numeric ), use decreasing =T for other type
#' @export
#' @examples
#' df <- data.frame(col1 = c('a', 'a', 'b','b'),
#'                  col2 = c('orange', 'red', 'red', 'grey'),
#'                  col3 = c(3,6,8,5))
#' cols = c('col1', 'col2')
#' orderCol(df, cols)
#' orderCol(df, '-col3')
orderCol <- function(df, cols, decreasing = F){
    if(decreasing){
        cmd <- paste0("df[with(df, order(", paste0(cols, collapse = ','), ", decreasing = T)), ]")
    }else{
        cmd <- paste0("df[with(df, order(", paste0(cols, collapse = ','), ")), ]")
    }
    return(eval(parse(text= cmd)))
}


#' rmDup()
#' 
#' @description
#' remove duplicated entries in a dataframe
#' 
#' @param df: An dataframe
#' @export
#' @examples
#' noWarnings(dir.create(x))
rmDup <- function(df){
    return(df[!duplicated(df), ] %>% droplevels())
}

