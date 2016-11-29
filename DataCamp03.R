###############################################
#
#   DataCamp : Writing Functions
#
###############################################

#----------------------------------------------
# replace_missings
#----------------------------------------------

dfz = c(0.312, 0.1215, NA, 0.269, 1.547, 1.423, 0.179, NA, -2.46, 1.079)

repl_missings_mean <- function(x) {
    is_miss <- is.na(x)
    replacement = round(mean(x, na.rm = T),3)
    x[is_miss] <- replacement
    message(cat(as.character(sum(is_miss)), " missings replaced by the mean value"))
    x
}

repl_missings_mean(dfz)


#----------------------------------------------
# Functional Programming
#----------------------------------------------

df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10), d = rnorm(10))
df

col_summary <- function(df, fun) {
    output <- vector("numeric", length(df))
    for (i in seq_along(df)) {
        output[[i]] <- fun(df[[i]])
    }
    output
}

col_summary(df, median)
col_summary(df, mean)
col_summary(df, IQR)



#----------------------------------------------
# purrr - map functions
#----------------------------------------------

# map()     returns a list or data frame
# map_lgl() returns a logical vector
# map_int() returns a integer vector
# map_dbl() returns a double vector
# map_chr() returns a character vector

library(purrr)

map_dbl(df, mean)
map_dbl(df, quantile, probs = 0.5)
map_lgl(df, is.numeric)
map_chr(df, typeof)
map(df, summary)




