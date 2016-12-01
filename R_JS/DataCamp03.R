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


list_of_results <- list(
    list(a = 1, b = "Nice"), 
    list(a = 2, b = "Good"), 
    list(a = 3, b = "Wonderful")
)

map(list_of_results, "a")
map(list_of_results, 1)   # 1st element


#----------------------------------------------
# anonymous functions

cars = split(mtcars, mtcars$cyl)

cars[[1]]

map(cars, function(df) lm(mpg ~ hp+drat+wt+qsec, data = df))
map(cars, ~ lm(mpg ~ hp+drat+wt+qsec, data = .))

map_dbl(cars, function(df) mean(df$disp))
map_dbl(cars, ~ mean(.$disp))

# result list를 map function으로 조회
models = map(cars, ~ lm(mpg ~ hp+drat+wt+qsec, data = .))

coefs = map(models, coef)   # <-- coef(models[[1]])
map(coefs, "wt")    # name
map_dbl(coefs, 2)   # index


# purrr ㅡ pipe ( %>% ) 사용 가능.

mtcars %>% 
    split(mtcars$cyl) %>%
    map(~ lm(mpg ~ wt, data = .)) %>%
    map(coef) %>% 
    map_dbl("wt")


models <- mtcars %>% 
    split(mtcars$cyl) %>%
    map(~ lm(mpg ~ wt, data = .))

models %>% map(summary) %>% map_dbl("r.squared")


#----------------------------------------------
# Dealing with failure / purrr :: safely
#----------------------------------------------

library(purrr)

# Create safe_readLines() by passing readLines() to safely()
safe_readLines <- safely(readLines)

urls <- list(
    example = "http://example.org",
    krx = "http://asdf.kr",
    asdf = "http://asdf"
)

html <- map(urls, safe_readLines)
str(html)

html[[1]]$result
html[[3]]$error

# transpose 하여 result, error 선택을 쉽게 한다.
str(transpose(html))
res = transpose(html)[["result"]]
res
errs = transpose(html)[["error"]]
errs

is_ok = map_lgl(errs, is_null)
is_ok

res[is_ok]      # successful results
urls[!is_ok]    # input from unsuccessful results


#----------------------------------------------
# Map
#----------------------------------------------

# map()         - iterate over one arguments
# map2()        - iterate over two arguments
# pmap()        - iterate over many arguments : list(arguments)
# invoke_map()  - iterate over functions and arguments

# make random number
n = list(5, 10, 20)
mu = list(1, 5, 10)
sd = list(0.1, 1, 0.1)

map(n, rnorm)

map2(n, mu, rnorm)

pmap(list(n, mu), rnorm)

pmap(list(n, mu, sd), rnorm)


# Mapping : functions & arguments
f <- list("rnorm", "runif", "rexp")

rnorm_params <- list(mean = 10)
runif_params <- list(min = 0, max = 5)
rexp_params <- list(rate = 5)

params <- list(
    rnorm_params,
    runif_params,
    rexp_params
)

invoke_map(f, params, n = 5) # f & params 를 1:1로 invoke


#----------------------------------------------
# walk : map 과 동일하지만 아무것도 리턴하지 않음.
# 주로 printing, plotting, saving 처리시에 사용

f <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")

params <- list(
    Normal = list(mean = 10),
    Uniform = list(min = 0, max = 5),
    Exp = list(rate = 5)
)

samples = invoke_map(f, params, n = 50)

walk(samples, hist)

# 위 plot에 x축 범위를 설정
breaks_list <- list(
    Normal = seq(6, 16, 0.5),   # bins - x축 범위지정.
    Uniform = seq(0, 5, 0.25),
    Exp = seq(0, 1.5, 0.1)
)

walk2(samples, breaks_list, hist)   # 2 arguments

# 각 sample의 범위를 30개의 bin으로 쪼개는 지점 찾는 함수
find_breaks <- function(x) {
    rng <- range(x, na.rm = TRUE)
    seq(rng[1], rng[2], length.out = 30)
}

samples[[1]]
find_breaks(samples[[1]])

nice_breaks <- map(samples, find_breaks)  
walk2(samples, nice_breaks, hist)   # x축 범위가 아닌 bin point 설정

# plot title
p_titles = c("Normal(10, 1)", "Uniform(0, 5)", "Exp(5)")

# pwalk() : pmap 처럼 여러 개의 arguments를 list로 설정.
pwalk(list(x = samples, breaks = nice_breaks, main = p_titles), hist, xlab = "")

