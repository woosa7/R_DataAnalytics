## Chapter 10: Functional Data Structures ##

arg_function <- function(g) g(seq(1, 100, by=1))

arg_function(mean)
arg_function(sd)


library(rstackdeque)
## Fully Persistent stacks ##
a <- as.rstack(c("p", "q", "r","s","t"))
b <- insert_top(a, c("o"))
c <- without_top(a)
d <- peek_top(a)


## Fast Fully Persistent Queues ##
a <- as.rpqueue(c("p","q","r","s")) 

a$l
a$r
a$lhat

b <- insert_back(a, "t")
c <- without_front(b)
d <- insert_back(c,"v")
e <- insert_back(d,"w")
f <- without_front(e)


## Slowly Pesistent Queues and Dequeues ##
a <- as.rdeque(c("p", "q", "r","s","t","u","v"))
b <- insert_front(a, c("o"))
c <- insert_back(a, c("w"))
d <- without_front(a)
e <- without_back(a)


