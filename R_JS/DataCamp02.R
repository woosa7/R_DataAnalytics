###############################################
#
#   DataCamp : Intermediate R
#
###############################################

#----------------------------------------------
# List
#----------------------------------------------

# make List
log01 = list(success=TRUE, details=list(message="check"), timestamp="2015-09-14 23:01:07")
log02 = list(success=TRUE, details=list(message="all good"), timestamp="2015-09-15 00:00:13")
log03 = list(success=FALSE, details=list(message="stack overflow", location="control room"), timestamp="2015-09-15 08:00:53")
log04 = list(success=TRUE, details=list(message="ok"), timestamp="2015-09-15 08:59:54")
log05 = list(success=TRUE, details=list(message="all good"), timestamp="2015-09-15 11:03:18")
log06 = list(success=TRUE, details=list(message="check"), timestamp="2015-09-15 12:01:49")
log07 = list(success=FALSE, details=list(message="segmentation fault", location="waste"), timestamp="2015-09-15 18:07:58")
log08 = list(success=TRUE, details=list(message="check"), timestamp="2015-09-15 19:08:24")
log09 = list(success=TRUE, details=list(message="all good"), timestamp="2015-09-15 20:10:08")
log10 = list(success=FALSE, details=list(message="human error", location="reactor"), timestamp="2015-09-15 21:14:16")
log11 = list(success=TRUE, details=list(message="ok"), timestamp="2015-09-15 23:14:57")
log12 = list(success=TRUE, details=list(message="check"), timestamp="2015-09-16 01:17:27")
log13 = list(success=TRUE, details=list(message="ok"), timestamp="2015-09-16 02:17:04")
log14 = list(success=TRUE, details=list(message="ok"), timestamp="2015-09-16 08:21:56")
log15 = list(success=TRUE, details=list(message="all good"), timestamp="2015-09-16 10:21:13")

logs = list(log01, log02, log03, log04, log05, log06, log07, log08, log09, log10, log11, log12, log13, log14, log15)
logs
str(logs)


#----------------------------------------------
i <- 1
found <- FALSE

while (found == FALSE) {
    if (logs[[i]]$success == FALSE && logs[[i]]$details$location == "waste") {
        print("found")
        found = TRUE
    } else {
        print(paste("looking : ", as.character(i)))
        i = i + 1
    }
}

#----------------------------------------------
for (i in 1:length(logs)) {
    logs[[i]]$date <- as.Date(logs[[i]]$timestamp)
}

head(logs)
class(logs[[1]]$date)

#----------------------------------------------
failures <- list()

for (log in logs) {
    if (log$success == FALSE) {
        failures <- c(failures, list(log))
    }
}

failures

length(failures)

#----------------------------------------------
extract_info <- function(x, property = "success", include_all = TRUE) {
    info <- c()
    for (log in x) {
        if (include_all == TRUE || log$success == FALSE)
            info <- c(info, log[[property]])
    }
    return(info)
}

extract_info(logs)

extract_info(logs, property = "timestamp")

extract_info(logs, include_all = FALSE)

extract_info(logs, property = c("details", "message"))

extract_info(logs, property = c("details", "location"), include_all = FALSE)

#----------------------------------------------
compute_fail_pct <- function(x) {
    fail = 0
    for (log in x) {
        if (log$success == FALSE)
            fail = fail + 1
    }
    
    return (fail/length(x)*100)
}

compute_fail_pct(logs)

#----------------------------------------------
lapply(logs, function(x) { x$details })
sapply(logs, function(x) { x$timestamp })

results = sapply(logs, function(x) { x$success })
results
mean(results)

sapply(logs, length)
vapply(logs, length, integer(1))

vapply(logs, function(x) {x$success}, logical(1))

#----------------------------------------------
extract_info <- function(x) {
    return(toupper(x$details$message))
}

sapply(logs, extract_info)


