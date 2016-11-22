################################################################
#
# Data Mining : EndTerm Exam - QVC Challenge
#
################################################################

#--------------------------------------------------------------
# Import Data
#--------------------------------------------------------------

customer = read.csv("QVC/customer.csv", header = T, stringsAsFactors = F)
product = read.csv("QVC/product.csv", header = T, stringsAsFactors = F)
airtime = read.csv("QVC/airtime.csv", header = T, stringsAsFactors = F)
order = read.csv("QVC/order.csv", header = T, stringsAsFactors = F)

head(customer)
summary(customer)










