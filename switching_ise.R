# Script to determine common customers and their respective bookings amongst Switching and ISE customers
# Copyright (c) Vaibhav Katkade - August 2013

setwd("/Users/vkatkade/Desktop")
library(plyr)
cise <- read.csv("CISE.csv")
c6k <- read.csv("fyc6k.csv")

# Remove all the adjustments and disti stock which have negative custID
cise_filt <- subset(cise, cise$End.Customer.Global.Ultimate.Company.Target.ID>0)
c6k_filt <- subset(c6k, c6k$End.Customer.Global.Ultimate.Company.Target.ID>0)

# Filter on ProductID
c6k_filt_s2t <- subset(c6k_filt, grepl("S2T", c6k_filt$Product.ID))
rm(cise)
rm(c6k)
rm(c6k_filt)

# Order by CustID
c6k_ord<-c6k_filt_s2t[with(c6k_filt_s2t, order(End.Customer.Global.Ultimate.Company.Target.ID)),]
cise_ord<-cise_filt[with(cise_filt, order(End.Customer.Global.Ultimate.Company.Target.ID)),]
rm(c6k_filt_s2t)
rm(cise_filt)

# Take subset of interesting data
c6kbook <- data.frame(c6k_ord$End.Customer.Global.Ultimate.Company.Target.ID, c6k_ord$End.Customer.Global.Ultimate.Name, c6k_ord$Cisco.Bookings.Net, c6k_ord$Cisco.Bookings.Quantity)
colnames(c6kbook) <- c("CustID", "CustName", "Bookings", "Quantity")
cisebook <- data.frame(cise_ord$End.Customer.Global.Ultimate.Company.Target.ID, cise_ord$End.Customer.Global.Ultimate.Name, cise_ord$Product.Bookings.Net, cise_ord$Product.Bookings.Quantity)
colnames(cisebook) <- c("CustID", "CustName", "Bookings", "Quantity")

# Aggregate the orders by CustomerID/CustomerName 
c6kagg<-ddply(c6kbook, .(CustID, CustName), summarize, Bookings=sum(Bookings), Quantity=sum(Quantity))
ciseagg<-ddply(cisebook, .(CustID, CustName), summarize, Bookings=sum(Bookings), Quantity=sum(Quantity))

# Merge the datasets by CustID
switchise<-merge(ciseagg, c6kagg, by.x="CustID", by.y="CustID")
switchise <- data.frame(switchise$CustID, switchise$CustName.x, switchise$Bookings.x, switchise$Bookings.y)
colnames(switchise) <- c("CustID", "CustName", "ISEBookings", "SupBookings")



