# This is a script which takes in Sales Orders of products of various Business Units, groups these by Sales Order #, 
# and finds a correlation between then various products ordered as part of same Sales Order.


eng_corr <- function() {

library(reshape2)
library(Matrix)

data = read.csv('/Users/vkatkade/Desktop/WNGUAG.csv')
colnames(data);

# Make a copy of the invalid data
invaliddata = data[(data$ERP.Sales.Order.Number == -9999) | (data$Cisco.Bookings.Net <= 0.0),];

# Filter out all invalid data - negative or 0 cisco bookings, and unknown SO number
data = data[(data$ERP.Sales.Order.Number != -9999) & (data$Cisco.Bookings.Net > 0.0),];


# How much data have we filtered out in terms of number of rows and $$ amount
cat("\nTotal Rows: ", nrow(data), "\t", "Total Bookings:", sum(data$Cisco.Bookings.Net));
cat("\nExcluded Rows: ", nrow(invaliddata), "\t", "Excluded Bookings:", sum(invaliddata $Cisco.Bookings.Net));
cat("\n\n");

# How many unique customers
cat("\nNumber of unique customers = ", length(unique(data$Sold.To.Global.Ultimate.Name)));

# How many unique orders
cat("\nNumber of unique orders = ", length(unique(data$ERP.Sales.Order.Number)));
cat("\n\n");


data_uag = data[data$Business.Unit == "UABU",];
data_wng = data[data$Business.Unit == "WNBU",];


# Extract SO data
data_uag_so = data_uag[, c('ERP.Sales.Order.Number') ];
data_wng_so = data_wng[, c('ERP.Sales.Order.Number') ];

# Unique SO numbers
data_wng_so = unique(data_wng_so);
data_uag_so = unique(data_uag_so);

# Common Sales Orders
data_common_so = intersect(data_wng_so, data_uag_so);
data_common_uag = data_uag[data_uag$ERP.Sales.Order.Number %in% data_common_so, ];
data_common_wng = data_wng[data_wng$ERP.Sales.Order.Number %in% data_common_so, ];

# Total Revenue which is correlated
sum(data_common_uag$Cisco.Bookings.Net);
sum(data_common_wng$Cisco.Bookings.Net);
val_common_uag = 100*sum(data_common_uag$Cisco.Bookings.Net) / sum(data_uag$Cisco.Bookings.Net);
val_common_wng = 100*sum(data_common_wng$Cisco.Bookings.Net) / sum(data_wng$Cisco.Bookings.Net);

cat("\t","Total Bookings","\t","Common SO Bookings","\t","%Common","\n")
cat("UAG","\t",sum(data_uag$Cisco.Bookings.Net),"\t",sum(data_common_uag$Cisco.Bookings.Net),"\t",val_common_uag,"\n");
cat("WNG","\t",sum(data_wng$Cisco.Bookings.Net),"\t",sum(data_common_wng$Cisco.Bookings.Net),"\t",val_common_wng,"\n");
cat("\n\n");


cat("\tTotal SO \t Common SO \t Common SO %");
cat("\nUAG", "\t", length(data_uag_so), "\t", length(data_common_so), "\t", 100*length(data_common_so)/length(data_uag_so));
cat("\nWNG",  "\t", length(data_wng_so), "\t", length(data_common_so), "\t", 100*length(data_common_so)/length(data_wng_so));
cat("\n\n");

data_common_uag_wng = rbind(data_common_uag, data_common_wng);
data_common_uag_wng_sorted = data_common_uag_wng[order(data_common_uag_wng$ERP.Sales.Order.Number),]

data2 = dcast(data_common_uag_wng_sorted, ERP.Sales.Order.Number ~ Product.Family, value.var="Cisco.Bookings.Net", sum)
data2 = data2[order(data2$ERP.Sales.Order.Number),]
aggdata <-aggregate(data2, by=list(data2$ERP.Sales.Order.Number),  FUN=sum, na.rm=TRUE)
num_cols = ncol(aggdata)
aggdata <- aggdata[,3:(num_cols)]   # Exclude the index number and ERP SO Number columns
outdata = cor(aggdata, aggdata);
outdata[lower.tri(outdata,diag=TRUE)] <- 0;
write.csv(outdata, "output.csv", sep = ",");
}

