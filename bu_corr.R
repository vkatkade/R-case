library(reshape2)
library(Matrix)

data = read.csv('/Users/vkatkade/Desktop/WNGUAG.csv')
colnames(data);

# Make a copy of the invalid data
invaliddata = data[(data$ERP.Sales.Order.Number == -9999) | (data$Cisco.Bookings.Net <= 0.0),];

# Filter out all invalid data - negative or 0 cisco bookings, and unknown SO number
data = data[(data$ERP.Sales.Order.Number != -9999) & (data$Cisco.Bookings.Net > 0.0),];


# How much data have we filtered out in terms of number of rows and $$ amount
nrow(data);
nrow(invaliddata);
sum(data$Cisco.Bookings.Net);
sum(invaliddata$Cisco.Bookings.Net);

# How many unique customers
length(unique(data$Sold.To.Global.Ultimate.Name));

# How many unique orders
length(unique(data$ERP.Sales.Order.Number));

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

sprintf("          Total Bookings     Common SO Bookings     Percent Common");
sprintf("%s      %.0f      %.0f     %.2f",  "UAG", sum(data_uag$Cisco.Bookings.Net), sum(data_common_uag$Cisco.Bookings.Net), val_common_uag);
sprintf("%s      %.0f      %.0f     %.2f",  "WNG", sum(data_wng$Cisco.Bookings.Net), sum(data_common_wng$Cisco.Bookings.Net), val_common_wng);

sprintf("        Total SO         Common SO         Common SO Percent");
sprintf("%s       %.0f            %.0f         %.2f", "UAG", length(data_uag_so), length(data_common_so), 100*length(data_common_so)/length(data_uag_so));
sprintf("%s       %.0f            %.0f         %.2f", "WNG", length(data_wng_so), length(data_common_so), 100*length(data_common_so)/length(data_wng_so));

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

