# Loading Train.csv dataset which includes the Sales as well as
machine identifier data attributes.
transactions <- read.table(file="Train.csv", header=TRUE, sep=",", quote="\"", row.names=1, fill=TRUE, 
colClasses=c(MachineID="factor", ModelID="factor", datasource="factor", YearMade="character",
           SalesID="character", auctioneerID="factor", UsageBand="factor", saledate="custom.date.2",
           Tire_Size="tire.size", Undercarriage_Pad_Width="undercarriage", Stick_Length="stick.length"),
na.strings=na.values)
# Loading Machine_Appendix.csv for machine configuration information
machines <- read.table(file="Machine_Appendix.csv", header=TRUE, sep=",", quote="\"",
fill=TRUE, colClasses=c(MachineID="character", ModelID="factor", fiManufacturerID="factor"), na.strings=na.values)

# Updating the values to numeric
# updating sale data number
transactions$saledatenumeric <- as.numeric(transactions$saledate)
transactions$ageAtSale <- as.numeric(transactions$saledate - as.Date(transactions$YearMade, format="%Y"))
transactions$saleYear <- as.numeric(format(transactions$saledate, "%Y"))
# updating the month of sale from transaction
transactions$saleMonth <- as.factor(format(transactions$saledate, "%B"))
# updating the date of sale from transaction
transactions$saleDay <- as.factor(format(transactions$saledate, "%d"))
# updating the day of week of sale from transaction
transactions$saleWeekday <- as.factor(format(transactions$saledate, "%A"))
# updating the year of sale from transaction
transactions$YearMade <- as.integer(transactions$YearMade)
# deriving the model price from transaction
transactions$MedianModelPrice <- unsplit(lapply(split(transactions$SalePrice, transactions$ModelID), median), transactions$ModelID)
# deriving the model count from transaction
transactions$ModelCount <- unsplit(lapply(split(transactions$SalePrice, transactions$ModelID), length), transactions$ModelID)
# Merging the transaction and machine data in to dataframe
training.data <- merge(x=transactions, y=machines, by="MachineID")

# write denormalized data out
write.table(x=training.data, file="/temp/training.csv", sep=",", quote=TRUE, row.names=FALSE, eol="\n", col.names=FALSE)

# Create poisson directory at HDFS
bin/hadoop dfs -mkdir /poisson
# Uploading file training.csv at HDFS
bin/hadoop dfs -put temp/training.csv /poisson/

#10% of input data to each sample on avg
frac.per.model <- 0.1
#number of models
num.models <- 50

poisson.subsample <- function(k, input) {
   # this function is used to generate a sample from the current block of data
   generate.sample <- function(i) {
      # generate N Poisson variables
      draws <- rpois(n=nrow(input), lambda=frac.per.model)
      # compute the index vector for the corresponding rows,
      # weighted by the number of Poisson draws
      indices <- rep((1:nrow(input)), draws)
      # emit the rows; RHadoop takes care of replicating the key
      appropriately
      # and rbinding the data frames from different mappers together
      for the
      # reducer
      keyval(i, input[indices, ])
   }
   # here is where we generate the actual sampled data
   c.keyval(lapply(1:num.models, generate.sample))
}

fit.trees <- function(k, v) {
   # rmr rbinds the emitted values, so v is a dataframe
   # note that do.trace=T is used to produce output to stderr to keep the reduce task from timing out
   rf <- randomForest(formula=model.formula, data=v, na.action=na.roughfix, ntree=10, do.trace=FALSE);
   # rf is a list so wrap it in another list to ensure that only
   # one object gets emitted. this is because keyval is vectorized
   keyval(k, list(forest=rf))
}

model.formula <- SalePrice ~ datasource + auctioneerID + YearMade
+ saledatenumeric + ProductSize + ProductGroupDesc.x + Enclosure
+ Hydraulics + ageAtSale + saleYear + saleMonth + saleDay +
saleWeekday + MedianModelPrice + ModelCount + MfgYear

mapreduce(input="/poisson/training.csv", input.format=bulldozer.input.format,
map=poisson.subsample, reduce=fit.trees, output="/poisson/output")

mraw.forests <- values(from.dfs("/poisson/output"))
forest <- do.call(combine, raw.forests)
