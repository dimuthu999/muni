rm(list=ls())
setwd("E:/Muni")
require(gdata)
require(dplyr)


# INITIALIZATION ----------------------------------------------------------

.jinit(classpath="C:/Users/dnratnadiwakara/Documents/sas.core.jar", parameters="-Xmx4g")
.jaddClassPath("C:/Users/dnratnadiwakara/Documents/sas.intrnet.javatools.jar")


run_and_fetch <- function(sql){
  fetch(dbSendQuery(wrds,sql),n=-1)
}

run_query <- function(sql){
  res <<-dbSendQuery(wrds,sql)
}


fetch_last_query <- function(name="data",rows=-1)  {
  if(is.null(res)) cat("No res object","\n")
  eval(parse(text=paste(name," <<- fetch(res, n = ",rows,")",sep="")))
}


# RETRIVE DATA ------------------------------------------------------------

wrds <- wrdsconnect(user=user1, pass=pass1)

# Strategy
#--------
# Select the cusips of bloomberg data. Get the security descriptions associated with those cusips.
# Get the distinct cusips, security description and dated date for each security description
# merge bloomberg data using security description and dated date. This way all the tranches of a particualr issue will have the same bloomberg data
# this becomes the set of issues we are going to deal with

bb_data <- read.csv("bond_deals.csv",stringsAsFactors = FALSE)
cusips <- paste("'",paste(as.vector(bb_data$CUSIP),collapse = "','"),"'",sep="")

filtered_data <- unique(read.csv("filtered_muni_bonds.csv",stringsAsFactors = FALSE))
filtered_data$date <- as.Date(filtered_data$date)
names(filtered_data) <- c("CUSIP","desc","date")

bb_data <- merge(bb_data,filtered_data,by=c("CUSIP"),all.x = TRUE)
bb_data['cusip_date'] <- paste(bb_data$CUSIP,bb_data$date,sep="_")

run_and_fetch("select COUNT(*) from MSRB.MSRB")
run_and_fetch("select name from dictionary.columns where libname='MSRB' and memname='MSRB'")


run_and_fetch(paste("select COUNT(*) from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))


run_query(paste("select distinct CUSIP,DATED_DATE,SECURITY_DESCRIPTION from MSRB.MSRB where 
                SECURITY_DESCRIPTION in (select distinct SECURITY_DESCRIPTION from MSRB.MSRB 
                where CUSIP in (",cusips,"))",sep = ""))
fetch_last_query("all_cusips")
all_cusips['cusip_date'] <- paste(all_cusips$CUSIP,all_cusips$DATED_DATE,sep="_")
all_cusips <- all_cusips[!duplicated(all_cusips$cusip_date),]


bb_data <- merge(bb_data,all_cusips,all.x = TRUE,by=c("cusip_date"))





