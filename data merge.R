rm(list=ls())
setwd("E:/Muni")
require(gdata)


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
sfdsfdsds



wrds <- wrdsconnect(user=user1, pass=pass1)

run_and_fetch("select COUNT(*) from MSRB.MSRB")
run_and_fetch("select name from dictionary.columns where libname='MSRB' and memname='MSRB'")

bb_data <- read.csv("bond_deals.csv")
cusips <- paste("'",paste(as.vector(bb_data$CUSIP),collapse = "','"),"'",sep="")

run_and_fetch(paste("select COUNT(*) from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))


run_query(paste("select distinct CUSIP,DATED_DATE,SECURITY_DESCRIPTION from MSRB.MSRB where 
                SECURITY_DESCRIPTION in (select distinct SECURITY_DESCRIPTION from MSRB.MSRB 
                where CUSIP in (",cusips,"))",sep = ""))
fetch_last_query("all_cusips")


filtered_data <- read.csv("filtered_muni_bonds.csv",stringsAsFactors = FALSE)
filtered_data$date <- as.Date(filtered_data$date)
names(filtered_data) <- c("CUSIP","desc","date")