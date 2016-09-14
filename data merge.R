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

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# RETRIVE DATA ------------------------------------------------------------

wrds <- wrdsconnect(user=user1, pass=pass1)

# Strategy
#--------
# Select the cusips of bloomberg data. Get the security descriptions associated with those cusips.
# Get the distinct cusips, security description and dated date for each security description
# merge bloomberg data using security description and dated date. This way all the tranches of a particualr issue will have the same bloomberg data
# this becomes the set of issues we are going to deal with

bb_data <- read.csv("bond_deals.csv",stringsAsFactors = FALSE)
all_issues <- read.csv("muni_bonds_id_data.csv",stringsAsFactors = FALSE)
all_issues$X <- NULL
names(all_issues)<-c("CUSIP","SECURITY_DESCRIPTION","DATED_DATE")
all_issues <- all_issues[!duplicated(all_issues$CUSIP),]
bb_data <- merge(bb_data,all_issues,by=c("CUSIP"),all.x=TRUE)
bb_data <- bb_data[!duplicated(bb_data[c('SECURITY_DESCRIPTION','DATED_DATE')]),]
bb_data['CUSIP_BB'] <- bb_data$CUSIP
bb_data$SECURITY_DESCRIPTION <- trim(bb_data$SECURITY_DESCRIPTION)
cusips <- paste("'",paste(as.vector(bb_data$CUSIP),collapse = "','"),"'",sep="")

run_and_fetch("select COUNT(*) from MSRB.MSRB")
run_and_fetch("select name from dictionary.columns where libname='MSRB' and memname='MSRB'")
run_and_fetch(paste("select COUNT(*) from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))
run_query(paste("select distinct CUSIP,DATED_DATE,SECURITY_DESCRIPTION from MSRB.MSRB where 
                SECURITY_DESCRIPTION in (select distinct SECURITY_DESCRIPTION from MSRB.MSRB 
                where CUSIP in (",cusips,"))",sep = ""))
fetch_last_query("all_cusips")

all_cusips <- all_cusips[!duplicated(all_cusips$CUSIP),]
all_cusips$SECURITY_DESCRIPTION <- trim(all_cusips$SECURITY_DESCRIPTION)
all_cusips <- merge(all_cusips,bb_data,all.x = TRUE,by=c("SECURITY_DESCRIPTION","DATED_DATE"))
all_cusips <- all_cusips[!is.na(all_cusips$CUSIP_BB),]
cusips <- paste("'",paste(as.vector(all_cusips$CUSIP),collapse = "','"),"'",sep="")

run_and_fetch(paste("select COUNT(*) from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))




