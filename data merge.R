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
issuer_bb_data <- read.csv("issuer_bb_data.csv")
issuer_bb_data <- issuer_bb_data[issuer_bb_data$INDUSTRY_SUBGROUP=="Municipal-City",]

bb_data <- bb_data[bb_data$BOND_TO_EQY_TICKER %in% as.vector(issuer_bb_data$BOND_TO_EQY_TICKER),]


all_issues <- read.csv("muni_bonds_id_data.csv",stringsAsFactors = FALSE)
all_issues$X <- NULL
names(all_issues)<-c("CUSIP","SECURITY_DESCRIPTION","DATED_DATE")
all_issues <- all_issues[!duplicated(all_issues$CUSIP),]
bb_data <- merge(bb_data,all_issues,by=c("CUSIP"),all.x=TRUE)
bb_data$SECURITY_DESCRIPTION <- trim(bb_data$SECURITY_DESCRIPTION)
bb_data <- bb_data[!duplicated(bb_data[c('SECURITY_DESCRIPTION','DATED_DATE')]),]
bb_data['CUSIP_BB'] <- bb_data$CUSIP
bb_data$CUSIP <-NULL
cusips <- paste("'",paste(as.vector(bb_data$CUSIP_BB),collapse = "','"),"'",sep="")

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

run_query(paste("select * from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))
fetch_last_query("city_muni_data")
saveRDS(city_muni_data,file = "city_muni_data.rds")


# ZIP CODE DATA -----------------------------------------------------------
rm(list=ls())
setwd("E:/Muni")
require(dplyr)
require(plyr)
require(zoo)

rename <- function(dataframe,oldname,newname) {
  eval(parse(text=paste("names(",dataframe,")[names(",dataframe,")=='",oldname,"']<<-'",newname,"'",sep="")))
  
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


city_muni_data <- readRDS("city_muni_data.rds")
city_muni_data <- city_muni_data[,c("CUSIP","DATED_DATE","SECURITY_DESCRIPTION","TRADE_DATE","MATURITY_DATE","PAR_TRADED","DOLLAR_PRICE","YIELD")]
city_muni_data['TRADE_MONTH'] <- as.yearmon(as.Date(city_muni_data$TRADE_DATE))
city_muni_data$SECURITY_DESCRIPTION <- trim(city_muni_data$SECURITY_DESCRIPTION)

bb_data <- read.csv("bond_deals.csv",stringsAsFactors = FALSE)
issuer_bb_data <- read.csv("issuer_bb_data.csv")
issuer_bb_data <- issuer_bb_data[issuer_bb_data$INDUSTRY_SUBGROUP=="Municipal-City",]
bb_data <- bb_data[bb_data$BOND_TO_EQY_TICKER %in% as.vector(issuer_bb_data$BOND_TO_EQY_TICKER),]
all_issues <- read.csv("muni_bonds_id_data.csv",stringsAsFactors = FALSE)
all_issues$X <- NULL
names(all_issues)<-c("CUSIP","SECURITY_DESCRIPTION","DATED_DATE")
all_issues <- all_issues[!duplicated(all_issues$CUSIP),]
bb_data <- merge(bb_data,all_issues,by=c("CUSIP"),all.x=TRUE)
bb_data$SECURITY_DESCRIPTION <- trim(bb_data$SECURITY_DESCRIPTION)
bb_data <- bb_data[!duplicated(bb_data[c('SECURITY_DESCRIPTION','DATED_DATE')]),]
bb_data['CUSIP_BB'] <- bb_data$CUSIP
bb_data$CUSIP <-NULL
bb_data <- bb_data[,c("BOND_TO_EQY_TICKER","SECURITY_DESCRIPTION","DATED_DATE")]

city_muni_data <- merge(city_muni_data,bb_data,by=c('SECURITY_DESCRIPTION','DATED_DATE'))



zip_database <- read.csv("zipcode-database.csv")
zip_database$City <- tolower(zip_database$City)
zip_database$State <- tolower(zip_database$State)
zip_database['zip'] <- floor(zip_database$Zipcode/100)*100

issuer_bb_data <- read.csv("issuer_bb_data.csv")
issuer_bb_data <- issuer_bb_data[issuer_bb_data$INDUSTRY_SUBGROUP=="Municipal-City",]
issuer_bb_data$CITY_OF_DOMICILE <- tolower(issuer_bb_data$CITY_OF_DOMICILE)
issuer_bb_data$STATE_CODE <- tolower(issuer_bb_data$STATE_CODE)
rename("issuer_bb_data","CITY_OF_DOMICILE","City")
rename("issuer_bb_data","STATE_CODE","State")

issuer_bb_data <- merge(issuer_bb_data,zip_database,by=c("City","State"),all.x = TRUE)
issuer_bb_data <- issuer_bb_data[!duplicated(issuer_bb_data[,c("zip","BOND_TO_EQY_TICKER")]),]

forc_numbers <- read.csv("forc_df.csv",sep = "|")
default_numbers <- read.csv("def1_df.csv",sep="|")
loan_numbers <- read.csv("loan_df.csv",sep="|")

mortgage_data <- merge(loan_numbers,default_numbers,by=c("month","zip"),all.x = TRUE)
mortgage_data <- merge(mortgage_data,forc_numbers,by=c("month","zip"),all.x = TRUE)
mortgage_data['forc_pct'] <- mortgage_data$forc_numbers/mortgage_data$loan_numbers
mortgage_data['default_pct'] <- mortgage_data$def1_numbers/mortgage_data$loan_numbers

mortgage_data <- merge(mortgage_data,issuer_bb_data[,c("BOND_TO_EQY_TICKER","zip")],by="zip")
mortgage_sum_data<-ddply(mortgage_data,.(BOND_TO_EQY_TICKER,month),summarise,forc_pct=median(forc_pct,na.rm = TRUE),default_pct=median(default_pct,na.rm = TRUE))
mortgage_sum_data['TRADE_MONTH']<-as.yearmon(as.Date(mortgage_sum_data$month))

city_muni_data <- merge(city_muni_data,mortgage_sum_data,by=c('BOND_TO_EQY_TICKER','TRADE_MONTH'),all.x = TRUE)

tbill_rates <- read.csv("tbill_rates.csv")
tbill_rates$date<-as.Date(as.character(tbill_rates$date))
tbill_rates['TRADE_MONTH'] <- as.yearmon(tbill_rates$date)
tbill_rates$date <- NULL

city_muni_data <- merge(city_muni_data,tbill_rates,by=c("TRADE_MONTH"),all.x = TRUE)

bb_data <- read.csv("bond_deals.csv",stringsAsFactors = FALSE)
all_issues <- read.csv("muni_bonds_id_data.csv",stringsAsFactors = FALSE)
all_issues$X <- NULL
names(all_issues)<-c("CUSIP","SECURITY_DESCRIPTION","DATED_DATE")
all_issues <- all_issues[!duplicated(all_issues$CUSIP),]
bb_data <- merge(bb_data,all_issues,by=c("CUSIP"),all.x=TRUE)
bb_data$SECURITY_DESCRIPTION <- trim(bb_data$SECURITY_DESCRIPTION)
bb_data <- bb_data[!duplicated(bb_data[c('SECURITY_DESCRIPTION','DATED_DATE')]),]

city_muni_data <- merge(city_muni_data,bb_data,by=c('SECURITY_DESCRIPTION','DATED_DATE'))

saveRDS(city_muni_data,file="city_muni_data_merged.rds")