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

# bond_deals.csv is from bloomberg
bb_data <- read.csv("bond_deals.csv",stringsAsFactors = FALSE)
issuer_bb_data <- read.csv("issuer_bb_data.csv", stringsAsFactors = FALSE)
# Out of 11143 issuers, Municipal-City = 7002;    Municipal-County = 1380 
issuer_bb_data <- issuer_bb_data[issuer_bb_data$INDUSTRY_SUBGROUP=="Municipal-City",]

# bloomberg data for only "Municipal-City" issuers
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

#run_and_fetch("select COUNT(*) from MSRB.MSRB")
#run_and_fetch("select name from dictionary.columns where libname='MSRB' and memname='MSRB'")
#run_and_fetch(paste("select COUNT(*) from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))

run_query(paste("select distinct CUSIP,DATED_DATE,SECURITY_DESCRIPTION from MSRB.MSRB where 
                SECURITY_DESCRIPTION in (select distinct SECURITY_DESCRIPTION from MSRB.MSRB 
                where CUSIP in (",cusips,"))",sep = ""))
fetch_last_query("all_cusips")

all_cusips <- all_cusips[!duplicated(all_cusips$CUSIP),]
all_cusips$SECURITY_DESCRIPTION <- trim(all_cusips$SECURITY_DESCRIPTION)
all_cusips <- merge(all_cusips,bb_data,all.x = TRUE,by=c("SECURITY_DESCRIPTION","DATED_DATE"))
all_cusips <- all_cusips[!is.na(all_cusips$CUSIP_BB),]
#cusips <- paste("'",paste(as.vector(all_cusips$CUSIP),collapse = "','"),"'",sep="")
cusips <- as.vector(unique(all_cusips$CUSIP))


fn = "city_muni_data.csv"
temp <- read.csv("city_muni_data.csv",header = TRUE,sep="|",stringsAsFactors = FALSE)
cusips <- cusips[!cusips %in% unique(temp$CUSIP)]
i=2
wrds <- wrdsconnect(user=user1, pass=pass1)
for (cusip in cusips) {
  cat(i," ",cusip)
  tryCatch({
    run_query(paste("select * from MSRB.MSRB where CUSIP ='",cusip,"'",sep = ""))
    fetch_last_query("city_muni_data")
    cat(" ",nrow(city_muni_data),"\n")
    #if(i==1) write.table(t(names(temp)),file=fn,append = FALSE,sep = "|",quote = FALSE,col.names = FALSE,row.names = FALSE)
    write.table(city_muni_data,file=fn,append = TRUE,sep = "|",quote = FALSE,col.names = FALSE,row.names = FALSE)
  },error=function(cond) {
    cat(" ","error","\n")
    dbDisconnect(wrds)
    wrds <- wrdsconnect(user=user1, pass=pass1)
  })
  i = i+1
}
# 
# run_and_fetch(paste("select COUNT(*) from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))
# run_and_fetch(paste("select COUNT(*) from MSRB.MSRB where CUSIP like ('003626QP6','003626QL5','003626QS0','003626QT8','003626QN1','003626QM3')",sep = ""))
# 
# 
# run_query(paste("select * from MSRB.MSRB where CUSIP in ('989790RZ5','989790SA9')",sep = ""))
# run_query(paste("select * from MSRB.MSRB where CUSIP in (",cusips,")",sep = ""))
# fetch_last_query("city_muni_data")


fn = "city_muni_data.csv"
city_muni_data <- read.csv(fn,header = TRUE,sep="|",stringsAsFactors = FALSE)
temp <- read.csv("city_muni_data2.csv",header = TRUE,sep="|",stringsAsFactors = FALSE)
saveRDS(city_muni_data,file = "city_muni_data_3.rds")


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

# trading details
city_muni_data <- readRDS("city_muni_data_3.rds")
city_muni_data <- city_muni_data[,c("CUSIP","DATED_DATE","SECURITY_DESCRIPTION","TRADE_DATE","MATURITY_DATE","PAR_TRADED","DOLLAR_PRICE","YIELD")]
city_muni_data['TRADE_MONTH'] <- as.yearmon(as.Date(city_muni_data$TRADE_DATE))
city_muni_data$SECURITY_DESCRIPTION <- trim(city_muni_data$SECURITY_DESCRIPTION)

# get issuer and security description data.
# bb_data <- issue information from bloomberg
# issuer_bb_data <- issuer id from bb_data and issuer data from bloomberg
# all_issues <- issue info from MSRB.MSRB
# find issuers "Municipal-City" from issuer_bb_data and get bb_data only for those issuers
# merge security description and dated date to bb_data from all_issues
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

# by merging the trading data with the merged bb_data above, we get the issuer for each issue in city_mini_data
# assumption: issues that have the same description and same date have the same issuer.
city_muni_data <- merge(city_muni_data,bb_data,by=c('SECURITY_DESCRIPTION','DATED_DATE'))


# city and state for each zipcode
zip_database <- read.csv("zipcode-database.csv")
zip_database$City <- tolower(zip_database$City)
zip_database$State <- tolower(zip_database$State)
zip_database['zip'] <- floor(zip_database$Zipcode/100)*100
zip_database <- zip_database[!duplicated(zip_database[,c("zip","City","State")]),]
zip_database <- zip_database[!duplicated(zip_database[,c("City","State")]),]

# get the city and state of each issuer
issuer_bb_data <- read.csv("issuer_bb_data.csv")
issuer_bb_data <- issuer_bb_data[issuer_bb_data$INDUSTRY_SUBGROUP=="Municipal-City",]
issuer_bb_data$CITY_OF_DOMICILE <- tolower(issuer_bb_data$CITY_OF_DOMICILE)
issuer_bb_data$STATE_CODE <- tolower(issuer_bb_data$STATE_CODE)
rename("issuer_bb_data","CITY_OF_DOMICILE","City")
rename("issuer_bb_data","STATE_CODE","State")


issuer_bb_data <- merge(issuer_bb_data,zip_database,by=c("City","State"),all.x = TRUE)
issuer_bb_data <- issuer_bb_data[!duplicated(issuer_bb_data[,c("zip","BOND_TO_EQY_TICKER")]),]

library(DataCombine)

forc_numbers <- read.csv("forc_df.csv",sep = "|")
default_numbers <- read.csv("def1_df.csv",sep="|")
loan_numbers <- read.csv("loan_df.csv",sep="|")

mortgage_data <- merge(loan_numbers,default_numbers,by=c("month","zip"),all.x = TRUE)
mortgage_data <- merge(mortgage_data,forc_numbers,by=c("month","zip"),all.x = TRUE)
mortgage_data['forc_pct'] <- mortgage_data$forc_numbers/mortgage_data$loan_numbers
mortgage_data['default_pct'] <- mortgage_data$def1_numbers/mortgage_data$loan_numbers
mortgage_data['year'] <- as.integer(format(as.Date(as.character(mortgage_data$month)),"%Y"))
mortgage_data <- merge(mortgage_data,issuer_bb_data[,c("BOND_TO_EQY_TICKER","zip")],by="zip")

hpi <- read.csv("HPI_AT_3zip.csv")
hpi$zip <- hpi$zip*100
hpi$quarter <- NULL
hpi$X.1 <- NULL
hpi$X.2 <- NULL
hpi$X <- NULL
hpi <- ddply(hpi,.(zip,year),summarise,hpi_0=mean(hpi,na.rm = TRUE))
hpi <- hpi[order(hpi$zip,hpi$year),]
hpi <- slide(hpi, Var = "hpi_0", GroupVar = "zip",slideBy = -1,NewVar = "hpi_1")
hpi <- slide(hpi, Var = "hpi_0", GroupVar = "zip",slideBy = -2,NewVar = "hpi_2")
hpi <- slide(hpi, Var = "hpi_0", GroupVar = "zip",slideBy = -3,NewVar = "hpi_3")
hpi <- slide(hpi, Var = "hpi_0", GroupVar = "zip",slideBy = -4,NewVar = "hpi_4")
hpi['return_1yr_1'] <- log(hpi$hpi_1/hpi$hpi_2)
hpi['return_2yr_1'] <- log(hpi$hpi_1/hpi$hpi_3)/2
hpi['return_3yr_1'] <- log(hpi$hpi_1/hpi$hpi_4)/3

mortgage_data <- merge(mortgage_data,hpi,by=c("year","zip"),all.x = TRUE)
mortgage_data$month <- as.Date(as.character(mortgage_data$month))
mortgage_sum_data<-ddply(mortgage_data,.(BOND_TO_EQY_TICKER,month),summarise,
                         forc_pct=median(forc_pct,na.rm = TRUE),
                         default_pct=median(default_pct,na.rm = TRUE),
                         hpi_1 = median(hpi_1,na.rm = TRUE),
                         hpi_return_1yr_1 = median(return_1yr_1,na.rm = TRUE),
                         hpi_return_2yr_1 = median(return_2yr_1,na.rm = TRUE),
                         hpi_return_3yr_1 = median(return_3yr_1,na.rm = TRUE))
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

property_tax <- read.csv("property_tax.csv")
property_tax$BOND_TO_EQY_TICKER <- as.character(property_tax$BOND_TO_EQY_TICKER)
property_tax$date <- as.Date(as.character(property_tax$date))
property_tax$GEN_FD_REVENUES <- as.numeric(property_tax$GEN_FD_REVENUES)
property_tax$PROPERTY_TAX_REVENUES <- as.numeric(property_tax$PROPERTY_TAX_REVENUES)
property_tax['property_tax_pct'] <- property_tax$PROPERTY_TAX_REVENUES/property_tax$GEN_FD_REVENUES
property_tax['TRADE_YEAR']<-format(property_tax$date,"%Y")

city_muni_data['TRADE_YEAR']<- format(as.Date(city_muni_data$TRADE_DATE),"%Y")
city_muni_data <- merge(city_muni_data,property_tax,by.x = c("BOND_TO_EQY_TICKER.x","TRADE_YEAR"),by.y = c("BOND_TO_EQY_TICKER","TRADE_YEAR"),all.x = TRUE)

city_muni_data$YIELD <- as.numeric(city_muni_data$YIELD)
city_muni_data['yield_minus_tbill'] <- city_muni_data$YIELD-city_muni_data$tbill_3month
city_muni_data['purpose'] <- ifelse(city_muni_data$MUNI_PURPOSE %in% c("REFUNDING BONDS","REFUNDING NOTES"),"refunding",ifelse(city_muni_data$MUNI_PURPOSE %in% c("PUBLIC IMPS."),"public improvment","other"))
city_muni_data['issue_type'] <- ifelse(city_muni_data$MUNI_ISSUE_TYP %in% c("GENERAL OBLIGATION LTD")
                                  ,"general obligation limited",
                                  ifelse(city_muni_data$MUNI_ISSUE_TYP %in% c("GENERAL OBLIGATION UNLTD"),
                                         "general obligation unlimited","other"))
city_muni_data['offering_type'] <- ifelse(city_muni_data$MUNI_OFFERING_TYP %in% c("COMPETITIVE"),"competitive",
                                     ifelse(city_muni_data$MUNI_OFFERING_TYP %in% c("NEGOTIATED"), "negotiated","other"))
city_muni_data$MUNI_ISSUE_SIZE <- as.numeric(city_muni_data$MUNI_ISSUE_SIZE)
city_muni_data['TRADE_YEAR']<-substr(city_muni_data$TRADE_DATE,1,4)

saveRDS(city_muni_data,file="city_muni_data_merged_3.rds")
