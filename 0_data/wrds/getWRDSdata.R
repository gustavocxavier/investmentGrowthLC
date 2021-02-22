## Load libary -----------------------------------------------------------------
library(RPostgres)
library(data.table)
library(dplyr)

## Set sample date range -------------------------------------------------------
begdate = '01/01/1966'
enddate = '12/31/2019'

## set wrds user ---------------------------------------------------------------
my_wrds_user = 'gxavier'


## >> RETRIEVE WRDS DATA ## ####################################################
## Load data from WRDS Server

## Connect with WRDS Server ----------------------------------------------------
wrdsConnection <- dbConnect(Postgres(),
                            host='wrds-pgdata.wharton.upenn.edu',
                            port=9737,
                            user=my_wrds_user,
                            password=getPass::getPass(),
                            dbname='wrds',
                            sslmode='require')

## Retrieve Compustat annual data ----------------------------------------------
myQuery <- paste0("select GVKEY, DATADATE, AT, DLTT, DLC,
                   TXDITC, SEQ, CEQ, PSTK, LT, PSTKL, PSTK, PSTKRV,
                   REVT, COGS, XSGA, XRD, RECT, INVT, XPP, DRC, DRLT, AP, XACC,
                   IB, CAPX, sppe, ppent, SICH, CIK,
                   SALE, NI, DP, PPEGT, SSTK, CSHO, AJEX, EBITDA, XINT, IDIT,
                   TXT, TXDC, FYEAR
                   from COMP.FUNDA
                   where INDFMT='INDL'
                   and DATAFMT='STD'
                   and POPSRC='D'
                   and CONSOL='C'
                   and DATADATE between \'", begdate, "\' and \'",enddate,"\'")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
comp_a <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

## Retrieve GICS from Compustat data -------------------------------------------
myQuery <- paste0("select GVKEY, GIND, GSECTOR, GSUBIND
                   from comp.company
                  ")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
gics <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

## Retrieve Name Data Compustat ------------------------------------------------
myQuery <- paste0("select TIC, GVKEY, CUSIP, CIK, CONM, GIND, GSUBIND, NAICS,
                   SIC, YEAR1, YEAR2
                   from comp.names
                  ")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
comp_names <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)
comp_names %>% filter(grepl("GOOGL", tic))

## Retrieve Compustat quartely data --------------------------------------------
myQuery <- paste0("select GVKEY, DATADATE, IBQ, RDQ,
                   PSTKQ, PSTKRQ, SEQQ, CEQQ, ATQ, LTQ, TXDITCQ,
                   FQTR, FYEARQ
                   from COMP.FUNDQ
                   where INDFMT='INDL'
                   and DATAFMT='STD'
                   and POPSRC='D'
                   and CONSOL='C'
                   and DATADATE between \'", begdate, "\' and \'",enddate,"\'")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
comp_q <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

## Retrieve Merged Compustat/CRSP link table -----------------------------------

# wrdsResult <- dbSendQuery(wrdsConnection,"select GVKEY, LPERMNO, LINKDT,
#                                           LINKENDDT, LINKTYPE, LINKPRIM
#                                           from crsp.ccmxpf_lnkhist")
wrdsResult <- dbSendQuery(wrdsConnection,"SELECT gvkey, lpermno as permno,
                                          linktype, linkprim,
                                          linkdt, linkenddt
                                          FROM crsp.ccmxpf_linktable
                                          WHERE substr(linktype,1,1)='L'
                                          AND (linkprim ='C' OR linkprim='P')")
ccmlink <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)


# # Retrieve CRSP data (MSF and MSE each time)
# myQuery <- paste0("select DATE, PERMNO, PERMCO, CFACPR, CFACSHR, SHROUT, PRC, RET, RETX, VOL
#                    from CRSP.MSF
#                    where DATE between \'", begdate, "\' and \'",enddate,"\'")
# wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
# crsp_msf <- dbFetch(wrdsResult, n = -1)
# dbClearResult(wrdsResult)
# 
# 
# myQuery <- paste0("select DATE, PERMNO, SHRCD, EXCHCD
#                    from CRSP.MSE
#                    where DATE between \'", begdate, "\' and \'",enddate,"\'")
# wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
# crsp_mse <- dbFetch(wrdsResult, n = -1)


# Retrieve CRSP data at once ---------------------------------------------------
myQuery <- paste0("SELECT a.permno, a.permco, a.date,
                          b.shrcd, b.exchcd, b.siccd,
                          a.ret, a.retx, a.shrout, a.prc
                   FROM crsp.msf as a
                   LEFT JOIN crsp.msenames as b
                          ON a.permno=b.permno
                          AND b.namedt<=a.date
                          AND a.date<=b.nameendt
                   WHERE a.date   BETWEEN \'", begdate, "\' AND \'",enddate,"\'
                   AND   b.exchcd BETWEEN 1 AND 3")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
crsp_m <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve delisting returns ---------------------------------------------------
myQuery <- paste0("select permno, dlret, dlstdt
                   from crsp.msedelist")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
# Erro: Failed to fetch row: ERROR:  permission denied for schema crsp_q_stock
dlret <- dbFetch(wrdsResult, n = -1)
dbClearResult(wrdsResult)

# Retrieve CRSP names ----------------------------------------------------------
myQuery <- paste0("SELECT permno, permco, ticker, tsymbol, comnam, namedt,
                          nameendt, ncusip, cusip, siccd, naics 
                   FROM crsp.msenames")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
crsp_names <- dbFetch(wrdsResult, n = -1)
setDT(crsp_names)
crsp_names %>% arrange(permno, namedt) %>% filter(ticker=="GOOG")
dbClearResult(wrdsResult)

# Retrieve CRSP header file (MSFHDR)  ------------------------------------------
wrdsResult <- dbSendQuery(wrdsConnection, "select * from MSFHDR")
msfhdr <- dbFetch(wrdsResult, n = -1)

dbClearResult(wrdsResult)


# ## Retrieve WRDS_CIK LINK ------------------------------------------------------
# ## SEC Compustat Merge (CIK link GVKEY)
# wrdsResult <- dbSendQuery(wrdsConnection, "SELECT * FROM WRDSSEC.WCIKLINK_GVKEY")
# 
# scm <- as_tibble(dbFetch(wrdsResult, n=-1)) ; dbClearResult(wrdsResult)


saveRDS(comp_names, file = "0_data/wrds/raw_comp_names.rds")
saveRDS(comp_a,     file = "0_data/wrds/raw_comp_a.rds")
saveRDS(gics,       file = "0_data/wrds/raw_gics.rds")
saveRDS(comp_q,     file = "0_data/wrds/raw_comp_q.rds")
saveRDS(ccmlink,    file = "0_data/wrds/raw_ccmlink.rds")
saveRDS(crsp_m,     file = "0_data/wrds/raw_crsp_m.rds")
saveRDS(crsp_names, file = "0_data/wrds/raw_crsp_names.rds")
saveRDS(msfhdr,     file = "0_data/wrds/raw_msfhdr.rds")
saveRDS(dlret,      file = "0_data/wrds/raw_dlret.rds")
# saveRDS(crsp_msf, file = "0_data/wrds/raw_crsp_msf.rds")
# saveRDS(crsp_mse, file = "0_data/wrds/raw_crsp_mse.rds")
# saveRDS(scm,        file = "0_data/wrds/wciklink_gvkey.rds")

dbDisconnect(wrdsConnection)

## Retrieve compustat annual data for the life cycle proxies ## ----------------

## change the beginning date (consider the cash flow data)
begdate = '01/01/1973'

# Retrieve Compustat annual data with a new query
myQuery <- paste0("SELECT gvkey, datadate, fyear,
                   at, sale, re, ebit, prcc_c, csho,
                   oancf, ivncf, fincf
                   FROM COMP.FUNDA
                   WHERE INDFMT='INDL'
                   AND DATAFMT='STD'
                   AND POPSRC='D'
                   AND CONSOL='C'
                   AND DATADATE between \'", begdate, "\' and \'",enddate,"\'")
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
lcycle <- dbFetch(wrdsResult, n = -1)

dbClearResult(wrdsResult)

dbDisconnect(wrdsConnection)

setDT(lcycle)
saveRDS(lcycle, "0_data/wrds/raw_comp_a_lifecycle_data.rds")
