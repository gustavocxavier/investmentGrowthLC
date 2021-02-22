# Report H1a: IP decrease when firm become more mature.
# 

library(data.table)
library(plm)
library(stargazer)
library(tidyverse)
library(lmtest)
library(xtable)

## Functions -------------------------------------------------------------------
##
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  # x is a matrix containing the data
  # method : correlation method. "pearson"" or "spearman"" is supported
  # removeTriangle : remove upper or lower triangle
  # results :  if "html" or "latex"
  # the results will be displayed in html or latex format
  require(xtable)
  
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "*** ", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

##Double-clustering formula (Thompson, 2011)
vcovDC <- function(x, ...){
  vcovHC(x, cluster="group", ...) + vcovHC(x, cluster="time", ...) - 
    vcovHC(x, method="white1", ...)
}

## Winsorize all variables
winsorize <- function (x, limits=0.01) {
  # Source: https://www.r-bloggers.com/winsorization/
  #
  if(length(limits) != 1 || limits < 0 ||
     limits > 0.5) { stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(limits, 1-limits))
  x[ x < lim[1] ] <- lim[1] # NA 
  x[ x > lim[2] ] <- lim[2] # NA 
  x
}

# Load Data --------------------------------------------------------------------

lc <- as_tibble(readRDS("2_pipeline/2_out/2a_life_cycle_faff.rds"))
lc %>% select(gvkey, datadate, LC2=faff) %>%
  mutate(LCintro  = if_else(LC2 == 1, 1, 0, missing = 0)) %>%
  mutate(LCgrowth = if_else(LC2 == 2, 1, 0, missing = 0)) %>%
  mutate(LCshadec = if_else(LC2 == 4, 1, 0, missing = 0)) %>%
  select(gvkey, datadate, LCintro, LCgrowth, LCshadec, LC = LC2) -> lc

# # codigo antigo
# lc <- as_tibble(readRDS("~/Data/EIG/life_cycle.rds"))
# lc %>% select(gvkey, datadate, LC2) %>%
#   mutate(LCintro  = if_else(LC2 == 1, 1, 0, missing = 0)) %>%
#   mutate(LCgrowth = if_else(LC2 == 2, 1, 0, missing = 0)) %>%
#   mutate(LCshadec = if_else(LC2 == 4, 1, 0, missing = 0)) %>%
#   select(gvkey, datadate, LCintro, LCgrowth, LCshadec, LC = LC2) -> lc2

ccm_a <- readRDS("2_pipeline/2_out/1b_ccm_a.rds")

# Filters ----------------------------------------------------------------------
# Exclude firms less than USD 10 million m cap abnormal asset growth (>1)
# The sample consists of 12,125 US firms. Firms excluded from the final sample:
# those with less than USD 10 million market capitalization and those with
# abnormal growth (assets growth or sales growth greater than 1).
ccm_a2 <- ccm_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  # Create Lagged AT
  mutate( lag_sale = dplyr::lag(sale)     ) %>% 
  ungroup %>% 
  # sale growth
  mutate( SalesGrowth = ((sale - lag_sale)/lag_sale) ) %>% 
  mutate( AssetGrowth = ia ) %>% 
  filter( me > 10 & SalesGrowth < 1 & AssetGrowth < 1)

length(unique(ccm_a2$gvkey)) # Number of firms

## Calculate leverage as Total debt/Total assets -------------------------------
ccm_a2 %>% mutate(Leverage = (dltt+dlc)/at) -> ccm_a2

## Calculate IG as change in PPE -----------------------------------------------
ccm_a2 <- ccm_a2 %>% 
  arrange(gvkey, datadate) %>% group_by(gvkey) %>% 
  mutate( InvPPE = ppegt - dplyr::lag(ppegt) ) %>%
  mutate( GrthInvPPE = dplyr::lead(InvPPE) - InvPPE ) %>%
  ungroup

## Calculate future I/A --------------------------------------------------------
ccm_a2 <- ccm_a2 %>% 
  arrange(gvkey, datadate) %>% group_by(gvkey) %>% 
  mutate( ia1 = dplyr::lead(ia) ) %>%
  ungroup

## Select relevant variables ---------------------------------------------------
names(ccm_a2)
ccm_a2 %>% select(gvkey, permno, datadate, year,
                  ia, ia1, d1_ia,
                  # GrthInvPPE, InvPPE,
                  CF, cop, q, sale, dROE, ia, me, Leverage, at) -> ccm_a2


## Eliminate duplicated observations -------------------------------------------
setDT(ccm_a2)
ccm_a2[, year := year(datadate)]

ccm_a2 %>% group_by(gvkey, year) %>% count %>% filter(n>1)
ccm_a2 %>% group_by(permno, year) %>% count %>% filter(n>1)

ccm_a2 %>% filter(permno==18649 & year==1979)
ccm_a2 <- ccm_a2 %>%
  mutate(id_year = paste(permno, year)) %>%
  filter(!duplicated(id_year, fromLast = T)) %>% 
  mutate(id_year = NULL)

ccm_a2 %>% group_by(permno, year) %>% count %>% filter(n>1)
ccm_a2 %>% group_by(gvkey, year) %>% count %>% filter(n>1)

ccm_a2 %>% filter(gvkey=="006025" & year==1992)
ccm_a2 %>% filter(gvkey=="022221" & year==2002)
ccm_a2 %>% filter(gvkey=="065218" & year==2015)
ccm_a2 <- ccm_a2 %>%
  mutate(id_year = paste(gvkey, year)) %>%
  filter(!duplicated(id_year, fromLast = T)) %>% 
  mutate(id_year = NULL)

ccm_a2_bkp2 <- ccm_a2
## Join CCM and LC data --------------------------------------------------------
ccm_a2 <- ccm_a2_bkp2 %>% inner_join(lc, by = c("gvkey", "datadate"))

## Clean the data --------------------------------------------------------------
ccm_a2[is.na(dROE), dROE := 0]
ccm_a2[!is.finite(CF), CF := 0]
ccm_a2 <- ccm_a2[is.finite(rowSums(select(ccm_a2, -(gvkey:year), -LC ))),]
ccm_a2 %>% select(gvkey:datadate)
ccm_a2 %>% select(gvkey:permno) %>% unique

ccm_a2 %>% summary
ccm_a2[, ia       := winsorize(ia,       limits=0.05), by = year]
ccm_a2[, ia1      := winsorize(ia1,      limits=0.05), by = year]
ccm_a2[, d1_ia    := winsorize(d1_ia,    limits=0.05), by = year]
ccm_a2[, CF       := winsorize(CF,       limits=0.05), by = year]
ccm_a2[, cop      := winsorize(cop,      limits=0.05), by = year]
ccm_a2[, dROE     := winsorize(dROE,     limits=0.05), by = year]
ccm_a2[, q        := winsorize(q,        limits=0.05), by = year]
ccm_a2[, sale     := winsorize(sale,     limits=0.05), by = year]
ccm_a2[, me       := winsorize(me,       limits=0.05), by = year]
ccm_a2[, at       := winsorize(at,       limits=0.05), by = year]
ccm_a2[, Leverage := winsorize(Leverage, limits=0.05), by = year]
ccm_a2 %>% summary

## Descriptives ----------------------------------------------------------------
# Log all large variables
ccm_a2 %>% select(ia:at, -ia1) %>% summary

# mydata <- ccm_a2 %>% select(-(gvkey:year), 
#                             #INVt0=IGt0,
#                             -IGt0, -IGc0, -IGc2, -IGc3,
#                             EIG, -(LCintro:LC))

# TODO: Change line above: readRDS("~/Data/EIG/EIG_ENet_serie.rds")
EIG_elnet <- readRDS("~/Data/EIG/EIG_ENet_serie.rds") %>%
  select(gvkey, datadate, IGt1, EIG) %>% 
  arrange(datadate) %>% as.data.table
EIG_elnet[, cor(IGt1, EIG), by=year(datadate)] %>% summarise(mean(V1))

EIG_elnet %>% filter(datadate=="2012-01-31")
EIG_elnet[complete.cases(IGt1, EIG)]
ccm_a3 <- ccm_a2 %>% inner_join(EIG_elnet, by = c("gvkey", "datadate"))

mydata <- ccm_a2 %>%
  mutate_at(vars(sale, me, at), log ) %>% 
  left_join(ccm_a3 %>% select(permno, datadate, EIG),
            by=c("permno", "datadate")) %>%
  select(-permno, -year, -ia1, -(LCintro:LC))
  
stargazer(mydata,
          summary.stat = c("n", "mean", "min", "p25" , "median","p75", "max", "sd"),
          type = "latex", out = "3_output/results/3b_desciptive_stat.txt",
          title="Descriptive statistics", digits=2,
          notes=c("\\small Notes. This table reports descriptive statistics for this study. For variables definitions see",
                  "\\small Appendix \\ref{appen:variables}."))


mydata2 <- mydata %>% select(ia, d1_ia, EIG, CF, cop, q, dROE, Leverage, sale, me, at)

# corstars(mydata, result="none")
# corstars(mydata, result="latex")

mcor <- round(cor(mydata2, use = "pairwise.complete.obs"),2)
mcor[upper.tri(mcor)]<-""
mcor<-as.data.frame(mcor)

rownames(mcor) <- paste0("(", 1:nrow(mcor), ") ", rownames(mcor))
colnames(mcor) <- paste0("(", 1:nrow(mcor), ")")


print(xtable(mcor), file="3_output/results/3b_correlation_matrix_tex.txt", type="latex")
print(xtable(mcor), file="3_output/results/3b_correlation_matrix_htm.html", type="html")

print(xtable(mcor[,1:6]), type="latex")
print(xtable(mcor[,c(7:11,11)]), type="latex")


# Estimate Models --------------------------------------------------------------

model1 <- plm(ia    ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = ccm_a2, model='pooling', index=c('permno', 'year'))
model2 <- plm(ia1   ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = ccm_a2, model='pooling', index=c('permno', 'year'))
model3 <- plm(d1_ia ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = ccm_a2, model='pooling', index=c('permno', 'year'))

EIG_elnet <- readRDS("2_pipeline/2_out/3a_EIG_ENet_serie.rds") %>%
  select(gvkey, datadate, IGt1, EIG) %>% 
  arrange(datadate) %>% as.data.table
EIG_elnet[, cor(IGt1, EIG), by=year(datadate)] %>% summarise(mean(V1))

EIG_elnet %>% filter(datadate=="2012-01-31")
EIG_elnet[complete.cases(IGt1, EIG)]
ccm_a3 <- ccm_a2 %>% inner_join(EIG_elnet, by = c("gvkey", "datadate"))

model4 <- plm(d1_ia ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = ccm_a3, model='pooling', index=c('permno', 'year'))
model5 <- plm(EIG ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = ccm_a3, model='pooling', index=c('permno', 'year'))

# Adjust standard errors
robust.se <- function(x) {
  # coeff <- coeftest(x)
  coeff <- coeftest(x, vcov=vcovDC(x))
  # coeff <- coeftest(x, vcov=vcovHC(x, cluster="group", type="HC1"))
  # coeff <- coeftest(x, vcov=vcovHC(x, cluster="time", type="HC1"))
  # coeff <- coeftest(x, vcov=vcovHC(x, type="HC3", cluster="group"))
  output <- list()
  output$t.stat <- coeff[,"t value"]
  output$p.val <- coeff[,"Pr(>|t|)"]
  return(output)
}
robust_se1 <- robust.se(model1)
robust_se2 <- robust.se(model2)
robust_se3 <- robust.se(model3)
robust_se4 <- robust.se(model4)
robust_se5 <- robust.se(model5)

stargazer(model1, model2, model3, model4, model5,
          title = "Life-cycle stages, investment-to-assets and expected investment growth",
          se = list(robust_se1$t.stat,robust_se2$t.stat,robust_se3$t.stat,
                    robust_se4$t.stat, robust_se5$t.stat),
          p = list(robust_se1$p.val, robust_se2$p.val,robust_se3$p.val,
                   robust_se4$p.val, robust_se5$p.val),
          dep.var.labels.include = F,
          column.labels = c("I/A", "I/A t+1", "d1I/A", "E[d1I/A]"),
          column.separate = c(1,1,2,1),
          font.size="small",
          style="qje", omit.stat="f",
          df = FALSE, intercept.bottom = FALSE,
          type="latex", out = "3_output/results/3b_model.txt")
 
# Descriptive by Life-Cycle ----------------------------------------------------
ccm_a3 %>% group_by(LC) %>%
  summarise(across(ia:EIG, ~ mean(.x, na.rm = TRUE))) %>% 
  select(ia, ia1, d1_ia, EIG) %>% as.data.frame -> df1

ccm_a3 %>% group_by(LC) %>%
  summarise(across(ia:EIG, ~ sd(.x, na.rm = TRUE))) %>% 
  select(ia, ia1, d1_ia, EIG) %>% as.data.frame -> df2

print(xtable(round(df1,3)), file="3_output/results/3b_descriptive_lc_mean_tex.txt", type="latex")
print(xtable(round(df2,3)), file="3_output/results/3b_descriptive_lc_sd_tex.txt", type="latex")

print(xtable(round(df1,3)), file="3_output/results/3b_descriptive_lc_mean_htm.html", type="html")
print(xtable(round(df2,3)), file="3_output/results/3b_descriptive_lc_sd_htm.html", type="html")

print(xtable(round(cbind(df1,df2),3)),
      file="3_output/results/3b_descriptive_lc_all_htm.html", type="html")
print(xtable(round(cbind(df1,df2),3)),
      file="3_output/results/3b_descriptive_lc_all_tex.txt", type="latex")

ccm_a2 %>% group_by(LC) %>% summarise(across(ia:at, ~ sd(.x, na.rm = TRUE)))

ccm_a3 %>% group_by(LC) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(-permno, -year, -(LCintro:LCshadec))
ccm_a3 %>% group_by(LC) %>% summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))

ccm_a3[, cor(d1_ia, EIG), by=year(datadate)] %>% summarise(mean(V1))

# ## Second Table to add ---------------------------------------------------------
# EIG_elnet <- readRDS("~/Data/EIG/EIG_ENet_serie.rds") %>%
#   select(gvkey, datadate, IGt1, EIG) %>% 
#   arrange(datadate) %>% as.data.table
# db <- ccm_a2 %>%
#   select(-IGt1, -EIG) %>% 
#   inner_join(EIG_elnet, by = c("gvkey", "datadate")) %>% 
#   ## Remove duplicated values
#   mutate(id_year = paste(permno, year)) %>%
#   filter(!duplicated(id_year, fromLast = T)) %>%
#   mutate(id_year = NULL) %>%
#   as.data.table
# db
# model1 <- plm(IGt1 ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = db[IGt1>0], model='pooling', index=c('permno', 'year'))
# model2 <- plm(EIG  ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = db[EIG>0], model='pooling', index=c('permno', 'year'))
# model3 <- plm(IGt1 ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = db[IGt1<0], model='pooling', index=c('permno', 'year'))
# model4 <- plm(EIG  ~ LCintro + LCgrowth + LCshadec + CF + q + dROE + log(sale) + Leverage + log(at), data = db[EIG<0], model='pooling', index=c('permno', 'year'))
# 
# # Adjust standard errors
# robust.se <- function(x) {
#   # coeff <- coeftest(x)
#   coeff <- coeftest(x, vcov=vcovDC(x))
#   # coeff <- coeftest(x, vcov=vcovHC(x, cluster="group", type="HC1"))
#   # coeff <- coeftest(x, vcov=vcovHC(x, cluster="time", type="HC1"))
#   # coeff <- coeftest(x, vcov=vcovHC(x, type="HC3", cluster="group"))
#   output <- list()
#   output$t.stat <- coeff[,"t value"]
#   output$p.val <- coeff[,"Pr(>|t|)"]
#   return(output)
# }
# robust_se1 <- robust.se(model1)
# robust_se2 <- robust.se(model2)
# robust_se3 <- robust.se(model3)
# robust_se4 <- robust.se(model4)
# 
# stargazer(model1, model2, model3, model4,
#           title = "Life-cycle stages, investment-to-assets and expected investment growth",
#           se = list(robust_se1$t.stat,robust_se2$t.stat,
#                     robust_se3$t.stat, robust_se4$t.stat),
#           p = list(robust_se1$p.val, robust_se2$p.val,
#                    robust_se3$p.val, robust_se4$p.val),
#           dep.var.labels.include = F,
#           column.labels = c("d1I/A>0", "E[d1I/A]>0", "d1I/A<0", "E[d1I/A]<0"),
#           column.separate = c(1,1,1,1),
#           font.size="small",
#           style="qje", omit.stat="f",
#           df = FALSE, intercept.bottom = FALSE,
#           type="text")
