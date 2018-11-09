library(tibble)
library(readr)
library(dplyr)
library(haven)
library(tidyr)
library(purrr)
library(sos)
library(foreach)
library(doMC)
library(glmnet)
library(e1071)
library(threejs)
registerDoMC(cores=6)

data_path <- file.path("~", "Box Sync", "Clovis", "Tiger-X Data", 
                       "CO1686-008_ADaM_2016-04-27")

all_same <- function(x) {
  apply(x, 2, function(y) {sum(duplicated(y)) == (length(y) - 1)})
}
adorirr <- read_sas(file.path(data_path, "adorirr.sas7bdat"))
adorirr$TRTSDT <- as.integer(adorirr$TRTSDT)
adorirr$TRTEDT <- as.integer(adorirr$TRTEDT)
adorirr <- na.omit(adorirr[, !(names(adorirr) %in% c("ADT", "ADY"))])

vars <- setdiff(names(adorirr)[!all_same(adorirr)], "USUBJID")
form <- as.formula(paste0(" SUBJID ~ ", 
                   paste(vars[-(1:3)], collapse=" + ")))

adcm <- read_sas(file.path(data_path, "adcm.sas7bdat")) 
for (j in seq_len(ncol(adcm))) {
  if (is.character(adcm[,j])) {
    adcm[is.na(adcm[,j]), j] <- "Not Applicable"
  }
}
adcm <- adcm[,!all_same(adcm)]

vars <- setdiff(names(adcm)[!all_same(adcm)], 
  c("USUBJID", "ASTDT", "ASTDY", "AENDT", "AENDY", "CMSTDY", "CMENDY",
    "CMDOSE", "RACEGR1N", "TR01AG1N", "SAFFL"))
form <- as.formula(paste0(" SUBJID ~ ", 
                   paste(vars[-1], collapse=" + ")))
adcm <- na.omit(adcm[,vars])

hosp <- read_sas(file.path(data_path, "adae.sas7bdat")) %>%
  group_by(SUBJID) %>%
  summarize(has_hosp= AEACNOTH %in% c("HOSPITALIZATION", "TREATMENT REQUIRED")

resp <- left_join(unique(adcm[,"SUBJID"]), hosp)
resp$has_hosp <- factor(resp$has_hosp)

#adorirr %>% group_by(SUBJID) %>%
#  summarize(best_resp = AVALC[PARAMN == 3][1])
#resp$best_resp <- factor(resp$best_resp)

sub_split <- split(1:nrow(adcm), adcm$SUBJID)
mm <- model.matrix(form, adcm)
mms <- foreach(sub=sub_split, .combine=rbind) %dopar% {
  apply(mm[sub,], 2, mean, na.rm=TRUE)  
}

rem_rows <- which(is.na(resp$has_hosp))

fit1 <- cv.glmnet(y=as.integer(resp$has_hosp[-rem_rows]), 
                  x=mms[-rem_rows,], family="binomial")

errors <- foreach (i=1:nrow(mms), .combine=c) %dopar% {
  if (i %% 10 == 0) print(i)
  fit <- svm(x=mms[-i,], resp$has_hosp[-i])
  predict(fit, mms[i,,drop=FALSE]) != resp$has_hosp[i] 
}

# Accuracy
print(sum(!errors, na.rm=TRUE) / length(errors))

reduce_dim <- 5
dist_mat <- fdist_mat(adcm, form, reduce_dim=5)

features <- cmdscale(dist_mat, eig=TRUE, k=reduce_dim)$points

fit2 <- cv.glmnet(y=as.integer(resp$has_hosp[-rem_rows]), 
                  x=features[-rem_rows,], family="binomial")

errors <- foreach (i=1:nrow(mms), .combine=c) %dopar% {
  if (i %% 10 == 0) print(i)
  fit <- svm(x=features[-i,], resp$has_hosp[-i])
  predict(fit, features[i,,drop=FALSE]) != resp$has_hosp[i]
}

# Accuracy
print(sum(!errors, na.rm=TRUE) / length(errors))


features <- as_tibble(cbind(SUBJID=rownames(features), as_tibble(features)))
features$SUBJID <- as.character(features$SUBJID)

adeg <- read_sas(file.path(data_path, "adeg.sas7bdat")) %>%
  group_by(SUBJID) %>% 
  summarize(QTC=any("Y" == AVALC[any(PARAMCD %in% c("QTCFGR3", "QTCBGR3"))]))

x <- na.omit(right_join(adeg, features, by="SUBJID"))
fit1 <- cv.glmnet(as.matrix(x[,3:ncol(x)]), as.integer(x$QTC)+1, 
                  family="binomial")

mdf <- as_tibble(
  cbind(adorirr$SUBJID, as_tibble(model.matrix(form, adorirr))))[,-2]
names(mdf)[1] <- "SUBJID"
mdf$SUBJID <- as.character(mdf$SUBJID)
x <- foreach(sub = unique(mdf$SUBJID), .combine=rbind) %do% {
  t(apply(mdf[adorirr$SUBJID == sub, -1], 2, mean, na.rm=TRUE))
}

y <- left_join(tibble(SUBJID=unique(mdf$SUBJID)), adeg, by="SUBJID")
fit2 <- cv.glmnet(x[-234,], as.integer(y$QTC[-234])+1, family="binomial")
