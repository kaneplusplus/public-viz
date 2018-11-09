library(sos)
library(tibble)
library(dplyr)
library(e1071)
library(foreach)
library(doMC)
registerDoMC(cores=6)

data(bart)
bart <- as_tibble(bart)[, -2]

form <- pid ~ . - 1
bmm <- model.matrix(form, bart[, -which(names(bart) == "diagnosis")]) %>% 
  as_tibble

user_split <- split(1:nrow(bart), bart$pid)
umm <- foreach(sub = user_split, .combine=rbind) %do% {
  apply(bmm[sub,], 2, mean, na.rm=TRUE)
}

diagnoses <- factor(bart$diagnosis[!duplicated(bart$pid)])
stop("here")
err_perc <- matrix(0, ncol=4, nrow=4, 
                   dimnames=list(c("CONTROL", "SCHZ", "BIPOLAR", "ADHD"),
                                 c("CONTROL", "SCHZ", "BIPOLAR", "ADHD")))
error <- c()
class_res <- foreach (i = 1:nrow(umm), .combine=rbind) %do% {
  fit <- svm(x=as.data.frame(umm)[-i,], y=diagnoses[-i])
  pred <- predict(fit, umm[i,,drop=FALSE]) 
  tibble(pred=pred, actual=diagnoses[i])
}



error_rate <- sum(error) / length(error)
print(error_rate)

dm <- fdist_mat(bart[, -which(names(bart) == "diagnosis")], form)
umm = cmdscale(dm, eig=TRUE, k=12)$points
class_res <- foreach (i = 1:nrow(umm), .combine=rbind) %dopar% {
  fit <- svm(x=as.data.frame(umm)[-i,], y=diagnoses[-i])
  pred <- predict(fit, umm[i,,drop=FALSE])
  tibble(pred=pred, actual=diagnoses[i])
}

error_rate <- sum(error) / length(error)
print(error_rate)

