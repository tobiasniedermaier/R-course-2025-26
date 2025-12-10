
library(future.apply)
library(parallel)
plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

prob_dup_bday_pll <- function(repl=10000, n=2){
 gen_days <- function(n){sample.int(365, n, replace = TRUE)}
 dup_bday <- function(n){any(duplicated(gen_days(n)))}
 res <- 100*prop.table(table(future_replicate(n=repl, expr=dup_bday(n))))[2]
 return(res)
}

prob_dup_bday_pll(n=2)
res_tmp <- list()
for(i in 2:60){res_tmp[[i]] <- prob_dup_bday_pll(n=i)} #takes some time
(res <- as.numeric(unlist(res_tmp)))

plot(res)
dev.off()
