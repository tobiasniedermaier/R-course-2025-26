
############################################################
# Parallelize a for-loop by rewriting it as an lapply call #
############################################################

#https://www.r-bloggers.com/parallelize-a-for-loop-by-rewriting-it-as-an-lapply-call/amp/

num <- 1:5
y <- list()
for (i in seq_along(num)){
 x <- num[[i]]
 tmp <- sqrt(x)
 y[[i]] <- tmp
}
y

#Mod., damit Schleife keine "side-effects" hat:
y <- list()
for (i in seq_along(num)){
 y[[i]] <- local({ #local() erzwingt, dass Variablen in lokaler Environment gesucht werden und nicht in globaler. S. https://stackoverflow.com/questions/6216968/r-force-local-scope
 x <- X[[i]]
 tmp <- sqrt(x)
 tmp
 })
}
y

#nun als lapply-call:
y <- lapply(1:5, function(x){
 tmp <- sqrt(x)
 tmp
})

#oder wie ich es gemacht hätte:
lapply(1:5, sqrt)

#etwas komplexer:
system.time(y <- lapply(1:5, function(x){
 tmp1 <- sqrt(x)
 tmp2 <- x^2
 list(y=tmp1, z=tmp2)
})) #ca. 1,5 - 1,6 Sekunden auf Laptop
y

#parallelisieren:
library(future.apply)
plan(multiprocess)

k <- 1:100000
system.time(y <- future_lapply(k, function(x){
 tmp1 <- sqrt(x)
 tmp2 <- x^2
 list(y=tmp1, z=tmp2)
}))
y
