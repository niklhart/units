# Compare behavior retained from main. Dimensionless numeric assignment,
# unknown-unit arithmetic and ordinary numeric defaults have separate regressions.
args <- commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(units)
x <- mixed_units(c(a=1,b=NA,c=3,d=4),c('m','s','m','s'))
y <- mixed_units(c(100,2),c('cm','s'))
normalize <- function(x) {
 if (inherits(x,'mixed_units')) return(list(mixed=lapply(as.list(x),normalize),names=names(x)))
 if (is.list(x) && !inherits(x,'symbolic_units')) return(lapply(x,normalize))
 x
}
cases <- alist(x, x[c(4,1,NA,9)], x[FALSE], x['absent'], x[[2]], x[['absent']],
 units(x), drop_units(x), format(x), as.character(x), as.numeric(x),
 c(x,y), c(a=x,b=y), c(x,NULL), unique(x), duplicated(x),
 set_units(x,c('cm','min')), set_units(x,'m'), set_units(x[FALSE],character()),
 set_units(x[1:2],c('cm','s','cm')), set_units(x[1],NA_character_),
 x+y,x-y,x*y,x/y,x==y,x!=y, x*2, 2*x, x/2, 2/x, x+1, -x, x^2,
 x[1:3]*y, x[FALSE]*y,x[FALSE]==y,
 {z<-x; z[c(1,2)]<-y; z}, {z<-x; z[7]<-y[1];z},
 {z<-x;z[1]<-NULL;z}, {z<-x;z[[1]]<-set_units(5,kg);z},
  mixed_units(c(a=5),c('m','s','m')),
 mixed_units(numeric(),character()), mixed_units(numeric(),'m'),
 mixed_units(1:3,c('m','s')), mixed_units(1:4,c('100 m','meter','m','degC')),
 as_units(x[c(1,3)]), as.data.frame(x), x[c(1,1)] == y[1], y[1]*x)
a <- mixed_units(c(a = 1), "m")
b <- mixed_units(c(b = 2, c = 3), "m")
cases <- c(cases, alist(a+b, a==b, 2*b, b*c(d=2), a*1:3, c(a,b),

  units(b[c(1,NA)]),
 {z<-b;z[NA]<-a;z}, {z<-b;z[4]<-a;z}, b[[1.5]], b[c(TRUE,FALSE,TRUE)],
 {z<-b;z[]<-a;z}, unname(b)))
results <- lapply(cases,function(e) tryCatch(normalize(suppressWarnings(eval(e))),error=function(e) 'ERROR'))
saveRDS(list(cases = cases, results = results), args[2])
