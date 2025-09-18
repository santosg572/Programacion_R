Solucion_Ecu_Dif <- function(valini=0, del1=0, fun=0){
  np1 = length(fun)
  funN = rep(0,np1)

  funN[1] = valini
  x1 = valini
  for (i in 2:np1){
    x2 = x1 + del1 * fun[i-1]
    funN[i] = x2
    x1 = x2
  }
  ret = funN
}

t1 = 0
t2 = .1
x0 = 1
del1 = .001

np1 = round((t2-t1)/del1 +1)

fun = rep(1,np1)
t = seq(t1, t2, length.out = np1)

tt = c()
ff = c()

for (i in 1:80){
   tt = c(tt, t)
   res = Solucion_Ecu_Dif(x0, del1, fun)
   ff = c(ff, res)
   x0 = res[np1]
   fun = res
   t = t2+t
}

plot(tt, ff, type='l')

yy = exp(tt)

points(tt, yy, type='l', col='red')

