Solucion_Ecu_Dif_Atras <- function(valini=0, del1=0, fun=0){
  np1 = length(fun)
  funN = rep(0,np1)

  funN[np1] = valini
  x2 = valini
  for (i in (np1-1):1){
    x1 = x2 - del1 * fun[i+1]
    funN[i] = x1
    x2 = x1
  }
  ret = funN
}

x2 = .1
t1 = 0+10
t2 = x2+10
x0 = exp(t2)
del1 = .001

np1 = round((t2-t1)/del1 +1)

fun = rep(x0,np1)
t = seq(t1, t2, length.out = np1)

tt = c()
ff = c()

for (i in 1:80){
   tt = c(t, tt)
   res = Solucion_Ecu_Dif_Atras(x0, del1, fun)
   ff = c(res, ff)
   x0 = res[1]
   fun = res
   t = t-x2
}

print(tt)
plot(tt, ff, type='l')

yy = exp(tt)

points(tt, yy, type='l', col='red')

