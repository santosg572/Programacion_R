Solucion_Ecu_Dif <- function(valini=0, fun=0){
  np1 = length(fun)
  del1 = fun[2]-fun[1]
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
t2 = 4
x0 = 1
del1 = .001

np1 = round((t2-t1)/del1 +1)

fun = rep(1,np1)

res = Solucion_Ecu_Dif(x0, fun)

plot(res, type='l')

