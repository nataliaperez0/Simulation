#IDEA 1
prueba = 0.0452145
n = seq(1, 7, 1)
for (i in n) {
  x = trunc(prueba*10^n)/10^n
}

#IDEA 2
prueba = 0.0452145
bueno = 0.0483569

p = trunc(prueba*10^3)/10^3
b = trunc(bueno*10^3)/10^3
if (p == b) {
  deci = 2
  print("2")
} else {
  print("no coindicen")
}

#IDEA 3
bueno = 0.0483569 #número de Wolfram Alpha
prueba = 0.0452145

n = seq(1, 7, 1)

for (i in n) {
  b = trunc(bueno*10^i)/10^i
  p = trunc(prueba*10^i)/10^i
  
  if (p == b) {
    deci = i
    print(deci)
  } else {
    print("no coinciden")
  }
}

#IDEA 4
bueno = 0.048834 #número de Wolfram Alpha
prueba = 0.048214

n = seq(1, 6, 1)

for (i in n) {
  b = trunc(bueno*10^i)/10^i
  p = trunc(prueba*10^i)/10^i
  
  if (p == b) {
    deci = i
  } else {
    print(deci)
    break
  }
}
