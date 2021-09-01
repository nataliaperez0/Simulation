dim = 60
pos = rep(0,dim)
dur = 5
for (t in 1:dur){
  cual = sample(1:dim,1)
  if (runif(1) < 0.5){
    pos[cual] = pos[cual] + 1 
  } else {
    pos[cual] = pos[cual] - 1
  }
  cat(pos, '\n')
}