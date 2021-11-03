library(tcltk)
timer = 2
pb <- tkProgressBar("Timer")
start = Sys.time()
while(TRUE) {
  elapsed = as.numeric(difftime(Sys.time(), start, units = 'secs'))
  remaining = timer - elapsed
  Sys.sleep(0.1)
  setTkProgressBar(pb, remaining/timer, label = sprintf("Time remaining: %i seconds", round(remaining)))
  print(elapsed)
  if (remaining <= 0) break
  
}
Sys.sleep(2)
close(pb)

seg = 10
Sys.sleep(seg)

while ( {
  
}

library(R.utils)
withTimeout(Sys.sleep(10), timeout = 1)

require(R.utils)

for(i in 1:5) {
  tryCatch(
    expr = {
      withTimeout({Sys.sleep(i); cat(i, "\n")}, 
                  timeout = 5)
    }, 
    TimeoutException = function(ex) cat("Timeout. Skipping.\n")
  )
}

repeat{
  
}
