#SIR Model of malaria. Will eventually include congenital cases.

library(deSolve)

N <- 10000 #population
###Np <- N * .05 #pregnant population
###Nn <- N - Np #non-pregnant population

#Values taken from Olaniyi & Obabiyi (2013)

B <- 0.1 #infection rate
v <- 0.05 #recovery rate
gam <- 1/730 #return to susceptiblity rate

Si <- N - 1
Ii <- 1
Ri <- 0

initial.vals <- c(S=Si, I=Ii, R=Ri)
parms <- c(B, v, gam)


model <- function(times, initial.vals, parms) {
  S = initial.vals[1]
  I = initial.vals[2]
  R = initial.vals[3]
  with(as.list(parms), {
    dS <- -B*I*S/N + gam*R #susceptible
    dI <- B*I*S/N - v*I #infected
    dR <- v*I - gam*R #recovered
    return (list(c(dS, dI, dR)))
    })
}

dt <- seq(0,10000, 0.1)

out <- as.data.frame(lsoda(initial.vals, dt, model, parms))

plot(0, type="n", xlim = c(0,10000), ylim = c(0,12000), cex.lab = 1.5,
     xlab = "Time", ylab = "Population", main ="Malaria Model 1",
     xaxs="i", yaxs="i")

SIRcol = c("black", "gold", "darkgreen")

legend("topright", c("S","I","R"), col= SIRcol, lwd=2)

lines(out$time, out$S, lty = 1, lwd = 2)
lines(out$time, out$I, lty = 1, lwd = 2, col = SIRcol[2])
lines(out$time, out$R, lty = 1, lwd = 2, col = SIRcol[3])
abline(5000,0, col = "darkblue", lty = 3, lwd = 1.5)
