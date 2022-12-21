rm(list = ls()) 
warnings('off')
k1 = 100
k2 = 600
k3 = 150
f1 = function(t, y1, y2, y3, y4) k1*y1*y2 - k2*y3 - k3*y3
f2 = function(t, y1, y2, y3, y4) k1*y1*y2 - k2*y3
f3 = function(t, y1, y2, y3, y4) k1*y1*y2 - k2*y3 - k3*y3
f4 = function(t, y1, y2, y3, y4) k3*y3
h = 0.001
N = 2000
t = c(0)
y1 = c(1)
y2 = c(10)
y3 = c(0)
y4 = c(0)
for (i in c(1:N)){
  K11 = f1(tail(t,1), tail(y1,1), tail(y2,1), tail(y3,1), tail(y4,1)) 
  K12 = f1(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K11, tail(y2,1) + 0.5*h*K11, tail(y3,1) + 0.5*h*K11, tail(y4,1) + 0.5*h*K11)
  K13 = f1(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K12, tail(y2,1) + 0.5*h*K12, tail(y3,1) + 0.5*h*K12, tail(y4,1) + 0.5*h*K12)
  K14 = f1(tail(t,1) + h, tail(y1,1) + h*K13, tail(y2,1) + h*K13, tail(y3,1) + h*K13, tail(y4,1) + h*K13)
  y1_temple = tail(y1,1) - h/6*(K11 + 2*K12 + 2*K13 + K14)
  K21 = f2(tail(t,1), tail(y1,1), tail(y2,1), tail(y3,1), tail(y4,1))  
  K22 = f2(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K21, tail(y2,1) + 0.5*h*K21, tail(y3,1) + 0.5*h*K21, tail(y4,1) + 0.5*h*K21)
  K23 = f2(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K22, tail(y2,1) + 0.5*h*K22, tail(y3,1) + 0.5*h*K22, tail(y4,1) + 0.5*h*K22)
  K24 = f2(tail(t,1) + h, tail(y1,1) + h*K23, tail(y2,1) + h*K23, tail(y3,1) + h*K23, tail(y4,1) + h*K23)
  y2_temple = tail(y2,1) - h/6*(K21 + 2*K22 + 2*K23 + K24)
  K31 = f3(tail(t,1), tail(y1,1), tail(y2,1), tail(y3,1), tail(y4,1))  
  K32 = f3(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K31, tail(y2,1) + 0.5*h*K31, tail(y3,1) + 0.5*h*K31, tail(y4,1) + 0.5*h*K31)
  K33 = f3(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K32, tail(y2,1) + 0.5*h*K32, tail(y3,1) + 0.5*h*K32, tail(y4,1) + 0.5*h*K32)
  K34 = f3(tail(t,1) + h, tail(y1,1) + h*K33, tail(y2,1) + h*K33, tail(y3,1) + h*K33, tail(y4,1) + h*K33)
  y3_temple = tail(y3,1) + h/6*(K31 + 2*K32 + 2*K33 + K34)
  K41 = f4(tail(t,1), tail(y1,1), tail(y2,1), tail(y3,1), tail(y4,1))  
  K42 = f4(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K41, tail(y2,1) + 0.5*h*K41, tail(y3,1) + 0.5*h*K41, tail(y4,1) + 0.5*h*K41)
  K43 = f4(tail(t,1) + 0.5*h, tail(y1,1) + 0.5*h*K42, tail(y2,1) + 0.5*h*K42, tail(y3,1) + 0.5*h*K42, tail(y4,1) + 0.5*h*K42)
  K44 = f4(tail(t,1) + h, tail(y1,1) + h*K43, tail(y2,1) + h*K43, tail(y3,1) + h*K43, tail(y4,1) + h*K43)
  y4_temple = tail(y4,1) + h/6*(K41 + 2*K42 + 2*K43 + K44)
  if(y1_temple > 0){
    y1 = append(y1,y1_temple)
  }else{
    y1 = append(y1,0)
  }
  if(y2_temple > 0){
    y2 = append(y2,y2_temple)
  }else{
    y2 = append(y2,0)
  }
  y3 = append(y3,y3_temple)
  if(y4_temple > 10){
    y4 = append(y4,10)
  }else{
    y4 = append(y4,y4_temple)
  }
  
  t = append(t,tail(t,1) + h)
}

plot(t,y1,type="l",col="red",main="Concentration",xlab="Time/min",ylab="Concentration/μM")
par(new=TRUE)
plot(t,y2,type="l",col="yellow",pch =0,axes = FALSE,xlab=" ", ylab=" ")
par(new=TRUE)
plot(t,y3,type="l",col="blue",pch =0,axes = FALSE,xlab=" ", ylab=" ")
par(new=TRUE)
plot(t,y4,type="l",col="green",pch =0,axes = FALSE,xlab=" ", ylab=" ")
legend("topright", legend=c("E","S","ES","P"),lty=c(1,1,1,1),col=c("red","yellow", "blue","green"))

V = rep(0,length(y3))
for (i in c(1:length(y3))){
  V[i] = k3*y3[i]
}

plot(y2,V,type="l",col="blue",main="Relationship between [S] and V",xlab="Concentration of S/μM",ylab="Rate of change of the product P")