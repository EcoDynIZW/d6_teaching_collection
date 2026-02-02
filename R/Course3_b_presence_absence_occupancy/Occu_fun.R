###############################################################################

#################### DO NOT ALTER THESE FUNCTIONS #############################

occu_fun<-function(psi=0.5, covar=F, return=F) {
  
  x<-1:10
  y<-1:10
  
  G<-expand.grid(x,y)
  
  if(covar){
    lpsi=log(psi/(1-psi))
    G$xp<-rnorm(100)
    psi<-plogis(lpsi+0.8*G$xp)
  }
  
  G$z<-psi
  G$occ<-rbinom(nrow(G), 1, G$z)
  image(x=x,y=y, z=matrix(G$z,length(x), length(y) ),
        main=paste0(sum(G$occ), ' cells occupied'),
        col = hcl.colors(20),zlim=c(0,1))
  grid(10, 10, col='black', lty=1)
  points(G$Var1[G$occ==1],
         G$Var2[G$occ==1], pch=19)
  if(return) return(G)
}

detec_fun<-function(p=0.5, psi=0.5, K=1, covar=FALSE,
                    return=FALSE){
  x<-1:10
  y<-1:10
  
  G<-expand.grid(x,y)
  
  G$z<-psi
  
  if(covar){
    lp=log(p/(1-p))
    G$xp<-rnorm(100)
    p<-plogis(lp+0.8*G$xp)
  }
  G$p<-p
  
  G$occ<-rbinom(nrow(G), 1, G$z)
  
  G$y<-rbinom(nrow(G), K, p*G$occ)
  sym<-rep(NA,nrow(G))
  sym[G$occ==1 &  G$y == 0]<-1
  sym[G$occ==1 &  G$y >= 1]<-19
  
  #par(mfrow=c(1,2))
  
  image(x=x,y=y, z=matrix(G$z,length(x), length(y) ),
        main=paste0(sum(G$occ), ' cells occupied'),
        col = hcl.colors(20),zlim=c(0,1))
  mtext(side=3, line=0.6, at=0, cex=0.8, adj=-1, 
        paste0(sum(G$y >0), ' cells with detection'))
  grid(10, 10, col='black', lty=1)
  points(G$Var1[G$occ==1],
         G$Var2[G$occ==1], pch=sym[G$occ==1])
  
  if(return) return(G)
}
