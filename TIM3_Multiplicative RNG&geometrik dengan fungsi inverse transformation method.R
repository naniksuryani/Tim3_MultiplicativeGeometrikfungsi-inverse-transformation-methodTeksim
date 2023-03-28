#TIM 3
#Nanik Suryani B2A020014
#Rizal Okiyanto B2A020016
#Indra Firmansyah B2A020026
#Salmaa Fauziah B2A020028

#Multiplicative RNG dan bilangan acak geometrik dengan fungsi inverse transformation method

#a=35,z0=11123,m=138,n=100

multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3]) 
  View(xj)
  p<-0.5
  R<-xj[,3]
  X<-log(1-R)/log(1-p)
  print(X)
  hist(X)
  
}
multiplicative_RNG(35,11123,138,100)
