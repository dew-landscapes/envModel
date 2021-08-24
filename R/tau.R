

tau <- function(CM, P) {
  #convert both data frames and vectors to matrices
  cmx<-as.matrix(CM)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
    cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc)
  { print("Error: matrix is not square"); break }
  #check P and create if necessary
  if (missing(P))
    P<-rep(1/nr, nr)
  if (length(P) != nc)
  { print("Error: prior probabilities vector has wrong length"); break }
  if (abs(1-sum(P)) > 0.0001)
  { print("Error: prior probabilities must sum to 1"); break }
  n<-sum(cmx)
  d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
  csum<-apply(cmx,2,sum); th2<-(csum%*%P)/n
  tau<-(th1-th2)/(1-th2);
  th3<-sum( (csum + (P*n)) * diag(cmx) ) / n^2;
  rsum<-apply(cmx,1,sum)
  ua<-d/rsum; pa<-d/csum
  th4 <- 0; for (i in 1:nr) for (j in 1:nc)
    th4 <- th4 + (cmx[i,j] * ((csum[i] + P[j]*n)^2));
  th4 <- th4 / n^3;
  th1c <- 1 - th1; th2c <- 1 - th2;
  tv <- 1/n *
    ( ( ( th1 * th1c ) / th2c^2 )
      + ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
      + ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
    )
  list(prior=P, obs=rsum, ref=csum, n=n, tau=tau, tvar=tv, coeff=c(th1, th2, th3, th4))
}
