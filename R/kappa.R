

#' Calculate kappa statistics from confusion matrix
#'
#' Code from _Technical Note: Statistical methods for accuracy assessment of
#' classified thematic maps_ \insertRef{rossiter2004statistical}{envModel}.
#'
#' @param CM Confusion matrix.
#' @param by_class Logical. Return a row per class, or mean values across all
#' classes.
#'
#' @return Tibble
#' @export
#'
#' @examples
make_kappa_tibble <- function(CM, by_class) {
  #convert both data frames and vectors to matrices
  cmx<-as.matrix(CM)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
    cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc)
  { print("Error: matrix is not square"); break }
  n<-sum(cmx)
  d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
  th1v<-((th1*(1-th1))/n)
  csum<-apply(cmx,2,sum); rsum<-apply(cmx,1,sum)
  ua<-d/rsum; pa<-d/csum
  th2 <- sum(rsum*csum) / n^2; kh <- (th1-th2)/(1-th2)
  th3 <- sum( (csum + rsum) * d ) / n^2;
  th4 <- 0; for (i in 1:nr) for (j in 1:nc)
    th4 <- th4 + (cmx[i,j] * ((csum[i] + rsum[j])^2));
  th4 <- th4 / n^3;
  th1c <- 1 - th1; th2c <- 1 - th2;
  khv <- 1/n *
    ( ( ( th1 * th1c ) / th2c^2 )
      + ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
      + ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
    )
  #per-class kappa, user’s accuracy...
  p <- cmx/n; uap <- apply(p,1,sum); pap <- apply(p,2,sum); dp<-diag(p);
  kpu <- (dp/uap - pap)/(1 - pap);
  #...and its variance
  t1 <- uap-dp; t2 <- (pap*uap)-dp; t3 <- dp*(1 - uap - pap + dp);
  kpuv <- ( (t1/(uap^3 * (1-pap)^3)) * ((t1*t2) + t3) )/n;
  #per-class kappa, producer’s reliability...
  kpp <- (dp/pap - uap)/(1 - uap);
  #...and its variance
  t1 <- (pap-dp);
  kppv <- ( (t1/(pap^3 * (1-uap)^3)) * ((t1*t2) + t3) )/n;

  #return all statistics
  if(by_class) {

    tibble::tibble(cluster = names(ua)
                   , sum_n = n
                   , sum_naive = th1
                   , sum_var = th1v
                   , sum_kappa = kh
                   , sum_kvar = khv
                   , user_naive = ua
                   , prod_naive = pa
                   , user_kappa = kpu
                   , user_kvar = kpuv
                   , prod_kappa = kpp
                   , prod_kvar = kppv
                   ) %>%
      dplyr::mutate(across(where(is.numeric),unname))

  } else {

    tibble::tibble(sum_n = n
                   , accuracy = th1
                   , accuracy_var = th1v
                   , kappa = kh
                   , kappa_var = khv
                   , user_naive = ua
                   , prod_naive = pa
                   , user_kappa = kpu
                   , user_kvar = kpuv
                   , prod_kappa = kpp
                   , prod_kvar = kppv
                   ) %>%
      dplyr::summarise(across(where(is.numeric),mean,na.rm = TRUE))


  }


}
