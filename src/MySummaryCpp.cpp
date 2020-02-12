#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
Rcpp::List MySummaryCpp(NumericVector vecIn) {
  
  int n = vecIn.size();
  
  double sum = 0;
  
  for(int i = 0; i < n; i++){
    sum = sum + vecIn.at(i);
  }
  
  double myMean = sum / n;
  
  double myVar = 0;
  
  for(int i = 0; i < n; i++){
    myVar = myVar + pow(vecIn.at(i) - myMean, 2);
  }
  
  double var = (1/ (double(n) - 1)) * myVar;
  
  return(Rcpp::List::create(Named("mean") = myMean,
                            Named("std") = std::sqrt(var)));
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
MySummaryCpp(42)
*/
