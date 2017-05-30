#include <Rcpp.h>
using namespace Rcpp;
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
std::list<SEXP> flattenList(List x){
  std::list<SEXP> o(0);
  std::size_t input_size = x.size();
  
  for(std::size_t i = 0; i < input_size; i++)
    {
    if (TYPEOF(x[i]) == VECSXP)
      {
      o.splice(o.end(),flattenList(x[i]));
    } else 
      {
      o.push_back(x[i]);
    }
  }

  return o;
}
