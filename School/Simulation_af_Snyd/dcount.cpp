#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector sumDice(NumericVector roll, NumericVector diceVector) {
  int n = diceVector.length();
  
  NumericVector result(6 * n);
  NumericVector sumresult(6);
  
  int k = 0;
  int m = 0;
  
  for(int i = 0; i < n; i++){
    int z = i * 6;
    for(int j = k; j < k + diceVector[i]; j++){
      result(roll[j]-1 + z) += 1;
      m = j;
    }
    
    k = m + 1;
  }
  
  for(int i = 0; i < n; i++){
    int trappetal = 1;
    int z = i * 6;
    m = diceVector[i];
    
    for(int j = z; j < z + m; j++){
      trappetal = result[j]*trappetal;
    }
    
    if(trappetal == 1){
      for(int j = z; j < z + 6; j++){
        result[j] = m + 1;
      }
    } else {
      int ones = result[z];
      for(int j = z + 1; j < z + 6; j++){
        result[j] += ones;
      }
    }
  }
  
  for(int i = 0; i < 6; i++){
    for(int j = 0; j < n; j++){
      sumresult[i] += result[i + j * 6];
    }
  }
  return(sumresult);
  
}
