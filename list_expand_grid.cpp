#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
SEXP List_expand_grid(List a) {
  const int list_size = a.size();
  NumericVector element_length(list_size);
  NumericVector element_length_multiple(list_size);
  int res_nrow =1L;
  StringVector col_names(list_size);
  List L(list_size); 
  //##########
  for (int i = 0; i < list_size; ++i) { //loop each input vector
    SEXP ll = a[i]; 
    GenericVector temp_vec(ll);  
    int  m=temp_vec.size();   
    element_length[i] = m; //length of each input vector
    int multi = 1;
    res_nrow = res_nrow *= element_length[i];
    char name[6];
    sprintf(&(name[0]), "X.%d", i);
    col_names(i) = name; // column names
    for(int ii = 0; ii < i; ++ii){
      element_length_multiple[i] = multi *= element_length[ii]; //multiple of each input vector
    };
  };
  
  for (int l = 0; l < list_size; ++l) { //loop each input vector
    SEXP ll = a[l]; 
    GenericVector temp_vec(ll); 
    GenericVector temp_for_list(res_nrow);
    size_t ii = 1L, jj = 0L;
    for(size_t i = 0; i < res_nrow; 
    ++i, ii = ii / element_length_multiple[l] < 1 ? ii + 1L : 1L, 
                                                jj = ii > 1L ? jj : jj + 1L) 
      { if(jj < element_length[l]){
        temp_for_list[i] = temp_vec[jj];}
      else {jj = 0L; ii =1L;
      temp_for_list[i] = temp_vec[jj];
      }
      };
    L[l] = temp_for_list;
  };
  
  StringVector row_names(res_nrow);
  for (int i = 0; i < res_nrow; ++i) {
    char name[5];
    sprintf(&(name[0]), "%d", i);
    row_names(i) = name;
  };
  L.attr("row.names") = row_names;
  
  
  
  L.attr("names") = col_names;
  L.attr("class") = "data.frame";
  
  return L;
}
