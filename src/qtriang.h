#ifndef TRIANGULR_QTRIANG_H
#define TRIANGULR_QTRIANG_H

Rcpp::NumericVector QTriang(
    Rcpp::NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p
);

Rcpp::NumericVector QTriang(
    Rcpp::NumericVector p, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode, bool lower_tail, bool log_p
);

#endif
