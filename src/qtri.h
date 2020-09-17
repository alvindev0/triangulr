#ifndef TRIANGULR_QTRI_H
#define TRIANGULR_QTRI_H

Rcpp::NumericVector QTriC(
    Rcpp::NumericVector p, double min, double max, double mode, bool lower_tail,
    bool log_p
);

Rcpp::NumericVector QTriC2(
    Rcpp::NumericVector p, Rcpp::NumericVector min, Rcpp::NumericVector max,
    Rcpp::NumericVector mode, bool lower_tail, bool log_p
);

#endif
