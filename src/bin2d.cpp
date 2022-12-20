#include <stdlib.h>
#include <stdexcept>
#include <float.h>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

int getBin(const double needle, const NumericVector& breaks) {
  NumericVector::const_iterator it = std::lower_bound(breaks.begin(), breaks.end(), needle);
  
  if (it == breaks.end()) {
    printf("Bin not found!\n");
    return -1; // Hasn't found a suitable bin
  }
  
  // it - breaks.begin() gives the index of the right break of the corresponding
  // bin. If we want the bin index instead, we need to subtract 1.
  return it - breaks.begin() - 1;
}

// Not used right now, but using this would be marginally faster than working
// with an IntegerMatrix
int** allocate2dArray(const uint_fast32_t m, const uint_fast32_t n) {
  int* p = (int*) calloc(m*n, sizeof(int));
  int** pp = (int**) std::malloc(m * sizeof(int*));
  
  if (p == nullptr || pp == nullptr)
    throw std::runtime_error("Failed to allocate space for 2D array");
    
  for (uint_fast32_t i = 0; i < m; i++) {
    pp[i] = p + i*n;
  }
  
  return pp;
}

// binCounts

// [[Rcpp::export]]
IntegerMatrix binCounts(
    const NumericVector& x, 
    const NumericVector& y, 
    const NumericVector& xBreaks, 
    const NumericVector& yBreaks) {
  // The breaks indicate the borders of the bins/cells. If there are n breaks,
  // there are n-1 cells.
  // TODO: Change to calloc
  IntegerMatrix counts(xBreaks.length()-1, yBreaks.length()-1);
  for (uint_fast32_t i = 0; i < counts.length(); i++)
    counts[i] = 0;
  
  if (x.length() != y.length())
    stop("Vector x does not have the same length as vector y.");
  
  uint_fast32_t xBin, yBin;
  for (uint_fast32_t i = 0; i < x.length(); i++) {
    if (NumericVector::is_na(x[i]) || NumericVector::is_na(y[i]))
      continue;
    
    xBin = getBin(x[i], xBreaks);
    yBin = getBin(y[i], yBreaks);
    counts(xBin, yBin) += 1;
  }
  
  return counts;
}

double minNa(const NumericVector& vec) {
  double minimum = DBL_MAX;
  for (double x : vec) {
    if (NumericVector::is_na(x))
      continue;
    
    if (x < minimum)
      minimum = x;
  }
  
  return minimum;
}

double maxNa(const NumericVector& vec) {
  double maximum = DBL_MIN;
  for (double x : vec) {
    if (NumericVector::is_na(x))
      continue;
    
    if (x > maximum)
      maximum = x;
  }
  
  return maximum;
}

NumericVector calcBreaks(const NumericVector& dat, const uint_fast32_t& nBins) {
  double minimum = minNa(dat);
  double maximum = maxNa(dat);
  
  double diff = maximum - minimum;
  
  // Add a bit of padding (1%) so minimum and maximum get included in the range
  maximum = maximum + 0.01 * diff;
  minimum = minimum - 0.01 * diff;
  
  // Recalculate difference to take the padding into account
  diff = maximum - minimum;
  
  NumericVector breaks(nBins + 1);
  for (uint_fast32_t i = 0; i < nBins + 1; i++) {
    breaks(i) = minimum + diff / nBins * i;
  }
  
  return breaks;
}

NumericVector calcCenters(const NumericVector& breaks) {
  NumericVector centers(breaks.length() - 1);
  for (uint_fast32_t i = 0; i < breaks.length() - 1; i++) {
    centers[i] = (breaks[i] + breaks[i+1]) / 2;
  }
  
  return centers;
}

// bin2d (number of bins)

// [[Rcpp::export]]
List bin2d(NumericVector x, NumericVector y, int xBins, int yBins = -1) {
  // xBins and yBins indicate the number of desired bins. Set yBins to xBins
  // if it has not been supplied.
  if (yBins == -1)
    yBins = xBins;
  
  NumericVector xBreaks = calcBreaks(x, xBins);
  NumericVector yBreaks = calcBreaks(y, yBins);
  
  IntegerMatrix counts = binCounts(x, y, xBreaks, yBreaks);
  
  // Calculate centers of bins to provide row and column names
  NumericVector xCenters = calcCenters(xBreaks);
  NumericVector yCenters = calcCenters(yBreaks);
  
  colnames(counts) = yCenters;
  rownames(counts) = xCenters;
  
  DataFrame df(counts);
  
  // Combine with the breaks, for reference
  return List::create(_["counts"] = df, 
                      _["xBreaks"] = xBreaks, 
                      _["yBreaks"] = yBreaks);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/** R 
library(reshape)
library(tidyverse)

xBreaks <- seq(0, 1, 0.001)
yBreaks <- seq(0, 1, 0.1)
set.seed(42)
x <- runif(1000000)
y <- runif(1000000)

# binCounts(x, y, xBreaks, yBreaks)
ls <- bin2d(x, y, 10, 100)
print(ls)

df <- ls[[1]]
df <- rownames_to_column(df, "x")
df <- melt(df, id.vars = "x", variable_name = "y")
df
*/
