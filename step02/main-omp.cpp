#include <cmath>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

#include "get_time.h"

typedef double Real;

string realTypename(float x) { return "float"; }
string realTypename(double x) { return "double"; }

void calculate (const int n, Real *ma, Real *mb, Real *mc) {


#pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      Real sum = 0;
      for (int k = 0; k < n; ++k) {
	sum += ma[i*n+k]*mb[j*n+k];
      }
      mc[i*n+j] = sum;
    }
  }
}
void benchmark (const int n) {
  
  vector<Real> ma(n*n), mb(n*n), mc(n*n);
  vector<Real> mh(n*n);
  for (int addr = 0; addr < n*n; ++addr) {
    mh[addr] = Real(1);
  }
  ma = mh; mb = mh;

  double time_begin = get_time<double>();
  calculate
    (n, &ma[0],&mb[0],&mc[0]);
  double time_end = get_time<double>();
  
  mh = mc;
  bool correct = true;
  for (int addr = 0; addr < n*n; ++addr) {
    if (abs(mh[addr]-n) > 0.1) {
      correct = false; break;
    }
  }
  
  double flop = double(n)*n*n*2;
  double time_cost = time_end - time_begin;
  double flops = flop / time_cost;
  long long int score = correct ? flops : 0;
  cout << score << "\t| "
       << correct << " " << n << " " << realTypename(Real(0)) << " : "
       << flops/1e9 << " Gflops=  " << flop << " / " << time_cost << endl;
}

int main () {
  for (int n = 512; n <= 4096; n*=2 ) {
    benchmark(n-8);
    benchmark(n-4);
    benchmark(n);
    benchmark(n+4);
    benchmark(n+8);
  }
}


