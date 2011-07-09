#include <cmath>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

#include "get_time.h"

typedef double Real;

string realTypename(float x) { return "float"; }
string realTypename(double x) { return "double"; }

void calculate (const int logN, const int logNB, Real *ma, Real *mb, Real *mc) {
  const int n = 1<<logN;

  const int mask1  = (1<<logNB)-1;
  const int shift2 = logNB;
  const int mask2  = ((1<<logNB)-1)<<logNB;
  const int shift3 = logNB;
  const int mask3  = ((1<<(logN-logNB))-1)<<(2*logNB);
  const int shift4 = logN;
  const int mask4  = ((1<<(logN-logNB))-1)<<(logNB+logN);


  
#pragma omp parallel for
  for (int addr = 0; addr < n*n; ++addr) {

    int i = (addr&mask1) + ((addr&mask3)>>shift3);
    int j = ((addr&mask2)>>shift2) + ((addr&mask4)>>shift4);
  
    //int i = addr%n;
    //int j = addr/n;
  

  //for (int i = 0; i < n; ++i) {
  //for (int j = 0; j < n; ++j) {
    Real sum = 0;
    for (int k = 0; k < n; ++k) {
      sum += ma[k+i*n]*ma[k+j*n];
    }
    mc[i*n+j] = sum;
  }
// }
}
void benchmark (const int logN, const int logNB) {
  
  const int n  = 1<<logN;
  
    
  vector<Real> ma(n*n), mb(n*n), mc(n*n);
  vector<Real> mh(n*n);
  for (int addr = 0; addr < n*n; ++addr) {
    mh[addr] = Real(1);
  }
  ma = mh; mb = mh;

  double time_begin = get_time<double>();
  calculate
    (logN, logNB, &ma[0],&mb[0],&mc[0]);
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
       << correct << " " << logN << " " << logNB << " " << realTypename(Real(0)) << " : "
       << flops/1e9 << " Gflops=  " << flop << " / " << time_cost << endl;
}

int main () {
  for (int logN = 4; logN <= 12; ++logN) {
    for (int logNB = 1; logNB < logN; ++logNB) {
      benchmark(logN, logNB);
    }
  }
}


