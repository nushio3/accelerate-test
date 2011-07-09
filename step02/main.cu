#include <iostream>
#include <string>
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
using namespace std;

#include "get_time.h"

typedef float Real;

string realTypename(float x) { return "float"; }
string realTypename(double x) { return "double"; }

__global__
void calculate (const int n, Real *ma, Real *mb, Real *mc) {
  for (int addr = blockIdx.x * blockDim.x + threadIdx.x; addr < n*n;  
       addr += blockDim.x * gridDim.x) {
    int i = addr%n;
    int j = addr/n;

    Real sum = 0;
#pragma unroll 1024
    for (int k = 0; k < n; ++k) {
      sum += ma[k*n+i]*ma[k*n+j];
    }
    mc[i*n+j] = sum;
  }
}

void benchmark (const int n) {

  double time_begin = get_time<double>();
  
  thrust::device_vector<Real> ma(n*n), mb(n*n), mc(n*n);
  thrust::host_vector<Real> mh(n*n);
  for (int addr = 0; addr < n*n; ++addr) {
    mh[addr] = Real(1);
  }
  ma = mh; mb = mh;
  calculate<<<1024, 448*2>>>
    (n,
     thrust::raw_pointer_cast(&*ma.begin()),
     thrust::raw_pointer_cast(&*mb.begin()),
     thrust::raw_pointer_cast(&*mc.begin())
     );
  mh = mc;
  bool correct = true;
  for (int addr = 0; addr < n*n; ++addr) {
    if (abs(mh[addr]-n) > 0.1) {
      correct = false; break;
    }
  }

  double time_end = get_time<double>();
  
  double flop = double(n)*n*n*2;
  double time_cost = time_end - time_begin;
  double flops = flop / time_cost;
  long long int score = correct ? flops : 0;
  cout << score << "\t| "
       << correct << " " << n << " " << realTypename(Real(0)) << " : "
       << flops/1e9 << " Gflops=  " << flop << " / " << time_cost << endl;
}

int main () {
  // Set preference for above kernel to L1
  cudaFuncSetCacheConfig( calculate, cudaFuncCachePreferL1 );
  
  for (int n = 4; n <= 1<<12; n*=2)
    benchmark(n);
}

