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
void calculate (const int logN, const int logNB, Real *ma, Real *mb, Real *mc) {
  const int n = 1<<logN;
  const int mask1  = (1<<logNB)-1;
  const int shift2 = logNB;
  const int mask2  = ((1<<logNB)-1)<<logNB;
  const int shift3 = logNB;
  const int mask3  = ((1<<(logN-logNB))-1)<<(2*logNB);
  const int shift4 = logN;
  const int mask4  = ((1<<(logN-logNB))-1)<<(logNB+logN);
  
  for (int addr = blockIdx.x * blockDim.x + threadIdx.x; addr < n*n;  
       addr += blockDim.x * gridDim.x) {
    int i = (addr&mask1) + ((addr&mask3)>>shift3);
    int j = ((addr&mask2)>>shift2) + ((addr&mask4)>>shift4);

    Real sum = 0;
#pragma unroll 1024
    for (int k = 0; k < n; ++k) {
      sum += ma[k*n+i]*ma[k*n+j];
    }
    mc[i*n+j] = sum;
  }
}

void benchmark (const int logN, const int logNB) {


  const int n  = 1<<logN;
  

  
  thrust::device_vector<Real> ma(n*n), mb(n*n), mc(n*n);
  thrust::host_vector<Real> mh(n*n);
  for (int addr = 0; addr < n*n; ++addr) {
    mh[addr] = Real(1);
  }
  ma = mh; mb = mh;

  cudaThreadSynchronize();double time_begin = get_time<double>();
  calculate<<<1024, 448*2>>>
    (logN, logNB,
     thrust::raw_pointer_cast(&*ma.begin()),
     thrust::raw_pointer_cast(&*mb.begin()),
     thrust::raw_pointer_cast(&*mc.begin())
     );
  cudaThreadSynchronize();double time_end = get_time<double>();
  
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
  // Set preference for above kernel to L1
  cudaFuncSetCacheConfig( calculate, cudaFuncCachePreferL1 );
  
  for (int logN = 4; logN <= 12; ++logN) {
    for (int logNB = 1; logNB < logN; ++logNB) {
      benchmark(logN, logNB);
    }
  }
}


/*
int main () {
  for (int logN = 4; logN <= 13; ++logN) {
    for (int logNB = 1; logNB < logN; ++logNB) {
      const int n = 1<<logN;
      const int mask1  = (1<<logNB)-1;
      const int shift2 = logNB;
      const int mask2  = ((1<<logNB)-1)<<logNB;
      const int shift3 = logNB;
      const int mask3  = ((1<<(logN-logNB))-1)<<(2*logNB);
      const int shift4 = logN;
      const int mask4  = ((1<<(logN-logNB))-1)<<(logNB+logN);

      cerr << mask1 << " " << mask2 << " " << mask3 << " " << mask4 << endl;
      
      for (int addr = 0; addr < n*n; ++addr) {
	int i = (addr&mask1) + ((addr&mask3)>>shift3);
	int j = ((addr&mask2)>>shift2) + ((addr&mask4)>>shift4);

	cout << i << " " << j << endl;
      }
      return 0;
    }
  }
}

*/
