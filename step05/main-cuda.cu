#include <iostream>
#include <sstream>
#include <string>
#include <stdlib.h>
#include <vector>
using namespace std;

typedef float Real;

#include "fluid.h"
#include "get_time.h"

int zoom;
Real flowSpeed;

__global__ void initialize (Real flowSpeed, FluidPtr pFlu) {
  pFlu.initialize(flowSpeed);
}

__global__ void collision (FluidPtr pFlu, FluidPtr pFlu2) {
  pFlu.collision(pFlu2);
}

__global__ void proceed (FluidPtr pFlu, FluidPtr pFlu2) {
  pFlu2.proceed(pFlu);
}



int main (int argc, char **argv) {
  if (argc < 3) {
    cerr << "usage : " << argv[0] << " zoom flowSpeed" << endl;
    return -1;
  } else {
    istringstream iss(argv[1]);
    iss >> zoom;
    istringstream iss2(argv[2]);
    iss2 >> flowSpeed;
  }

  cerr << argv[0] << endl;
  
  cudaFuncSetCacheConfig(collision, cudaFuncCachePreferL1);
  cudaFuncSetCacheConfig(proceed  , cudaFuncCachePreferL1);

  string dirn;
  {
    ostringstream oss; oss << "bin/" << zoom << "_" << flowSpeed;
    dirn = oss.str();
    system(("mkdir -p " + dirn).c_str());
  }

  FluidMemory<thrust::device_vector<Real> > flu(1024*zoom,768*zoom);
  FluidMemory<thrust::host_vector<Real> > flu_host(1024*zoom,768*zoom);
  FluidMemory<thrust::device_vector<Real> >  flu2=flu;

  FluidPtr pFlu = flu.ptr();
  FluidPtr pFlu2 = flu2.ptr();

  const int blockDim = 1024, gridDim = 448;

  initialize<<<blockDim,gridDim>>>(flowSpeed, pFlu);
  initialize<<<blockDim,gridDim>>>(flowSpeed, pFlu2);

  double time_integrated = 0;

  const int max_t = 50001; //100001;
  for (int t = 0; t < zoom*max_t; ++t) {
    if (t % (zoom*50000) == 0) {
      ostringstream ossFn;
      ossFn << dirn << "/" << (100000000+t) << ".bin";
      cerr << ossFn.str() << " : time spent so far " << time_integrated << endl;
      flu_host.copyFrom(flu);
      flu_host.write(ossFn.str(), zoom);
    }

    double time_begin = get_time<double>();
    cudaThreadSynchronize();
    collision<<<blockDim,gridDim>>>(pFlu, pFlu2);
    cudaThreadSynchronize();
    proceed<<<blockDim,gridDim>>>(pFlu, pFlu2);
    cudaThreadSynchronize();
    double time_end = get_time<double>();
    time_integrated += time_end - time_begin;
  }
  
  return 0;
}
