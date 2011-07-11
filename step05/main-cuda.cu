#include <iostream>
#include <sstream>
#include <string>
#include <stdlib.h>
#include <vector>
using namespace std;

typedef float Real;

#include "fluid.h"

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

  string dirn;
  {
    ostringstream oss; oss << "bin/" << zoom << "_" << flowSpeed;
    dirn = oss.str();
    system(("mkdir -p " + dirn).c_str());
  }

  FluidMemory<thrust::device_vector<Real> > flu(1024*zoom,768*zoom);
  FluidMemory<thrust::device_vector<Real> >  flu2=flu;

  FluidPtr pFlu = flu.ptr();
  FluidPtr pFlu2 = flu2.ptr();

  initialize<<<1024,448>>>(flowSpeed, pFlu);
  initialize<<<1024,448>>>(flowSpeed, pFlu2);
  
  for (int t = 0; t < zoom*100001; ++t) {
    if (t % (zoom*100) == 0) {
      ostringstream ossFn;
      ossFn << dirn << "/" << (100000000+t) << ".bin";
      cerr << ossFn.str() << endl;
      flu.write(ossFn.str(), zoom);
    }

    collision<<<1024,448>>>(pFlu, pFlu2);
    proceed<<<1024,448>>>(pFlu, pFlu2);
  }
  
  return 0;
}
