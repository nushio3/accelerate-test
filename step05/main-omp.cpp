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

void collision (FluidPtr pFlu, FluidPtr pFlu2) {
  pFlu.collision(pFlu2);
  pFlu2.proceed(pFlu);
}

void proceed (FluidPtr pFlu, FluidPtr pFlu2) {
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

  FluidMemory<vector<Real> > flu(1024*zoom,768*zoom);
  FluidMemory<vector<Real> >  flu2=flu;

  FluidPtr pFlu = flu.ptr();
  FluidPtr pFlu2 = flu2.ptr();

  pFlu.initialize(flowSpeed);
  pFlu2.initialize(flowSpeed);
  
  double time_integrated = 0;

  for (int t = 0; t < zoom*100001; ++t) {
    if (t % (zoom*100) == 0) {
      ostringstream ossFn;
      ossFn << dirn << "/" << (100000000+t) << ".bin";
      cerr << ossFn.str() << " : time spent so far " << time_integrated << endl;
      flu.write(ossFn.str(), zoom);
    }
      
    double time_begin = get_time<double>();
    collision(pFlu, pFlu2);
    proceed(pFlu, pFlu2);
    double time_end = get_time<double>();
    time_integrated += time_end - time_begin;
  }
  
  return 0;
}
