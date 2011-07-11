#include <algorithm>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>
#include <stdlib.h>
#include <vector>
using namespace std;
#include "ppm.h"

using namespace noost::visualization::color;
using namespace noost::visualization::ppm;


template<class T> T sq(const T &x) { return x*x; }

template<class Real>
void visualize (FILE* ifp, string ofn, int width, int height, string visualizeType) {
  ppm<Real> bmp(width, height, rgb<Real>(0,0,0));
  bmp.set_max_color(1);
  const int bmpSize = width*height;
  vector<Real> densBlock(bmpSize), velxBlock(bmpSize), velyBlock(bmpSize);
  fread(&densBlock[0], sizeof(Real), bmpSize, ifp);
  fread(&velxBlock[0], sizeof(Real), bmpSize, ifp);
  fread(&velyBlock[0], sizeof(Real), bmpSize, ifp);
  for (int y = 0; y < bmp.height(); ++y) {
    for (int x = 0; x < bmp.width(); ++x) {
      const int addr = y*width+x;
      const Real dens = densBlock[addr];
      const Real velx = velxBlock[addr];
      const Real vely = velyBlock[addr];
      Real 
        r = (30*vely+0.5),
        g = dens,
        b = (30*velx+0.5);
      bmp(x,y) = rgb<Real>(r,g,b);
    }
  }
  bmp.rescale_max_color(255);
  bmp.write(ofn);
}

int main (int argc, char **argv) {
  if (argc < 4) {
    cerr << "usage : " << argv[0] << " type inputfn outputfn" << endl;
    return -1;
  }
  const string visualizeType = argv[1];
  const string ifn = argv[2];
  const string ofn = argv[3];
  
  FILE *fp = fopen(ifn.c_str(),"r"); if (!fp) {
    cerr << ifn << " : cannot open." << endl;
    return -1; 
  }
  {
    int width, height, sizeOfReal;
    fread(&width, sizeof(width), 1, fp);
    fread(&height, sizeof(height), 1, fp);
    fread(&sizeOfReal, sizeof(sizeOfReal), 1, fp);
    if (sizeOfReal==4) {
      visualize<float>(fp, ofn, width, height, visualizeType);
    } else if (sizeOfReal==8) {
      visualize<double>(fp, ofn, width, height, visualizeType);
    } else {
      cerr << ifn << " : unsupported format." << endl;
    }
  } fclose(fp);
  return 0;
}
