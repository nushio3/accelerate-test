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
  const Real eps = 1e-20;
  ppm<Real> bmp(width, height, rgb<Real>(0,0,0));
  bmp.set_max_color(1);
  const int bmpSize = width*height;
  vector<Real> dens(bmpSize), momx(bmpSize), momy(bmpSize), enrg(bmpSize);
  fread(&dens[0], sizeof(Real), bmpSize, ifp);
  fread(&momx[0], sizeof(Real), bmpSize, ifp);
  fread(&momy[0], sizeof(Real), bmpSize, ifp);
  fread(&enrg[0], sizeof(Real), bmpSize, ifp);
  
  for (int i = 0; i < bmpSize;++i) dens[i] += eps; // avoid div0
  
  for (int y = 0; y < bmp.height(); ++y) {
    for (int x = 0; x < bmp.width(); ++x) {
      const int x1 = (x+1)%width;
      const int x2 = (x+2)%width;
      const int y1 = (y+1)%height;
      const int y2 = (y+2)%height;
      const int p00 = y  * width + x;
      const int p10 = y  * width + x1;
      const int p20 = y  * width + x2;
      const int p01 = y1 * width + x;
      const int p11 = y1 * width + x1;
      const int p21 = y1 * width + x2;
      const int p02 = y2 * width + x;
      const int p12 = y2 * width + x1;
      const int p22 = y2 * width + x2;


      Real r,g,b;
      if (visualizeType == "vel") {
        r = (30*momy[p11]+0.5); g = dens[p11]; b = (30*momx[p11]+0.5);
      } else if (visualizeType == "nrg") {
        Real vx = momx[p11]/dens[p11];
        Real vy = momy[p11]/dens[p11];
        Real knrg = 0.5*dens[p11]*(sq(vx)+sq(vy));
        r=0;g=enrg[p11]-0.15;b=0; g*=30;
      } else if (visualizeType == "vor") {
        Real vx21 = momy[p21]/dens[p21];
        Real vx01 = momy[p01]/dens[p01];
        Real vy12 = momy[p12]/dens[p12];
        Real vy10 = momy[p10]/dens[p10];
        Real vor = vy12-vy10+vx01-vx21;

        vor *= 1000;
        
        r=-vor;
        g=abs(vor/3);
        b=vor;
      } else {
        cerr << "unsupported visualization type : " << visualizeType << endl;
        return;
      }
      bmp(x1,y1) = rgb<Real>(r,g,b);
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
    cerr << "cannot open : " << ifn << endl;
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
      cerr << "unsupported floating point format : " << ifn << endl;
    }
  } fclose(fp);
  return 0;
}
