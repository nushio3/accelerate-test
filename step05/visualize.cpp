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
  
  vector<unsigned char> seed(bmpSize);
  vector<Real> noise(bmpSize);
  vector<Real> noiseNum(bmpSize);
  vector<Real> noiseDen(bmpSize,eps);
  {
    FILE* fpRand=fopen("/dev/urandom","r");
    fread(&seed[0],sizeof(seed[0]), seed.size(), fpRand);
    fclose(fpRand);
  }
  for (int i = 0; i < bmpSize;++i) {
    noise[i] = seed[i]/Real(255)*2-1;
  }

  for (int i = 0; i < bmpSize;++i) dens[i] += eps; // avoid div0

  // the preparation path
#pragma omp parallel for
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
      if (visualizeType == "rnd" || visualizeType == "vor"){
        Real vx = momx[p11]/dens[p11];
        Real vy = momy[p11]/dens[p11];
        Real amp = sqrt(sq(vx)+sq(vy))+eps;
        Real ex = vx/amp;
        Real ey = vy/amp;
        amp*=80;
        Real myVal = noise[p11];
        int curAddr = -1;
        for (int parity = -1; parity<=1; parity+=2) {
          for (Real t = 0; t <= amp; t+=0.5) {
            int nx = (int(x1 + 0.5 + parity*t*ex) + width)%width;
            int ny = (int(y1 + 0.5 + parity*t*ey) + height)%height;
            int addr = ny*width+nx;
            if (addr==curAddr) continue;
            curAddr = addr;
            noiseNum[addr] += myVal;
            noiseDen[addr] += 1;
          }
        }
      }
    }
  }

  // the first path
  bool failure = false;
#pragma omp parallel for
  for (int y = 0; y < bmp.height(); ++y) {
    if (failure) continue;
    for (int x = 0; x < bmp.width(); ++x) {
      if (failure) continue;
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
        Real vy21 = momy[p21]/dens[p21];
        Real vy01 = momy[p01]/dens[p01];
        Real vx12 = momx[p12]/dens[p12];
        Real vx10 = momx[p10]/dens[p10];
        Real vor = vy21-vy01-vx12+vx10;

        vor *= 300;
        
        r=-vor;
        g=abs(vor/10);
        b=vor;

        Real bunsan = 0.5*sqrt(max(Real(0),noiseDen[p11]-Real(0.8)));
        Real gray=0.5 + noiseNum[p11]/noiseDen[p11]*bunsan;
        gray = max(Real(0), min(Real(1), gray));
        gray *= 0.5;
        r+=gray; g+=gray;b+=gray;

      } else if (visualizeType == "rnd"){
        Real bunsan = 0.5*sqrt(max(Real(0),noiseDen[p11]-Real(0.8)));
        r=g=b=noiseNum[p11]/noiseDen[p11]*bunsan+0.5;
      } else {
        cerr << "unsupported visualization type : " << visualizeType << endl;
        failure = true; continue;
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
    cerr << width << " " << height << endl;
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
