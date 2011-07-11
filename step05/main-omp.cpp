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

typedef float Real;
const Real eps = 1e-20;
int zoom;

template<class T> T sq(const T &x) { return x*x; }

struct Fluid {
  int size;
  int width;
  int height;

  vector<Real> a00, a01, a02, a10, a11, a12, a20, a21, a22;
  vector<Real> solid;
  
  Fluid (int width0, int height0) :
    size(width0*height0), width(width0), height(height0),
    a00(size), a01(size), a02(size),
    a10(size), a11(size), a12(size),
    a20(size), a21(size), a22(size), solid(size) {

    initialize();
  }

  Real collide (Real &a, Real &b) {
    Real s = a*b/(a+b+eps);
    a-=s; b-=s; return 2*s;
  }
  void thermalize(Real &src, Real &a, Real &b) {
    src *= Real(1)/Real(3);
    a+=src; b+=src;
  }
  void thermalize(Real &src, Real &a, Real &b, Real &c, Real &d) {
    src *= Real(1)/Real(5);
    a+=src; b+=src; c+=src; d+=src;
  }
  void bounce (const Real solidity, Real &src, Real &dest) {
    Real amt = src * solidity; 
    dest += amt;
    src -= amt;
  }
  Real mix(const Real w, const Real n, const Real vx, const Real vy, const Real ux, const Real uy) {
    const Real inp = (ux*vx+uy*vy);
    return n*w*(Real(1) + Real(3) * inp + Real(4.5)*sq(inp) - Real(1.5)*(ux*ux+uy*uy));
  }
  void initialize () {
    for (int y = 0; y < height; ++y) {
      for (int x = 0; x < width; ++x) {
	const int p = y  * width + x;
	const Real r = height/8;
	const Real oy = height/2;
	const Real ox = oy;
	solid[p] = 64*sq(x-ox) + sq(y-oy) < sq(r) ? 1 : 0;
	Real w = 0.5*(Real(1) - solid[p]);
	a00[p] = a02[p] = a10[p] = a12[p] = a20[p] = a22[p] = 0;
        a01[p] = w*0.1;
	a11[p] = w*0.7;
	a21[p] = w*0.2;
      }
    }
  }
  
  void collision (const int t, Fluid& next) {
#pragma omp parallel for
    for (int y = 0; y < height; ++y) {
      for (int x = 0; x < width; ++x) {
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
        Real b00 = a00[p11];
        Real b10 = a10[p11];
        Real b20 = a20[p11];
        Real b01 = a01[p11];
        Real b11 = a11[p11];
        Real b21 = a21[p11];
        Real b02 = a02[p11];
        Real b12 = a12[p11];
        Real b22 = a22[p11];
        
	// boundary conditions
        {
          const Real w= 0.5;
          if (x==0 || x == width-1) {
            b00 = b02 = b10 = b12 = b20 = b22 = 0;
            b01 = w*0.1;
            b11 = w*Real(0.7); b21 = w*Real(0.2) + 1e-2 * sin(12*y/height);
          } 
        }

        Real n = b00+b01+b02+b10+b11+b12+b20+b21+b22+eps;
        Real mx=-b00-b01-b02            +b20+b21+b22;
        Real my=-b00    +b02-b10    +b12-b20    +b22;
        Real vx=mx/n;
        Real vy=my/n;

        b00 = mix(Real(1)/Real(36), n,-1,-1,vx,vy);
        b10 = mix(Real(1)/Real( 9), n, 0,-1,vx,vy);
        b20 = mix(Real(1)/Real(36), n, 1,-1,vx,vy);
        b01 = mix(Real(1)/Real( 9), n,-1, 0,vx,vy);
        b11 = mix(Real(4)/Real( 9), n, 0, 0,vx,vy);
        b21 = mix(Real(1)/Real( 9), n, 1, 0,vx,vy);
        b02 = mix(Real(1)/Real(36), n,-1, 1,vx,vy);
        b12 = mix(Real(1)/Real( 9), n, 0, 1,vx,vy);
        b22 = mix(Real(1)/Real(36), n, 1, 1,vx,vy);

	// solid boundary conditions
        Real solidifier = Real(1)-solid[p11];
        b00 *= solidifier;
        b10 *= solidifier;
        b20 *= solidifier;
        b01 *= solidifier;
        b11 *= solidifier;
        b21 *= solidifier;
        b02 *= solidifier;
        b12 *= solidifier;
        b22 *= solidifier;

	next.a00[p11] = b00;
	next.a10[p11] = b10;
	next.a20[p11] = b20;
	next.a01[p11] = b01;
	next.a11[p11] = b11;
	next.a21[p11] = b21;
	next.a02[p11] = b02;
	next.a12[p11] = b12;
	next.a22[p11] = b22;
      }
    }
  }

  void proceed (Fluid &next) {
#pragma omp parallel for
    for (int y = 0; y < height; ++y) {
      for (int x = 0; x < width; ++x) {
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
	next.a00[p11] = a00[p22] + solid[p22] * a22[p11];
	next.a10[p11] = a10[p12] + solid[p12] * a12[p11];
	next.a20[p11] = a20[p02] + solid[p02] * a02[p11];
	next.a01[p11] = a01[p21] + solid[p21] * a21[p11];
	next.a11[p11] = a11[p11] + solid[p11] * a11[p11];
	next.a21[p11] = a21[p01] + solid[p01] * a01[p11];
	next.a02[p11] = a02[p20] + solid[p20] * a20[p11];
	next.a12[p11] = a12[p10] + solid[p10] * a10[p11];
	next.a22[p11] = a22[p00] + solid[p00] * a00[p11];
      }
    }    
  }

  void write (string fn) {
    FILE *fp=fopen(fn.c_str(), "w"); 
    if (!fp) return;
    {
      fwrite(&width, sizeof(width), 1, fp);
      fwrite(&height, sizeof(height), 1, fp);
      int sizeOfReal = sizeof(Real);
      fwrite(&sizeOfReal, sizeof(sizeOfReal), 1, fp);
      const int bmpWidth = width/zoom;
      const int bmpHeight = height/zoom;
      const int bmpSize = bmpWidth * bmpHeight;
      
      vector<Real> densBlock(bmpSize),velxBlock(bmpSize),velyBlock(bmpSize);
      
      for (int y = 0; y < bmpHeight; ++y) {
        for (int x = 0; x < bmpWidth; ++x) {
          Real dens=0, momx=0, momy=0;
          for (int zy = 0; zy < zoom; ++zy) {
            for (int zx = 0; zx < zoom; ++zx) {
              int ix = x * zoom + zx;
              int iy = y * zoom + zy;
              int addr = iy * width + ix;
              dens 
                +=a00[addr]+a01[addr]+a02[addr]
                + a10[addr]+a11[addr]+a12[addr]
                + a20[addr]+a21[addr]+a22[addr];
              momx 
                +=-a00[addr]-a01[addr]-a02[addr]
                +  a20[addr]+a21[addr]+a22[addr];
              momy 
                +=-a00[addr]-a10[addr]-a20[addr]
                +  a02[addr]+a12[addr]+a22[addr];
            }
          }
          dens /= zoom*zoom;
          momx /= zoom*zoom;
          momy /= zoom*zoom;
          const Real velx = momx / (dens + eps);
          const Real vely = momy / (dens + eps);
          const int addr = y*bmpWidth+x;
          densBlock[addr] = dens;
          velxBlock[addr] = velx;
          velyBlock[addr] = vely;
        }
      }

      fwrite(&densBlock[0], sizeof(Real), bmpSize, fp);
      fwrite(&velxBlock[0], sizeof(Real), bmpSize, fp);
      fwrite(&velyBlock[0], sizeof(Real), bmpSize, fp);
    } fclose(fp);
  }
};

int main (int argc, char **argv) {
  if (argc <= 1) {
    zoom = 1;
  } else {
    istringstream iss(argv[1]);
    iss >> zoom;
  }

  string dirn;
  {
    ostringstream oss; oss << "img" << zoom;
    dirn = oss.str();
    system(("mkdir -p " + dirn).c_str());
  }

  Fluid flu(1024*zoom,256*zoom);
  Fluid flu2=flu;
  
  for (int t = 0; t < zoom*10001; ++t) {
    if (t % (zoom*100) == 0) {
      ostringstream ossFn;
      ossFn << dirn << "/" << (100000000+t) << ".bin";
      cerr << ossFn.str() << endl;
      flu.write(ossFn.str());
    }
      
    flu.collision(t,flu2);
    flu2.proceed(flu);
  }
  
  return 0;
}
