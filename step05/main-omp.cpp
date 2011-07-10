#include <iostream>
#include <sstream>
#include <string>
#include <vector>
using namespace std;
#include "ppm.h"

using namespace noost::visualization::color;
using namespace noost::visualization::ppm;

typedef float Real;
const Real eps = 1e-20;
const int zoom = 4;

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

  void initialize () {
    for (int y = 0; y < height; ++y) {
      for (int x = 0; x < width; ++x) {
	const int p = y  * width + x;
	const Real r = height/8;
	const Real oy = height/2;
	const Real ox = oy;
	solid[p] = 64*sq(x-ox) + sq(y-oy) < sq(r) ? 1 : 0;
	const Real w = Real(1) - solid[p];
	a00[p] = a01[p] = a02[p] = a10[p] = a20[p] = a21[p] = a22[p] = Real(0);
	a11[p] = w*Real(0.2); 
	a21[p] = w*(Real(0.8) + 1e-4 * (sin(x) + cos(y)));
      }
    }
  }
  
  void collision (Fluid& next) {
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
	if (x==0) {
	  b00 = b01 = b02 = b10 = b20 = b21 = b22 = Real(0);
	  b11 = Real(0.8); b12 = Real(0.2);
	}

	b11 += collide(b00,b22);
	b11 += collide(b02,b20);
	b01 += collide(b00,b02);  
	b21 += collide(b20,b22);  
	b10 += collide(b00,b20);  
	b12 += collide(b02,b22);
	b11 += collide(b10,b12);
	b11 += collide(b01,b21);
	thermalize(b11, b10, b12, b01, b21);
	thermalize(b10, b00, b20);
	thermalize(b12, b02, b22);
	thermalize(b01, b00, b02);
	thermalize(b21, b20, b22);
	bounce(solid[p00], b00, b11);
	bounce(solid[p01], b01, b11);
	bounce(solid[p02], b02, b11);
	bounce(solid[p10], b10, b11);
	bounce(solid[p12], b12, b11);
	bounce(solid[p20], b20, b11);
	bounce(solid[p21], b21, b11);
	bounce(solid[p22], b22, b11);

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
	next.a00[p00] = a00[p11];
	next.a10[p10] = a10[p11];
	next.a20[p20] = a20[p11];
	next.a01[p01] = a01[p11];
	next.a11[p11] = a11[p11];
	next.a21[p21] = a21[p11];
	next.a02[p02] = a02[p11];
	next.a12[p12] = a12[p11];
	next.a22[p22] = a22[p11];
      }
    }    
  }

  void write (string fn) {
    ppm<Real> bmp(width/zoom, height/zoom, rgb<Real>(0,0,0));
    bmp.set_max_color(1);
    for (int y = 0; y < bmp.height(); ++y) {
      for (int x = 0; x < bmp.width(); ++x) {
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
              -=a00[addr]+a01[addr]+a02[addr]
              + a20[addr]+a21[addr]+a22[addr];
            momy 
              -=a00[addr]+a10[addr]+a20[addr]
              + a02[addr]+a12[addr]+a22[addr];
          }
        }
	const Real velx = momx / (dens + eps);
	const Real vely = momy / (dens + eps);
	Real 
	  r = sq(velx),
	  g = dens,
	  b = sq(vely);
	bmp(x,y) = rgb<Real>(r,g,b);
      }
    }
    bmp.rescale_max_color(255);
    bmp.write(fn);
  }
};

int main () {
  Fluid flu(512*zoom,256*zoom);
  Fluid flu2=flu;
  
  for (int t = 0; t < 10001; ++t) {
    if (t % 100 == 0) {
      ostringstream ossFn;
      ossFn << "img/" << (100000000+t) << ".ppm";
      flu.write(ossFn.str());
    }
      
    flu.collision(flu2);
    flu2.proceed(flu);
  }
  
  return 0;
}
