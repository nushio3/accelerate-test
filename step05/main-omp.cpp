#include <algorithm>
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
const int zoom = 2;

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
  void setLimit (Real &limitter, const Real src, const Real dest) {
    if(dest<src)limitter = min(limitter, src/(src-dest));
  }
  void limit(const Real limitter, const Real src, Real &dest) {
    dest = src + limitter*(dest-src);
  }

  void initialize () {
    for (int y = 0; y < height; ++y) {
      for (int x = 0; x < width; ++x) {
	const int p = y  * width + x;
	const int r = height/8;
	const int oy = height/2;
	const int ox = oy;
	solid[p] = 0; // 64*sq(x-ox) + sq(y-oy) < sq(r) ? 1 : 0;
	Real w = 0.5*(Real(1) - solid[p]);
        if (y > height/2+r*sin(Real(12*x)/width)) {w/=2;}
	a00[p] = a02[p] = a10[p] = a12[p] = a20[p] = a22[p] = w*Real(0.05);
        a01[p] = w*0.1;
	a11[p] = w*0.7;
	a21[p] = w*Real(0.2);
        if (y > height/2+r*sin(Real(12*x)/width)) {swap(a01[p], a21[p]);}
      }
    }
  }
  
  void collision (const int t, Fluid& next) {
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
	if (x==0 && false) {
	  b00 = b02 = b10 = b12 = b20 = b22 = Real(0.01);
          b01 = 0.1;
	  b11 = Real(0.7); b21 = Real(0.2) + 1e-4 * cos(y);
	}
        Real c00 = 0;
        Real c10 = 0;
        Real c20 = 0;
        Real c01 = 0;
        Real c11 = 0;
        Real c21 = 0;
        Real c02 = 0;
        Real c12 = 0;
        Real c22 = 0;
        c00 += Real(0.5555555555555556) * b00;
        c00 += Real(0.22222222222222224) * b10;
        c00 += Real(0.2222222222222222) * b20;
        c00 += Real(0.22222222222222224) * b01;
        c00 += Real(-0.1111111111111111) * b11;
        c00 += Real(-0.11111111111111116) * b21;
        c00 += Real(0.22222222222222218) * b02;
        c00 += Real(-0.11111111111111116) * b12;
        c00 += Real(-0.11111111111111122) * b22;
        c10 += Real(0.22222222222222224) * b00;
        c10 += Real(0.3055555555555556) * b10;
        c10 += Real(0.22222222222222224) * b20;
        c10 += Real(0.1388888888888889) * b01;
        c10 += Real(0.2222222222222222) * b11;
        c10 += Real(0.1388888888888889) * b21;
        c10 += Real(-0.11111111111111116) * b02;
        c10 += Real(-2.7777777777777832e-2) * b12;
        c10 += Real(-0.11111111111111116) * b22;
        c20 += Real(0.2222222222222222) * b00;
        c20 += Real(0.22222222222222224) * b10;
        c20 += Real(0.5555555555555556) * b20;
        c20 += Real(-0.11111111111111116) * b01;
        c20 += Real(-0.1111111111111111) * b11;
        c20 += Real(0.22222222222222224) * b21;
        c20 += Real(-0.11111111111111122) * b02;
        c20 += Real(-0.11111111111111116) * b12;
        c20 += Real(0.22222222222222218) * b22;
        c01 += Real(0.22222222222222224) * b00;
        c01 += Real(0.1388888888888889) * b10;
        c01 += Real(-0.11111111111111116) * b20;
        c01 += Real(0.3055555555555556) * b01;
        c01 += Real(0.2222222222222222) * b11;
        c01 += Real(-2.7777777777777832e-2) * b21;
        c01 += Real(0.22222222222222224) * b02;
        c01 += Real(0.1388888888888889) * b12;
        c01 += Real(-0.11111111111111116) * b22;
        c11 += Real(-0.1111111111111111) * b00;
        c11 += Real(0.2222222222222222) * b10;
        c11 += Real(-0.1111111111111111) * b20;
        c11 += Real(0.2222222222222222) * b01;
        c11 += Real(0.5555555555555556) * b11;
        c11 += Real(0.2222222222222222) * b21;
        c11 += Real(-0.1111111111111111) * b02;
        c11 += Real(0.2222222222222222) * b12;
        c11 += Real(-0.1111111111111111) * b22;
        c21 += Real(-0.11111111111111116) * b00;
        c21 += Real(0.1388888888888889) * b10;
        c21 += Real(0.22222222222222224) * b20;
        c21 += Real(-2.7777777777777832e-2) * b01;
        c21 += Real(0.2222222222222222) * b11;
        c21 += Real(0.3055555555555556) * b21;
        c21 += Real(-0.11111111111111116) * b02;
        c21 += Real(0.1388888888888889) * b12;
        c21 += Real(0.22222222222222224) * b22;
        c02 += Real(0.22222222222222218) * b00;
        c02 += Real(-0.11111111111111116) * b10;
        c02 += Real(-0.11111111111111122) * b20;
        c02 += Real(0.22222222222222224) * b01;
        c02 += Real(-0.1111111111111111) * b11;
        c02 += Real(-0.11111111111111116) * b21;
        c02 += Real(0.5555555555555556) * b02;
        c02 += Real(0.22222222222222224) * b12;
        c02 += Real(0.2222222222222222) * b22;
        c12 += Real(-0.11111111111111116) * b00;
        c12 += Real(-2.7777777777777832e-2) * b10;
        c12 += Real(-0.11111111111111116) * b20;
        c12 += Real(0.1388888888888889) * b01;
        c12 += Real(0.2222222222222222) * b11;
        c12 += Real(0.1388888888888889) * b21;
        c12 += Real(0.22222222222222224) * b02;
        c12 += Real(0.3055555555555556) * b12;
        c12 += Real(0.22222222222222224) * b22;
        c22 += Real(-0.11111111111111122) * b00;
        c22 += Real(-0.11111111111111116) * b10;
        c22 += Real(0.22222222222222218) * b20;
        c22 += Real(-0.11111111111111116) * b01;
        c22 += Real(-0.1111111111111111) * b11;
        c22 += Real(0.22222222222222224) * b21;
        c22 += Real(0.2222222222222222) * b02;
        c22 += Real(0.22222222222222224) * b12;
        c22 += Real(0.5555555555555556) * b22;

        Real limitter = 1;
        setLimit(limitter, b00, c00);
        setLimit(limitter, b10, c10);
        setLimit(limitter, b20, c20);
        setLimit(limitter, b01, c01);
        setLimit(limitter, b11, c11);
        setLimit(limitter, b21, c21);
        setLimit(limitter, b02, c02);
        setLimit(limitter, b12, c12);
        setLimit(limitter, b22, c22);

        limit(limitter, b00, c00);
        limit(limitter, b10, c10);
        limit(limitter, b20, c20);
        limit(limitter, b01, c01);
        limit(limitter, b11, c11);
        limit(limitter, b21, c21);
        limit(limitter, b02, c02);
        limit(limitter, b12, c12);
        limit(limitter, b22, c22);

        

	bounce(solid[p00], c00, c22);
	bounce(solid[p01], c01, c21);
	bounce(solid[p02], c02, c20);
	bounce(solid[p10], c10, c12);
	bounce(solid[p12], c12, c10);
	bounce(solid[p20], c20, c02);
	bounce(solid[p21], c21, c01);
	bounce(solid[p22], c22, c00);

	next.a00[p11] = c00;
	next.a10[p11] = c10;
	next.a20[p11] = c20;
	next.a01[p11] = c01;
	next.a11[p11] = c11;
	next.a21[p11] = c21;
	next.a02[p11] = c02;
	next.a12[p11] = c12;
	next.a22[p11] = c22;
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
	Real 
	  r = sq(vely),
	  g = dens,
	  b = sq(velx);
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
      
    flu.collision(t,flu2);
    flu2.proceed(flu);
  }
  
  return 0;
}
