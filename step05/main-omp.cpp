#include <iostream>
#include <vector>
using namespace std;
#include "ppm.h"

using namespace noost::visualization::color;
using namespace noost::visualization::ppm;

typedef float Real;
const Real eps = 1e-20;


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
    a20(size), a21(size), a22(size), solid(size) {}

  Real collide (Real &a, Real &b) {
    Real s = a*b/(a+b+eps);
    a-=s; b-=s; return s;
  }
  void bounce (const Real solidity, Real &src, Real &dest) {
    Real amt = src * solidity; 
    dest += amt;
    src -= amt;
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
	const int p02 = y1 * width + x;
	const int p12 = y1 * width + x1;
	const int p22 = y1 * width + x2;
	Real b00 = a00[p11];
	Real b10 = a10[p11];
	Real b20 = a20[p11];
	Real b01 = a01[p11];
	Real b11 = a11[p11];
	Real b21 = a21[p11];
	Real b02 = a02[p11];
	Real b12 = a12[p11];
	Real b22 = a22[p11];
	b11 += collide(b00,b22);
	b11 += collide(b02,b20);
	b01 += collide(b00,b02);  
	b21 += collide(b20,b22);  
	b10 += collide(b00,b20);  
	b12 += collide(b02,b22);
	b11 += collide(b10,b12);
	b11 += collide(b01,b21);
	b11 *= Real(0.2);
	b10 += b11;
	b12 += b11;
	b01 += b11;
	b21 += b11;
	const Real r13 = Real(1)/Real(3);
	b10 *= r13;
	b12 *= r13;
	b01 *= r13;
	b21 *= r13;
	b00 += b10+b01;
	b02 += b12+b01;
	b20 += b21+b10;
	b22 += b12+b21;
	bounce(solid[p00], b00, b11);
	bounce(solid[p01], b01, b11);
	bounce(solid[p02], b02, b11);
	bounce(solid[p10], b10, b11);
	bounce(solid[p12], b12, b11);
	bounce(solid[p20], b20, b11);
	bounce(solid[p21], b21, b11);
	bounce(solid[p22], b22, b11);
      }
    }
  }

};

int main () {
  Fluid flu(256,256);
  Fluid flu2=flu;
  return 0;
}
