#include <algorithm>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>
#include <stdlib.h>
#include <vector>
using namespace std;

typedef float Real;
#define eps  1e-20
int zoom;
Real flowSpeed;

template<class T> T sq(const T &x) { return x*x; }

struct FluidPtr {
  int size;
  int width;
  int height;

  Real *a00, *a01, *a02, *a10, *a11, *a12, *a20, *a21, *a22;
  Real *solid;

  void initialize () {
    for (int y = 0; y < height; ++y) {
      for (int x = 0; x < width; ++x) {
	const int p = y  * width + x;
	const Real r = height/24;
	const Real oy = height/2;
	const Real ox = 4*r;
	solid[p] = 64*sq(x-ox) + sq(y-oy) < sq(r) ? 1 : 0;
	if(y==0 || y == height-1) solid[p] = 1;
	Real w = 0.5*(Real(1) - solid[p]);
	a00[p] = a02[p] = a10[p] = a12[p] = a20[p] = a22[p] = 0;
	a01[p] = w*0.2*(Real(1)-flowSpeed);
	a11[p] = w*0.7;
	a21[p] = w*0.2 + 1e-3 * sin(Real(12)*y/height);;
      }
    }
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

  
  void collision (const int t, FluidPtr next) {
#pragma omp parallel for
    for (int y = 0; y < height-2; ++y) {
      for (int x = 0; x < width-2; ++x) {
	const int x1 = x+1;
	const int x2 = x+2;
	const int y1 = y+1;
	const int y2 = y+2;
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

  void proceed (FluidPtr &next) {
#pragma omp parallel for
    for (int y = 0; y < height-2; ++y) {
      for (int x = 0; x < width-2; ++x) {
	const int x1 = x+1;
	const int x2 = x+2;
	const int y1 = y+1;
	const int y2 = y+2;
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

};

Real *raw(vector<Real> & vec) {
  return &vec[0];
}

template <class container>
struct FluidMemory {
  int size;
  int width;
  int height;

  container a00, a01, a02, a10, a11, a12, a20, a21, a22;
  container solid;
  FluidMemory (int width0, int height0) :
    size(width0*height0), width(width0), height(height0),
    a00(size), a01(size), a02(size),
    a10(size), a11(size), a12(size),
    a20(size), a21(size), a22(size), solid(size) {
  }
  FluidPtr ptr () {
    FluidPtr ret;
    ret.size=size; ret.width=width; ret.height=height;
    ret.a00 = raw(a00);
    ret.a10 = raw(a10);
    ret.a20 = raw(a20);
    ret.a01 = raw(a01);
    ret.a11 = raw(a11);
    ret.a21 = raw(a21);
    ret.a02 = raw(a02);
    ret.a12 = raw(a12);
    ret.a22 = raw(a22);
    ret.solid = raw(solid);
    return ret;
  }
  void write (string fn) {
    FILE *fp=fopen(fn.c_str(), "w"); 
    if (!fp) return;
    {
      const int bmpWidth = width/zoom;
      const int bmpHeight = height/zoom;
      const int bmpSize = bmpWidth * bmpHeight;

      fwrite(&bmpWidth, sizeof(bmpWidth), 1, fp);
      fwrite(&bmpHeight, sizeof(bmpHeight), 1, fp);
      int sizeOfReal = sizeof(Real);
      fwrite(&sizeOfReal, sizeof(sizeOfReal), 1, fp);
      
      vector<Real> densBlock(bmpSize),momxBlock(bmpSize),momyBlock(bmpSize),enrgBlock(bmpSize);
      
      for (int y = 0; y < bmpHeight; ++y) {
        for (int x = 0; x < bmpWidth; ++x) {
          Real dens=0, momx=0, momy=0, enrg=0;
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
              enrg
                +=2*a00[addr]+a01[addr]+2*a02[addr]
                +   a10[addr]+         +  a12[addr]
                + 2*a20[addr]+a21[addr]+2*a22[addr];
            }
          }
          dens /=   zoom*zoom;
          momx /=   zoom*zoom;
          momy /=   zoom*zoom;
          enrg /= 2*zoom*zoom;
          const int addr = y*bmpWidth+x;
          densBlock[addr] = dens;
          momxBlock[addr] = momx;
          momyBlock[addr] = momy;
          enrgBlock[addr] = enrg;
        }
      }

      fwrite(&densBlock[0], sizeof(Real), bmpSize, fp);
      fwrite(&momxBlock[0], sizeof(Real), bmpSize, fp);
      fwrite(&momyBlock[0], sizeof(Real), bmpSize, fp);
      fwrite(&enrgBlock[0], sizeof(Real), bmpSize, fp);
    } fclose(fp);
  }
};




int main (int argc, char **argv) {
  if (argc <= 2) {
    zoom = 1; flowSpeed=0.5;
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

  pFlu.initialize();
  pFlu2.initialize();
  
  for (int t = 0; t < zoom*100001; ++t) {
    if (t % (zoom*100) == 0) {
      ostringstream ossFn;
      ossFn << dirn << "/" << (100000000+t) << ".bin";
      cerr << ossFn.str() << endl;
      flu.write(ossFn.str());
    }
      
    pFlu.collision(t,pFlu2);
    pFlu2.proceed(pFlu);
  }
  
  return 0;
}
