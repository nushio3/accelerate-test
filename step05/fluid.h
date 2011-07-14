#pragma once

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <string>
#include <stdlib.h>
#include <vector>
using namespace std;

#ifdef __CUDACC__
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#else
// let the compiler ignore CUDA 
#define __host__
#define __device__
#endif

#define eps  Real(1e-20)

#ifdef __CUDACC__
#define X_INIT   threadIdx.x
#define Y_INIT   blockIdx.x
#define X_STRIDE blockDim.x
#define Y_STRIDE gridDim.x
#else
#define X_INIT   0
#define Y_INIT   0
#define X_STRIDE 1
#define Y_STRIDE 1
#endif


template<class T>
__device__ __host__
T sq(const T &x) { return x*x; }

struct FluidPtr {
  int size;
  int width;
  int height;

  Real *a00, *a01, *a02, *a10, *a11, *a12, *a20, *a21, *a22;
  Real *solid;
  __device__ 
  void initialize (Real flowSpeed) {
    for (int y = Y_INIT; y < height; y += Y_STRIDE) {
      for (int x = X_INIT; x < width; x += X_STRIDE) {
	const int p = y  * width + x;
	const Real r = height/24;
	const Real oy = height/2;
	const Real ox = 4*r;
	solid[p] = 64*sq(x-ox) + sq(y-oy) < sq(r) ? 1 : 0;
	if(y==0 || y == height-1) solid[p] = 1;
	Real w = Real(0.5)*(Real(1) - solid[p]);
	a00[p] = a02[p] = a10[p] = a12[p] = a20[p] = a22[p] = 0;
	a01[p] = w*Real(0.2)*(Real(1)-flowSpeed);
	a11[p] = w*Real(0.7);
	a21[p] = w*Real(0.2) + Real(1e-3) * sin(Real(12)*y/height);;
      }
    }
  }

  __device__
  Real collide (Real &a, Real &b) {
    Real s = a*b/(a+b+eps);
    a-=s; b-=s; return 2*s;
  }
  __device__
  void thermalize(Real &src, Real &a, Real &b) {
    src *= Real(1)/Real(3);
    a+=src; b+=src;
  }
  __device__
  void thermalize(Real &src, Real &a, Real &b, Real &c, Real &d) {
    src *= Real(1)/Real(5);
    a+=src; b+=src; c+=src; d+=src;
  }
  __device__
  void bounce (const Real solidity, Real &src, Real &dest) {
    Real amt = src * solidity; 
    dest += amt;
    src -= amt;
  }
  __device__
  Real mix(const Real w, const Real n, const Real vx, const Real vy, const Real ux, const Real uy) {
    const Real inp = (ux*vx+uy*vy);
    return n*w*(Real(1) + Real(3) * inp + Real(4.5)*sq(inp) - Real(1.5)*(ux*ux+uy*uy));
  }

  
  __device__
  void collision (FluidPtr next) {
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (int y = 1 + Y_INIT; y < height - 1; y += Y_STRIDE) {
      for (int x = 1 + X_INIT; x < width - 1; x += X_STRIDE) {
	const int p11 = y * width + x;
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

  __device__
  void proceed (FluidPtr &next) {
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (int y = Y_INIT; y < height -2 ; y += Y_STRIDE) {
      for (int x = X_INIT; x < width -2 ; x += X_STRIDE) {
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

#ifdef __CUDACC__
Real *raw(thrust::host_vector<Real> & vec) {
  return &vec[0];
}
Real *raw(thrust::device_vector<Real> & vec) {
  return thrust::raw_pointer_cast(&*vec.begin());
}
#endif

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

  template <class container2>
  void copyFrom (FluidMemory<container2> &other) {
    a00 = other.a00;
    a10 = other.a10;
    a20 = other.a20;
    a01 = other.a01;
    a11 = other.a11;
    a21 = other.a21;
    a02 = other.a02;
    a12 = other.a12;
    a22 = other.a22;
    solid = other.solid;
  }
  
  void write (string fn, int zoom) {
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


