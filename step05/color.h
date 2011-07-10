#pragma once

/**
   色を表すクラス。
   noost::visualization::color::linear_algebraネームスペースを使用すれば
   線形代数演算子が定義される。
 */

#include <iostream>
#include <cmath>

namespace noost{
namespace visualization{
namespace color{

template <class quantum>
struct rgb{
  quantum r,g,b;
  rgb(){}
  rgb(quantum r, quantum g, quantum b):r(r),g(g),b(b){}
};


template <class quantum>
bool operator==(const rgb<quantum> &a,const rgb<quantum> &x){
  return x.r==a.r && x.g==a.g && x.b==a.b;
}

namespace linear_algebra{

///vector addition
template<class t>
rgb<t> operator+(const rgb<t> & a, const rgb<t> & b){
  return rgb<t>(a.r + b.r,a.g + b.g,a.b + b.b);
}

///vector subtraction
template<class t>
rgb<t> operator-(const rgb<t> & a, const rgb<t> & b){
  return rgb<t>(a.r - b.r,a.g - b.g,a.b - b.b);
}

///vector negate
template<class t>
rgb<t> operator-(const rgb<t> & a){
  return rgb<t>(-a.r,-a.g,-a.b);
}

///scalar multiplication
template<class t>
rgb<t> operator*(const t &m, const rgb<t> & a){
  return rgb<t>(m*a.r,m*a.g,m*a.b);
}
template<class t>
rgb<t> operator*(const rgb<t> & a, const t &m){
  return rgb<t>(m*a.r,m*a.g,m*a.b);
}

///scalar multiplication by reciprocal of \a m
template<class t>
rgb<t> operator/(const rgb<t> & a, const t &m){
  return rgb<t>(a.r/m,a.g/m,a.b/m);
}

///inner product
template<class t>
t operator*(const rgb<t> &a, const rgb<t> & b){
  return a.r*b.r + a.g*b.g + a.b*b.b;
}

///outer product; outer product uses % operator.
template<class t>
rgb<t> operator%(const rgb<t> & a, const rgb<t> & b){
  return rgb<t>(a.g*b.b - a.b*b.g,a.b*b.r - a.r*b.b,a.r*b.g - a.g*b.r);
}

/// +=
template<class t>
rgb<t>& operator+=(rgb<t> &a, const rgb<t> &b){
  return a=a+b;
}
/// -=
template<class t>
rgb<t>& operator-=(rgb<t> &a, const rgb<t> &b){
  return a=a-b;
}
/// *=
template<class t>
rgb<t>& operator*=(rgb<t> &a, const t &b){
  return a=b*a;
}
/// /=
template<class t>
rgb<t>& operator/=(rgb<t> &a, const t &b){
  return a=a/b;
}


/*
///equality between two vectors 
template<class t>
bool operator==(const rgb<t> & a, const rgb<t> & b){
  return a.r==b.r && a.g==b.g && a.b==b.b;
}
*/

///dictionary order of vectors
template<class t>
bool operator<(const rgb<t> & a, const rgb<t> & b){
  if(a.r != b.r) return a.r < b.r;
  if(a.g != b.g) return a.g < b.g;
  return a.b < b.b;
}

///square of absolute value
template<class t>
t norm(const rgb<t> & a){
  return a*a;
}

///absolute value
template<class t> 
t abs(const rgb<t> & a){
  return std::sqrt(norm(a));
}

///projection factor of vector \a a in direction of \a b
template<class t>
t operator/(const rgb<t> & a, const rgb<t> & b){
  return (a*b) / (b*b);
}



///read from istream
template<class t> 
std::istream& operator>>(std::istream& cin, rgb<t>& a){
  t tmp0,tmp1,tmp2; 
  cin  >> tmp0 >> tmp1 >> tmp2;
  a = rgb<t>(tmp0,tmp1,tmp2);
  return cin;
}

///write to ostream
template<class t> 
std::ostream& operator<<(std::ostream& cout, const rgb<t> &a){
  return cout << a.r<< " " << a.g<< " " << a.b;
}


}

}
}
}
