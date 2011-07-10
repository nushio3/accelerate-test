#pragma once

#include <stdint.h>
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <complex>

#include "color.h"
#include "vector.h"

namespace noost{
namespace visualization{
namespace ppm{
  
typedef int image_coordinate_t;

  /**
     フルカラーppmフォーマットの画像を読み書きできるクラス。
     ピクセルは２次元ベクトル、複素数、2つの\a image_coordinate_t
     のいづれかを利用してアクセスできる。
     範囲外アクセスチェックは常に行い、速度よりも安全性を重視している。
     quantumには任意の型が使えるが、出力時には[0..255]に規格化される。
   */
template <class quantum>
class ppm{

private:
  quantum my_max_color;  
  image_coordinate_t w, h;

   /// ビットマップデータ
  std::vector<color::rgb<quantum> > bmp;

  /// 領域外のピクセルを参照しようとした時に返す変数
  mutable noost::visualization::color::rgb<quantum> __throw; 

protected:
  /// 読み込み中などに何らかの例外が起こる等、
  /// 画像が期待通りに読み込めなかったりデフォルト画像が利用されたりしたことを示すフラグ
  bool is_valid_;
public:
  void set_is_valid(bool v) { is_valid_ = v; }
  bool is_valid() { return is_valid_;}
  
public:
  ///背景色、領域外のピクセルを参照したときに返る色
  color::rgb<quantum> bgcolor; 

  ///画像の横幅
  const image_coordinate_t width() const{return w;}

  ///画像の縦幅
  const image_coordinate_t height() const{return h;}
  
  const bool in(const image_coordinate_t &x, const image_coordinate_t &y) const{ 
    ///指定された点が画像の内側に入っているか
    return x>=0 && y>=0 && x<w && y<h;
  }
  template<class r>
  const bool in(const noost::math::vector::component_by_name::vector2<r> &p) const{
    ///指定された点が画像の内側に入っているか
    return in((image_coordinate_t)p.x, (image_coordinate_t)p.y);
  }
  template<class r>
  const bool in(const std::complex<r> &p) const{
    ///指定された点が画像の内側に入っているか
    return in((image_coordinate_t)p.real(), (image_coordinate_t)p.imag());
  }

  const color::rgb<quantum> &operator()(image_coordinate_t x,image_coordinate_t y) const{
    ///()演算子：指定されたピクセルの色を取得
    if(!in(x,y)) {
      __throw = bgcolor;
      return __throw;
    }
    return bmp[y*w+x];
  }
  color::rgb<quantum> &operator()(image_coordinate_t x,image_coordinate_t y){
    ///()演算子：指定されたピクセルの色を参照
    if(!in(x,y)) {
      __throw = bgcolor;
      return __throw;
    }
    return bmp[y*w+x];
  }

  template<class r>
  const color::rgb<quantum> &operator()(const noost::math::vector::component_by_name::vector2<r> &p) const{
    ///()演算子：指定されたピクセルの色を取得
    return operator()((image_coordinate_t)p.x,(image_coordinate_t)p.y);
  }
  template<class r>
  color::rgb<quantum> &operator()(const noost::math::vector::component_by_name::vector2<r> &p){
    ///()演算子：指定されたピクセルの色を参照
    return operator()((image_coordinate_t)p.x,(image_coordinate_t)p.y);
  }
  template<class r>
  const color::rgb<quantum> &operator()(const std::complex<r> &p) const{
    ///()演算子：指定されたピクセルの色を取得
    return operator()((image_coordinate_t)p.real(),(image_coordinate_t)p.imag());
  }
  template<class r>
  color::rgb<quantum> &operator()(const std::complex<r> &p){
    ///()演算子：指定されたピクセルの色を参照
    return operator()((image_coordinate_t)p.real(),(image_coordinate_t)p.imag());
  }

  /// 色の最大値を取得
  const quantum max_color() const{return my_max_color;}

  /// 色の最大値を強制
  void set_max_color(quantum mc) {my_max_color = mc;}

  /// 色の最大値を変更しそれに比例してピクセルの内容を変更する
  void rescale_max_color(quantum mc) {
    double scale = mc / my_max_color;
    for(int i = 0; i < bmp.size(); ++i){
      bmp[i].r = quantum(bmp[i].r * scale);
      bmp[i].g = quantum(bmp[i].g * scale);
      bmp[i].b = quantum(bmp[i].b * scale);
    }
    my_max_color = mc;
  }



  ///何もしないコンストラクタ
  ppm() : bgcolor(color::rgb<quantum>(0,0,0)),is_valid_(false) {}

  ///\a w * \a h サイズのビットマップを確保、初期化は行わない
  ppm(int w,int h):w(w), h(h), bmp(w*h), bgcolor(color::rgb<quantum>(0,0,0)), is_valid_(false) {}

  ///\a w * \a h サイズのビットマップを確保、背景色で初期化を行う
  ppm(int w,int h,color::rgb<quantum> c):w(w), h(h), bmp(w*h,c), bgcolor(c), is_valid_(true){}

  ///コピーコンストラクタいらんよねえ
  //ppm(const ppm<quantum> &p):w(p.w), h(p.h), bmp(p.bmp), bgcolor(p.bgcolor){}

  ///指定されたファイル名のファイルから読み込んで初期化
  ///読み込みに失敗した場合はデフォルト画像を利用するか、
  ///デフォルト画像が指定されない場合は1x1ピクセルの黒画像を生成
  ppm(const std::string &filename, 
      ppm<quantum> default_ppm=ppm<quantum>(1,1,color::rgb<quantum>(0,0,0)))  :bgcolor(color::rgb<quantum>(0,0,0)){
    is_valid_ = true;
    std::ifstream cin(filename.c_str());
    if(!cin || !read(cin)){
      *this=default_ppm;
      is_valid_ = false;
    }
  }

  ///指定された入力ストリームから読み込んで初期化
  ///読み込みに失敗した場合はデフォルト画像を利用するか、
  ///デフォルト画像が指定されない場合は1x1ピクセルの黒画像を生成
  ppm(std::istream &cin, 
      ppm<quantum> default_ppm=ppm<quantum>(1,1,color::rgb<quantum>(0,0,0))){
    is_valid_ = true;
    if(!cin || !read(cin)){
      *this=default_ppm;
      is_valid_ = false;
    }
  }

  ///入力ストリームから破壊的に読み込み、成功したかどうかを返す
  bool read(std::istream &cin){
    int header_count=0;
    std::string format_str; quantum c_m;
    while(header_count < 3){
      std::string line;
      if(!cin) return is_valid_ = false;
      getline(cin,line);
      if(line[0]=='#'){
	continue;
      }else{
	std::istringstream iss(line);
	switch(header_count++){
	case 0:
	  iss>>format_str;
	  break;
	case 1:
	  iss >> w >> h;
	  break;
	case 2:
	  iss >> c_m;
	  my_max_color = c_m;
	  break;
	default:
#ifdef TEST
	  EXPECT_TRUE(false) << "ppm header parsing algorithm wrong" << std::endl;
#endif
	  break;
	}
      }
    }

    bmp.clear();
    for(int y=0;y<h;++y){
      for(int x=0;x<w;++x){
	quantum r,g,b;
	if(!cin) return is_valid_ = false;
	r=(quantum)cin.get();
	if(!cin) return is_valid_ = false;
	g=(quantum)cin.get();
	if(!cin) return is_valid_ = false;
	b=(quantum)cin.get();
	bmp.push_back(color::rgb<quantum>(r,g,b));
      }
    }
    return is_valid_ = true;
  }
  /// ファイル \a filename から読み込み
  bool read(const std::string &filename){
    std::ifstream cin(filename.c_str());
    return read(cin);
  }

  ///出力ストリームに書き出し
 private:
  uint8_t nc(quantum c){
    if(c<0) c=0;
    if(c>255) c=255;
    return uint8_t(c);
  }
 public:
  void write(std::ostream &cout){

    cout << "P6" << std::endl 
	 << w << " " << h << std::endl 
	 << my_max_color << std::endl;
    for(size_t i=0;i<bmp.size();++i){
      cout.put(nc(bmp[i].r));
      cout.put(nc(bmp[i].g));
      cout.put(nc(bmp[i].b));
    }
  }

  /// ファイル \a filename に書き出し
  void write(const std::string &filename){
    std::ofstream cout(filename.c_str());
    write(cout);
  }
};

}
} 
}
