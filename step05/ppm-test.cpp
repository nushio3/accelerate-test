#include "ppm.h"

using namespace noost::visualization::color;
using namespace noost::visualization::ppm;

int main () {
  ppm<double> bmp(256,256,rgb<double>(0,0,0));
  bmp.set_max_color(1);
  for (int iy = 0; iy < bmp.height(); ++iy) {
    for (int ix = 0; ix < bmp.width(); ++ix) {
      double x = double(ix)/bmp.width()*2-1;
      double y = double(iy)/bmp.height()*2-1;
      bmp(ix,iy) = rgb<double>(x*x+y*y,0,0);
    }
  }
  bmp.rescale_max_color(255);
  bmp.write("test.ppm");
  return 0;
}
