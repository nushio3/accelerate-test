#!/usr/bin/env ruby

def sh (str)
  STDERR.puts str
  system(str)
end

`ls bin/`.split(/\s+/).each{|dir|
  indir = 'bin/' + dir
  outdir = 'img/' + dir
  `mkdir -p #{outdir}`
  `ls #{indir}/*.bin`.split(/\s+/).each{|ifn|
    ofn_body = outdir+'/'+ifn.split(/\//)[-1].split(/\./)[0]
    ofn_ppm = ofn_body+'.ppm'
    ofn_png = ofn_body+'.png'
    unless File.exist?(ofn_png)
      sh "./visualize.out vor #{ifn} #{ofn_ppm}"
      sh "convert #{ofn_ppm} #{ofn_png}"
    end
  }
}
