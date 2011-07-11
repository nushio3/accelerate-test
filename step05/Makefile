all: main-g++.out visualize.out

main-nvcc.out : main-cuda.cu fluid.h Makefile
	nvcc -O3 -arch=sm_20 --ptxas-options=-v  main-cuda.cu -o main-nvcc.out  # --maxrregcount 16 

ptx: main-cuda.cu Makefile
	nvcc -O3 -arch=sm_20 --ptxas-options=-v  main-cuda.cu --ptx

main-icpc.out : main-omp.cpp fluid.h Makefile
	icpc -fast -openmp main-omp.cpp -o main-icpc.out 

main-g++.out : main-omp.cpp fluid.h  Makefile
	g++ -O3 --openmp main-omp.cpp -o main-g++.out 

visualize.out : visualize.cpp
	g++ -O3 --openmp visualize.cpp -o visualize.out

run : main.out
	CUDA_PROFILE=1 ./main.out 
