all: main-g++.out visualize.out

main-nvcc.out : main.cu Makefile
	nvcc -O3 -arch=sm_20 --ptxas-options=-v  --maxrregcount 16  main.cu -o main.out 

main-icpc.out : main-omp.cpp Makefile
	icpc -fast -openmp main-omp.cpp -o main-icpc.out 

main-g++.out : main-omp.cpp Makefile
	g++ -O3 --openmp main-omp.cpp -o main-g++.out 

visualize.out : visualize.cpp
	g++ -O3 visualize.cpp -o visualize.out

run : main.out
	CUDA_PROFILE=1 ./main.out 
