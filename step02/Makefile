all: main.out main-omp.out

main.out : main.cu Makefile
	nvcc -O3 -arch=sm_20 --ptxas-options=-v  --maxrregcount 8 main.cu -o main.out 

main-omp.out : main-omp.cpp Makefile
	icpc -fast -openmp main-omp.cpp -o main-omp.out 

run : main.out
	CUDA_PROFILE=1 ./main.out 
