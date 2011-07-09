all: main.out

main.out : main.cu Makefile
	nvcc -O3 -arch=sm_20 --ptxas-options=-v  --maxrregcount 8 main.cu -o main.out 

run : main.out
	CUDA_PROFILE=1 ./main.out 
