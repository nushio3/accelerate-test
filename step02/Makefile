all: main.out

main.out : main.cu
	nvcc -O3 -arch=sm_20 main.cu -o main.out
