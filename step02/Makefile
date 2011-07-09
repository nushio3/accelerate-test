all: main.out

main.out : main.cu
	nvcc -O3 -arch=sm_20 --ptxas-options=-v --maxrregcount 32 main.cu -o main.out
