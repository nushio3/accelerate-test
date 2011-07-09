cd /home/usr5/11ITA066/accelerate-test/step02
OMP_NUM_THREADS=24 ./main-omp.out
#mpirun -n 3 -hostfile $PBS_NODEFILE ./demo.out --new
#mpirun -n 1 -hostfile $PBS_NODEFILE ./demo.out --new




