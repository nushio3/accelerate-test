#t2sub -q G -W group_list=t2g-ppc-all -l select=1:gpus=3:mem=21gb -l walltime=24:00:00 ./tools/batch_execute.sh
#t2sub -q G -W group_list=t2g-ppc-all -l select=1:gpus=1:mem=21gb -l walltime=24:00:00 ./tools/batch_execute.sh
t2sub -N autotuning -q S -W group_list=t2g-ppc-all -l select=1:ncpus=24:mem=21gb -l walltime=8:00:00 ./batch_execute_S.sh 

