#!/usr/bin/python

# A simple script to grap console metrix and report benchmark execution time
# Note for iterative algorithm, only count iteration larger than 5

import sys,string

def process_perf_lines(lines):
    
    list2vec_time = 0.0 #va_list2vec time 
    mode = 0 #0 invalid 1: iterative; 2: direct
    num_iters = 0
    iter_total_time = 0
    
    for line in lines:
        if line.startswith('[INFO]Time'):
            #[INFO]Time = 2.421
            #direct algorithm report
            mode = 2
            values = string.split(line, ' ')
            direct_time = float(values[2])
        elif line.startswith('[INFO]Iter'):
            #[INFO]Iter 3 Time = 3.604
            mode = 1
            values = string.split(line, ' ')
            iter_num = int(values[1])
            if iter_num > 5:
                num_iters = num_iters + 1
                iter_total_time = iter_total_time + float(values[4])
        elif line.startswith('[INFO]va_list2vec'):    
            #[INFO]va_list2vec Time = 0.445
            values = string.split(line, ' ')
            list2vec_time = list2vec_time + float(values[3])
     
    if (mode == 2):
         print "[Direct]Time = %f TimeWithoutOH = %f Overhead = %f%%" % (direct_time, direct_time - list2vec_time, list2vec_time/direct_time * 100)
    elif mode == 1:
         if num_iters >0:
             avg_iter_time = iter_total_time/num_iters
             print "[Iterative]AvgIterTime = %f Overhead = %f%%"% (avg_iter_time, list2vec_time/avg_iter_time *100)
         else:
             print "[ERROR]Iterations is less or equal than 5!"
    else:
         print "[ERROR] Didn't capture time info" 

if __name__ == "__main__":
    lines = [line.strip() for line in sys.stdin.readlines()]
    process_perf_lines(lines)