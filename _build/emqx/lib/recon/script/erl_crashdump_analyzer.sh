#!/usr/bin/env bash
DUMP=$1

echo -e "analyzing $DUMP, generated on: " `head -2 $DUMP | tail -1` "\n"

### SLOGAN ###
grep Slogan: $DUMP -m 1

### MEMORY ###
echo -e "\nMemory:\n==="
M=`grep -m 1 'processes' $DUMP | sed "s/processes: //"`
let "m=$M/(1024*1024)"
echo "  processes: $m Mb"
M=`grep -m 1 'processes_used' $DUMP | sed "s/processes_used: //"`
let "m=$M/(1024*1024)"
echo "  processes_used: $m Mb"
M=`grep -m 1 'system' $DUMP | sed "s/system: //"`
let "m=$M/(1024*1024)"
echo "  system: $m Mb"
M=`grep -m 1 'atom' $DUMP | sed "s/atom: //"`
let "m=$M/(1024*1024)"
echo "  atom: $m Mb"
M=`grep -m 1 'atom_used' $DUMP | sed "s/atom_used: //"`
let "m=$M/(1024*1024)"
echo "  atom_used: $m Mb"
M=`grep -m 1 'binary' $DUMP | sed "s/binary: //"`
let "m=$M/(1024*1024)"
echo "  binary: $m Mb"
M=`grep -m 1 'code' $DUMP | sed "s/code: //"`
let "m=$M/(1024*1024)"
echo "  code: $m Mb"
M=`grep -m 1 'ets' $DUMP | sed "s/ets: //"`
let "m=$M/(1024*1024)"
echo "  ets: $m Mb"
M=`grep -m 1 'total' $DUMP | sed "s/total: //"`
let "m=$M/(1024*1024)"
echo -e "  ---\n  total: $m Mb"

### PROCESS MESSAGE QUEUES LENGTHS ###
echo -e "\nDifferent message queue lengths (5 largest different):\n==="
grep 'Message queue len' $DUMP | sed 's/Message queue length: //g' | sort -n -r | uniq -c | head -5 

### ERROR LOGGER QUEUE LENGTH ###
echo -e "\nError logger queue length:\n==="
grep -C 10 'Name: error_logger' $DUMP -m 1| grep 'Message queue length' | sed 's/Message queue length: //g'


### PORT/FILE DESCRIPTOR INFO ###
echo -e "\nFile descriptors open:\n==="
echo -e "  UDP: "   `grep 'Port controls linked-in driver:' $DUMP | grep 'udp_inet' | wc -l`
echo -e "  TCP: "   `grep 'Port controls linked-in driver:' $DUMP | grep 'tcp_inet' | wc -l`
echo -e "  Files: " `grep 'Port controls linked-in driver:' $DUMP | grep -vi 'udp_inet' | grep -vi 'tcp_inet' | wc -l`
echo -e "  ---\n  Total: " `grep 'Port controls linked-in driver:' $DUMP | wc -l`

### NUMBER OF PROCESSES ###
echo -e "\nNumber of processes:\n==="
grep '=proc:' $DUMP | wc -l

### PROC HEAPS+STACK ###
echo -e "\nProcesses Heap+Stack memory sizes (words) used in the VM (5 largest different):\n==="
grep 'Stack+heap' $DUMP | sed "s/Stack+heap: //g" | sort -n -r | uniq -c | head -5

### PROC OLDHEAP ###
echo -e "\nProcesses OldHeap memory sizes (words) used in the VM (5 largest different):\n==="
grep 'OldHeap' $DUMP | sed "s/OldHeap: //g" | sort -n -r | uniq -c | head -5

### PROC STATES ###
echo -e "\nProcess States when crashing (sum): \n==="
grep 'State: ' $DUMP | sed "s/State: //g" | sort | uniq -c
