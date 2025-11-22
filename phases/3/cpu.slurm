#!/bin/bash
#SBATCH --job-name=perf
#SBATCH --partition=lunaris
#SBATCH --nodes=1
#SBATCH --ntasks=32
#SBATCH --time=24:00:00
#SBATCH --output=%x_%j.out
#SBATCH --error=%x_%j.err

CSV_FILE=./cpu.csv
mapfile -t csv_lines < <(tail -n +2 "$CSV_FILE")

source env.sh
cd original
make backend=OpenMP
cd ..

for line in "${csv_lines[@]}"; do
    [[ -z "$line" ]] && continue # skip empty

    IFS=',' read -r size iterations threads blocks <<< "$line"

    echo "============================="
    echo "  Size: $size"
    echo "  Iterations: $iterations"
    echo "  Threads: $threads"
    echo "  Blocks: $blocks"

    absorption=$(( $size/10 ))
    path=$(hostname)/$size/$iterations/$threads
    mkdir -p $path
    OMP_NUM_THREADS=$threads ./original/ModelagemFletcher.exe TTI $size $size $size $absorption 1 1 1 1 $iterations ./value.dc 0 2>&1 > $path/$blocks.out
done
