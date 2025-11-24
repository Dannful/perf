#!/bin/bash
#SBATCH --job-name=perf-gpu
#SBATCH --partition=beagle
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=24:00:00
#SBATCH --output=%x_%j.out
#SBATCH --error=%x_%j.err

CSV_FILE=./gpu.csv
mapfile -t csv_lines < <(tail -n +2 "$CSV_FILE")

cp -r . $SCRATCH/fletcher-io
cd $SCRATCH/fletcher-io
source env.sh
cd original || exit
# Ignoring this for now
GPU_SM=$(nvidia-smi --query-gpu=compute_cap --format=csv,noheader | awk -F. '{print "sm-"$1$2}')
make backend=CUDA
cd ..

for line in "${csv_lines[@]}"; do
  [[ -z "$line" ]] && continue # skip empty

  IFS=',' read -r size iterations blocks <<<"$line"

  echo "============================="
  echo "  Size: $size"
  echo "  Iterations: $iterations"
  echo "  Blocks: $blocks"

  absorption=16
  path=$HOME/fletcher_results/$(hostname)/$size/$iterations
  mkdir -p "$path"
  ./original/ModelagemFletcher.exe TTI "$size" "$size" "$size" $absorption 1 1 1 1 "$iterations" ./value.dc 0 2>&1 >"$path"/"$blocks".out
done
