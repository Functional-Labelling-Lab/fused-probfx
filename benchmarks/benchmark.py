import os, subprocess, csv
from typing import Dict, List

class colour:
    GREEN = "\033[92m"
    RED = "\033[91m"
    RESETCOLOUR = "\033[0m"

BENCHMARK_DIR = "."


def clean_old(benchmark: str):
    csv_path = f"{BENCHMARK_DIR}/{benchmark}/benchmark-results.csv"
    if os.path.exists(csv_path):
        print(f"{colour.GREEN}Removing old {benchmark} results{colour.RESETCOLOUR}")
        os.remove(csv_path)


def collectbenchmarks():
    timings = {}
    benchmark_dirs = next(os.walk(BENCHMARK_DIR))[1]
    benchmark_string = ",".join(benchmark_dirs)
    print(f"{colour.GREEN}Running benchmarks: {benchmark_string} results{colour.RESETCOLOUR}")

    def insert_timing(bench: str, name: str, mean: float):
        if name in timings:
            timings[bench][name] = mean
        else:
            timings[bench] = {name: mean}

    def readcsv(benchmark: str):
        with open(f"{BENCHMARK_DIR}/{benchmark}/benchmark-results.csv") as f:
            for row in csv.DictReader(f):
                try:
                    insert_timing(benchmark, row["Name"], float(row["Mean"]))
                except ValueError:
                    pass

    for benchmark in benchmark_dirs:
        try:
            print(f"{colour.GREEN}Starting benchmark {benchmark}{colour.RESETCOLOUR}")
            clean_old(benchmark)
            subprocess.run(f"cd {BENCHMARK_DIR}/{benchmark} && ./bench.sh", shell=True, check=True).check_returncode()
            print(f"{colour.GREEN}benchmark {benchmark} complete, reading csv file{colour.RESETCOLOUR}")
            readcsv(benchmark)
        except subprocess.CalledProcessError:
            print(f"{colour.RED}benchmark {benchmark} failed{colour.RESETCOLOUR}")
        except FileNotFoundError:
            print(f"{colour.RED}benchmark {benchmark} csv not found{colour.RESETCOLOUR}")
    return timings, benchmark_dirs

def displaytimings(timings: Dict[str, Dict[str, float]], benches: List[str]):
    formatstring = "{:<20} " + len(benches) * "{:<10}"
    print(formatstring.format("Benchmark:", *benches))
    for (bench, res)  in timings.items():
        print(formatstring.format(bench, *[res[name] for name in benches]))

if __name__ == "__main__":
    displaytimings(*collectbenchmarks())
