#!/usr/bin/python3

import sys
import re

RE_RUN = r"=== (?P<name>[^=]+) ===\n(?:(?:[^=]+\n|\n))+"

RE_TOTAL_ALLOCATED_B = r"^ *([0-9,]+) bytes allocated in the heap$"
RE_COPIED_B = r"^ *([0-9,]+) bytes copied during GC$"
RE_RESIDENCY_B = r"^ *([0-9,]+) bytes maximum residency.*$"
RE_SLOP_B = r"^ *([0-9,]+) bytes maximum slop$"
RE_TOTAL_MEM_MB = r"^ *([0-9,]+) MiB total memory in use.*$"
RE_TIME_GC_S = r"^ *GC +time +([0-9.]+)s.*$"
RE_TIME_TOTAL_S = r"^ *Total +time +([0-9.]+)s.*$"

"""
=== Benchmark All.Queue.benchmark.2^10.funcImpl ===


Done!
         218,952 bytes allocated in the heap
          15,600 bytes copied during GC
          56,616 bytes maximum residency (1 sample(s))
          25,304 bytes maximum slop
               6 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.002s  (  0.002s elapsed)
  MUT     time    0.000s  (  0.000s elapsed)
  GC      time    0.000s  (  0.000s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.002s  (  0.002s elapsed)

  Alloc rate    970,033,138 bytes per MUT second

  Productivity   9.9% of total user, 13.7% of total elapsed
"""

def main():
    input_path = sys.argv[1]
    with open(input_path, "r", encoding="utf-8") as input_file:
        content = input_file.read()
    for match in re.finditer(RE_RUN, content, re.MULTILINE):
        match_content = match.group(0)
        name = match.group("name")
        raw_total_allocated_b = re.search(RE_TOTAL_ALLOCATED_B, match_content, re.MULTILINE).group(1)
        total_allocated_b = int(raw_total_allocated_b.replace(",", ""))
        raw_copied_b = re.search(RE_COPIED_B, match_content, re.MULTILINE).group(1)
        copied_b = int(raw_copied_b.replace(",", ""))
        raw_residency_b = re.search(RE_RESIDENCY_B, match_content, re.MULTILINE).group(1)
        residency_b = int(raw_residency_b.replace(",", ""))
        raw_slop_b = re.search(RE_SLOP_B, match_content, re.MULTILINE).group(1)
        slop_b = int(raw_slop_b.replace(",", ""))
        raw_total_mem_mb = re.search(RE_TOTAL_MEM_MB, match_content, re.MULTILINE).group(1)
        total_mem_b = int(raw_total_mem_mb.replace(",", "")) * 1024 * 1024
        raw_time_gc_s = re.search(RE_TIME_GC_S, match_content, re.MULTILINE).group(1)
        time_gc_ps = int(round(float(raw_time_gc_s) * 1e12, 0))
        raw_time_total_s = re.search(RE_TIME_TOTAL_S, match_content, re.MULTILINE).group(1)
        time_total_ps = int(round(float(raw_time_total_s) * 1e12, 0))
        # CSV tasty
        # Name,Mean time (ps),2*Stdev time (ps),Allocated (bytes),Copied (bytes),Peak Memory/Total memory (bytes),GC time (ps),Residency (bytes),Slop (bytes)]
        elems = [name, str(time_total_ps), "", str(total_allocated_b), str(copied_b), str(total_mem_b), str(time_gc_ps), str(residency_b), str(slop_b)]
        print(",".join(elems))

if __name__ == "__main__":
    main()
