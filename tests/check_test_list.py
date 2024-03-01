#!/usr/bin/env python3

import os

# Get all dk files
existing_files = {
    os.path.join(dp, f)
    for dp, __build_class__, filenames in os.walk("tests")
    for f in filenames
    if (os.path.splitext(f)[1] == '.dk'
    and not (dp in ["tests/universo/output", "tests/universo/simplified_output"]))
  }

# Open main.ml
main_file = open("tests/main.ml", 'r')

referenced_files = set()

current_dir=""
for line in main_file:
  l=line.split('"')
  for k in range(len(l)):
    if k % 2 == 1:
      if l[k][-1] == "/":
        current_dir=l[k]
      elif l[k][-3:] == ".dk":
        if '/' in l[k]:
          referenced_files.add(l[k])
        else:
          referenced_files.add(current_dir + l[k])

main_file.close()

res=0

for file in existing_files:
  if file not in referenced_files:
    print("File {f} not referenced in tests/main.ml".format(f=file))
    res=1

exit(res)
