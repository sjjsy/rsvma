#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
#
### Some preprocessing code
#
# Essentially rewrites the data into another file without excess whitespace and
# misplaced line endings.
#
# Usage: python3 src/pp.py pp ma_05-15.csv

import sys, csv, re, errno


cmd = sys.argv[1]
pf = sys.argv[2]

if cmd == 'pp':
  with open(pf.replace('.csv', '_pp.csv'), 'w') as fout:
    wrtr = csv.writer(fout, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    cells = []
    with open(pf, newline='') as fcsv:
      rdr = csv.reader(fcsv, delimiter=',', quotechar='"')
      for row in rdr:
        for cell in row:
          cell = re.sub('\n+', ' ', cell)
          cell = re.sub('\s\s+', ' ', cell).strip()
          cells.append(cell)
        if len(cells) >= 56:
          wrtr.writerow(cells[:56])
          del cells[:56]
elif cmd == 'check':
  with open(pf) as fcsv:
    rdr = csv.reader(fcsv, delimiter=';', quotechar='"')
    for row in rdr:
      try:
        print(len(row))
      except IOError as e:
        if e.errno == errno.EPIPE:
          print('Stopped due to broken pipe!')

# EOF
