#!/usr/bin/python
#
# This scrip takes a file which has a token on each line, and produces 
# outputs bigrams of the tokens. An empty line is interpreted as
# sentence boundary, which is represented by token '###' in the
# output.

import sys

ptoken = '###'
for line in sys.stdin:
    token = line.strip()
    if not token:
        token = '###'
    print '%s %s' % (ptoken, token)
    ptoken = token
