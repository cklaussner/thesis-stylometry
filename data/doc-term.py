#!/usr/bin/python

""" This script calculates and outputs a document-term matrix for the
    documents given in as arguments. The documents must include one token
    per line. Empty lines are converted to a special term/token '###'.
    If the variable 'freqlimit' is set (see below), only the terms 
    whose overall frequency is over this limit is included in the 
    output. If the variable 'ranklimit' is set only the first
    'ranklimit' terms are included in the output. Both 'freqlimit' and
    the 'ranklimit' are checked against the rank/frequency in the
    complete document set.

    The default separator is TAB. The output is written to the
    standard output, so you want to redirect the output. For example,

        ./doc-term.py *.tok > doc-term.csv

"""

import sys,codecs

sys.stdout = codecs.getwriter('utf8')(sys.stdout)
sys.stdin = codecs.getreader('utf8')(sys.stdin)

sep = "\t"
# ranklimit = 50 # only print most frequent N terms
# freqlimit = 5  # only print terms whose frequency is more than N

doc_term = {}
terms = {}     # frequency of terms over all documents
sys.stderr.write("reading documents...")
for fname in sys.argv[1:]:
    sys.stderr.write(" %s," % (fname))
    f = codecs.open(fname,'r', 'utf-8')
    freq = {}
    for line in f:
        term = line.strip()
        if not term:    # sentence boundary
            term = "###"
        if term in freq:
            freq[term] = freq[term] + 1
            terms[term] = terms[term] + 1
        else:
            freq[term] = 1
            if term in terms:
                terms[term] = terms[term] + 1
            else:
                terms[term] = 1
    doc_term[fname] = freq
sys.stderr.write(" done.\n\n\n")

docs = doc_term.keys()

# #sys.stdout.write("term")
# for d in docs:
#     sys.stdout.write("%s%s" % (sep, d))
# sys.stdout.write("\n\n")

termslist = sorted(terms, key=terms.get, reverse=True)
i = 0
for t in termslist:
    sys.stdout.write("%s%s" % (sep, t))
    if 'ranklimit' in vars() and i >= ranklimit:
        break
    if 'freqlimit' in vars() and  terms[t] <= freqlimit:
        break
    i = i + 1
sys.stdout.write("\n")

for d in docs:
    sys.stdout.write("%s" % (d))
    i = 0
    for t in termslist:
        if t in doc_term[d]:
            sys.stdout.write("%s%d" % (sep, doc_term[d][t]))
        else:
            sys.stdout.write("%s0" % (sep))
        if 'ranklimit' in vars() and i >= ranklimit:
            break
        if 'freqlimit' in vars() and  terms[t] <= freqlimit:
            break
        i = i + 1
    sys.stdout.write("\n")
