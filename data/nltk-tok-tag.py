#!/usr/bin/python2.7

from nltk import word_tokenize, sent_tokenize, pos_tag
import sys,codecs

sys.stdout = codecs.getwriter('utf8')(sys.stdout)
sys.stdin = codecs.getreader('utf8')(sys.stdin)

f = codecs.open(sys.argv[1],'r', 'utf-8')

tokf = codecs.open(sys.argv[1][0:-3] + 'tok', 'w', 'utf-8')
tagf = codecs.open(sys.argv[1][0:-3] + 'tag', 'w', 'utf-8')

for sent in sent_tokenize(f.read()):
    tokens = pos_tag(word_tokenize(sent))
    for (tok, tag) in tokens:
        tokf.write(tok + '\n')
        tagf.write(tag + '\n')
    tokf.write('\n')
    tagf.write('\n')
    sys.stderr.write('.')

tokf.close()
tagf.close()
