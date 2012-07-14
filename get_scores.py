#!/usr/bin/env python

import sys
import urllib2
import urllib
import re

def get_score(mapfile, route):
    req = urllib2.Request(url='http://undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi',
                          data=urllib.urlencode({'mapfile': mapfile, 'route': route}))
    rep = urllib2.urlopen(req).read()
    board = re.search(r'<pre>(.*)</pre>', rep, re.M | re.S).groups()[0]
    score = re.search(r'Score: (-?[0-9]*)', rep).groups()[0]
    return (board, score)

def get_high_scores():
    req = urllib2.Request(url='http://undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi?standings=1')
    rep = urllib2.urlopen(req).read()
    scores = re.findall(r'<h4>(.+?)</h4><p>([^<]+)', rep)
    nscores = {}
    for (name, values) in scores:
        nscores[name] = values
    return nscores

if __name__ == '__main__':
    print(get_high_scores())
#    board, score = get_score(*sys.argv[1:3])
