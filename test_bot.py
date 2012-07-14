#!/usr/bin/env python

import sys
import os
import urllib2
import urllib
import subprocess
import re
import time
from glob import glob

def get_online_score(mapfile, route):
    req = urllib2.Request(url='http://undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi',
                          data=urllib.urlencode({'mapfile': mapfile, 'route': route}))
    rep = urllib2.urlopen(req).read()
    board = re.search(r'<pre>(.*)</pre>', rep, re.M | re.S).groups()[0]
    score = re.search(r'Score: (-?[0-9]*)', rep).groups()[0]
    return (board, score)

def download_high_scores():
    req = urllib2.Request(url='http://undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi?standings=1')
    rep = urllib2.urlopen(req).read()
    scores = re.findall(r'<h4>(.+?)</h4><p>([^<]+)', rep)
    nscores = {}
    for (name, values) in scores:
        nscores[name] = values
    return nscores

def test_bot(mapfile):
    sp = subprocess.Popen(['./Main', os.path.join('..', 'task', mapfile + '.map')],
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    start = time.time()
    out, err = sp.communicate()
    end = time.time()
    lasted = end - start
    out = out.rsplit('\n\nRoute:\n', 1)[1].strip()
    route = re.search(r'^Used route\n(.+)$', out, re.M).group(1)
    onl_board, onl_score = get_online_score(mapfile, route)
    return lasted, onl_board, onl_score, route, out

def test_bot_on_all():
    os.chdir('src')
    subprocess.call(['ghc', 'Main.hs'])
    high_scores = download_high_scores()
    maps = [os.path.basename(m[:-4]) for m in glob('../task/*.map')]
    maps.sort()
    for mapfile in maps:
        print 'Testing %s...' % mapfile
        try:
            lasted, onl_board, onl_score, route, out = test_bot(mapfile)
        except Exception as e:
            print >>sys.stderr, e
            continue
        try:
            his = 'High scores: ' + high_scores[mapfile]
        except KeyError:
            his = 'No high scores.'
        print out
        print 'Took %f seconds' % lasted
        print 'Online score: %s' % onl_score
        print his
        print 'Online board: \n%s' % onl_board
        

if __name__ == '__main__':
    test_bot_on_all()

