#!/usr/bin/env python
import urllib2, urllib, re, curses, time

def validate(mapfile, route):
    req = urllib2.Request(url  = 'http://undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi',
                          data = urllib.urlencode({'mapfile': mapfile, 'route': route}))
    rep = urllib2.urlopen(req).read()
    board = re.search('<pre>(.*)</pre>', rep, re.M | re.S).groups()[0]
    score = re.search('Score: (-?[0-9]*)', rep).groups()[0]
    return (board, score)

#init
scr = curses.initscr()
scr.keypad(1)
curses.noecho()
curses.cbreak()

mapfile = 'flood2'
route = ''

try:
    while True:
        board, score = validate(mapfile, route)
        scr.addstr(0, 0, board + 'Score: ' + score + '\n' + route, 0)
        scr.refresh()
        c = scr.getch()
        if   c == ord('q'):
            break
        elif c == curses.KEY_BACKSPACE:
            if len(route) > 0:
                route = route[:-1]
        elif c == curses.KEY_LEFT:
            route += 'L'
        elif c == curses.KEY_RIGHT:
            route += 'R'
        elif c == curses.KEY_UP:
            route += 'U'
        elif c == curses.KEY_DOWN:
            route += 'D'
except Exception, e:
    print e
except KeyboardInterrupt:
    pass

#exit
curses.nocbreak()
scr.keypad(0)
curses.echo()
curses.endwin()
