chord-editor
============


Provide a simple UI in order to generate chord diagrams like this:

![chord-diagram-guitar](https://github.com/newlandsvalley/chord-builder/blob/master/F_guitar.png)

The idea comes after a late night conversation over a glass of whisky with felespiller [Sturla Eide](https://no.wikipedia.org/wiki/Sturla_Eide) who needs something similar for his guitar teaching. He also needed similar patterns for bass guitar:

![chord-diagram-bass](https://github.com/newlandsvalley/chord-builder/blob/master/F_bass.png)

(Note that, in general, you don't play an out-and-out chord on a bass guitar - you tend to play an arpeggio through the chord which in turn means that more than one fret on the same string needs to be fingered.  The black circle represents a primary fret position and the gray square a secondary one.)

and also piano:

![chord-diagram-piano](https://github.com/newlandsvalley/chord-builder/blob/master/F_piano.png)

Try it out [here](http://www.tradtunedb.org.uk:8603/).


To build
--------

    bower install
    npm run build

and then navigate in your browser to /dist.  The browser must support web-audio if you wish to hear the chord.
