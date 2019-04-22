chord-editor
============

__Work In Progress__

Provide a simple UI in order to generate chord diagrams like these:

![chord-diagram](https://github.com/newlandsvalley/chord-builder/blob/master/chord-diagram.jpg).

The idea comes after a late night conversation over a glass of whisky with felespiller [Sturla Eide](https://no.wikipedia.org/wiki/Sturla_Eide) who needs something similar for his guitar teaching.

Try it out [here](http://www.tradtunedb.org.uk:8603/).

Initial Design Aims
-------------------

*  We won't number the fingers.
*  Possibly we'll omit the cross that represents an unplayed string.
*  Simple mouse-click interface for editing fingering.
*  Display only the first 6 (or so) frets.
*  Export as JPG (i.e. with opaque background).
*  Export as PNG (i.e. with transparent background).
*  Export as EPS?
*  Allow images to be scaled on export.
*  Allow the chord to be played.

To build
--------

    bower install
    npm run build

and then navigate in your browser to /dist.  The browser must support web-audio if you wish to hear the chord.
