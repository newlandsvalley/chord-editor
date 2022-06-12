/* global exports */
"use strict";

/* Canvas Donwload */
export function exportAs(canvas) {
  return function (filename) {
    return function (mimeType) {
      return function() {

        /// create an "off-screen" anchor tag
        var lnk = document.createElement('a'), e;

        /// the key here is to set the download attribute of the a tag
        lnk.download = filename;

        /// convert canvas content to data-uri for link. When download
        /// attribute is set the content pointed to by link will be
        /// pushed as "download" in HTML5 capable browsers
        /// lnk.href = canvas.toDataURL("image/png;base64");
        lnk.href = canvas.toDataURL(mimeType);

        /// create a "fake" click-event to trigger the download
        if (document.createEvent) {
          e = document.createEvent("MouseEvents");
          e.initMouseEvent("click", true, true, window,
                           0, 0, 0, 0, 0, false, false, false,
                           false, 0, null);

          lnk.dispatchEvent(e);
        } else if (lnk.fireEvent) {
          lnk.fireEvent("onclick");
        }
      };
    };
  };
}

export function scaleCanvas(canvas) {
  return function (factor) {
    return function() {
      // create a new canvas just to hold the download image
      var downloadCanvas = document.createElement('canvas');
      var width = canvas.width * factor;
      var height = canvas.height * factor;
      // get the context of the new canvas
      var downloadCtx = downloadCanvas.getContext('2d');
      // set the dimensions of the download canvas
      downloadCanvas.width = width;
      downloadCanvas.height = height;

      // copy the canvas from source to destination, respecting
      // the source and target dimensions
      downloadCtx.drawImage (canvas, 0, 0, canvas.width, canvas.height,
                              0, 0, width, height);
      return downloadCanvas;
    };
  };
}
