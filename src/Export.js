/* global exports */
"use strict";

/* Canvas Donwload */
exports.exportAs = function(canvas) {
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
};
