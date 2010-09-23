;;; osm-maps.el --- Use maps from openstreetmap.org

;; Copyright (c) 2010 Sebastian Rose, sebastian_rose gmx de
;; Authors:    Sebastian Rose, sebastian_rose gmx de

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;; This file is NOT part of GNU Emacs.

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;; Commentary:

;; Create SVG images from coordinates.  Currently uses PNG images from
;; openstreetmap.org as background images.

;; Installation
;; ------------

;; Make sure `osm-maps.el' is in your `load-path' and add this to your
;; emacs setup:
;;     (require 'osm-maps)

;; Customization
;; -------------

;;  M-x customize-group RET osm-maps RET


;;; Code:

(defgroup osm-maps nil
  "Render Tracks as SVG images."
  :version "24"
  :group   'multimedia)

(defcustom osm-default-zoom 15
  "Default zoom level."
  :group 'osm-maps
  :type  'integer)

(defcustom osm-do-cache nil
  "Should the background tiles be stored on disc?
The cache is never emptied and might grow quite big. Tiles
will be stored in `osm-default-cache-directory'."
  :group 'osm-maps
  :type  'boolean)

(defcustom osm-default-cache-directory "~/.emacs.d/osm"
  "Directory for the osm tile tree.
Background tiles will be stored here on demand if you set
`osm-do-cache' to a non-nil value.
E.g  \"~/.emacs.d/osm\"."
  :group 'osm-maps
  :type  'directory)

(defcustom osm-margin 5
  "Minimum distance of trackpoints from the edge in pixels.
This will be multiplied with the zoom level.  Hence 0 means
no margin."
  :group 'osm-maps
  :type  'integer)

(defconst osmMaxLatitude 85.05112877
  "Polar areas with abs(latitude) bigger then 85.05112877 are clipped off.")

(defvar osm-hosts (list "a" "b" "c")
  "List of hosts to get tiles from.
We balance the load on OSM servers.")

(defvar osm-host 0
  "Current offset in osm-hosts.")

(defconst osm-track-regexp              ; DO NOT CHANGE without looking in osm-check-track!
  "^'?\\((\\(?:[[:space:]]*([0-9]+\\(?:.[0-9]*\\)?[[:space:]]+[0-9]+\\(?:.[0-9]*\\)?[[:space:]]*)\\)+[[:space:]]*)\\)[[:space:]]*\\(\\(?:[^)[:space:]]\\)*\\)?[[:space:]]*"
  "Match a valid track, i.e. a list of lists of coords.
 (match-string 1) will hold the track data, (match-string 2)
will hold the rest of the string with all whitespace removed.

See also: `osm-check-track'.")


;;; Functions

(defun osm-yx-to-xy-lol (lol)
  "Some application return lists with the values swapped:
 ((y1 x1) (y2 x2) ... ).  This functions turnes them into
proper ((x1 y1) (x2 y2) ... ) lists.  All osm functions
expect the latter format."
  (setq lol (mapcar
             'reverse
             lol)))


(defun osm-new-tile (x y z)
  "Return a new tile as used throughout this file.
A tile is a list (x y z url path). Either url or path is nil,
depending on wether the tile is cached on disk or not."
  (let ((cfile (osm-cache-file-name x y z)))
    (if (file-exists-p cfile)
        (list x y z nil cfile)
      (list x y z (osm-url-for-tile x y z) nil))))


(defun osm-url-for-tile (x y z)
  "Build the URL for a tile with coords X Y and Z."
  (setq osm-host (% (+ 1 osm-host) 3))
  (concat "http://"
          (nth osm-host osm-hosts)
          ".tile.openstreetmap.org/"
          (number-to-string z) "/"
          (number-to-string x) "/"
          (number-to-string y) ".png"))


(defun osm-cache-file-name (x y z)
  "Build the file name of a tiles cache file."
  (concat (file-name-as-directory osm-default-cache-directory)
          (number-to-string z) "/"
          (number-to-string x) "/"
          (number-to-string y) ".png"))


(defun osm-area-file-name (x y w h &optional zoom)
  "Build the file name of the background for an area denoted
by X, Y, W, H and ZOOM (all integers) sans extension.
X and Y denote the upper left corner in OSM tiles,
W and H the width and height in tiles respectively."
  (let ((z (or zoom osm-default-zoom)))
    (format "%s%d/area-%d-%d-%d-%d"
            (file-name-as-directory osm-default-cache-directory)
            z x y w h)))


(defun osm-fetch-tile (x y z)
  "Fetch a tile from an OpenStreetmap server.
Return the absolut path to the image file or nil if
`osm-do-cache' is nil."
  (if osm-do-cache
      (let* ((target-file (osm-cache-file-name x y z))
             (target-dir (file-name-directory target-file)))
        (make-directory target-dir t)
        (when (not (file-directory-p target-dir))
          (error "OSM: Cannot create directory: %s" target-dir))
        (unless (file-exists-p target-file)
          (url-copy-file (osm-url-for-tile x y z) target-file t))
        target-file)
    nil))


(defun osm-fetch-area (min-x min-y max-x max-y &optional zoom)
  "Fetch tiles for an entire area.
MIN-X, MIN-Y, MAX-X, MAX-Y and ZOOM must be positive integer values."
  (let ((x min-x) ;; FIXME: check for valid input
        (y min-y)   ;; FIXME: check for valid input
        (z (or zoom osm-default-zoom))
        (fetched 0))
    (while (<= y max-y)
      (setq x min-x)
      (while (<= x max-x)
        (osm-fetch-tile x y z)
        (setq fetched (+ 1 fetched))
        (setq x (+ 1 x)))
      (setq y (+ 1 y)))
    (message "Successfully fetched %s tiles." fetched)))


(defun osm-zoom (val z &optional new-z)
  "Zoom a value.  VAL could be a tile number (x or y) or a Pixel.
If NEW-Z is provided, assume VAL is in Z and zoom to NEW-Z.
Otherwise zoom by Z levels, where negative Z zooms out.
The return value is always an integer."
  (if new-z
      (lsh val (- new-z z))
    (lsh val z)))


(defun osm-compose-area-background (min-x min-y max-x max-y &optional zoom)
  "Compose a background PNG image (i.e. a map) for an area.
MIN-X, MIN-Y, MAX-X, MAX-Y and ZOOM must be positive integer values.
They denote the row and column minimum and maximum values of the area
to fetch.
If FILE is supplied, write the result to file FILE.
Returns the path to the resulting image PNG file.

Montage needs imageMagick to be installed, so that montage is found in
$PATH.

Deprecated."
  (let* ((x min-x) ;; FIXME: check for valid input
         (y min-y) ;; FIXME: check for valid input
         (w (+ 1 (- max-x min-x))) ;; FIXME: check for valid input
         (h (+ 1 (- max-y min-y)))   ;; FIXME: check for valid input
         (z (or zoom osm-default-zoom))
         (command (executable-find "montage"))
         (target (concat (osm-area-file-name x y max-x max-y z) ".png"))
         )
    (unless (and command (file-executable-p command))
      (error "%s%s%s" "Trying to execute " command "! Is it installed?"))
    (osm-fetch-area min-x min-y max-x max-y zoom)
    (while (<= y max-y)
      (setq x min-x)
      (while (<= x max-x)
        (setq command (concat command " " (osm-cache-file-name x y z)))
        (setq x (+ 1 x)))
      (setq y (+ 1 y)))
    (message
     "%s" (format "%s -geometry +0+0 -tile %dx%d %s" command w h target))
    (shell-command
     (format "%s -geometry +0+0 -tile %dx%d %s" command w h target))
    target
    ))


(defun osm-longitude-to-x (longitude zoom)
  "Return the x value in pixels from the date line for
a certain latitude."
;; TODO: cache the basic values for each zoom factor.

  (let* ((z (or zoom osm-default-zoom))
         (ntiles (lsh 1 z))
         ;; circumference of our pixel world:
         ;; (circ (lsh 1 (+ z 8)))
         (circ (lsh ntiles 8))
         (greenwich (lsh circ -1))
         (pixel (floor (+ greenwich (* (/ circ 360) longitude)))))
    pixel))


(defun osm-latitude-to-y (latitude zoom)
  "Return the y value in pixels (offset) from the northpole and the
y-number of the tile that pixel is found on as '(pixel tileY)."
  ;; TODO: cache the basic values for each zoom factor.
  ;; Polar areas with abs(latitude) bigger then 85.05112877 are clipped off.
  (when (> (abs latitude) osmMaxLatitude)
    (if (> latitude 0)
        (progn
          (message "Latitude %s out of bounds! Using %s, i.e. the maximum."
                   latitude osmMaxLatitude)
          (setq latitude osmMaxLatitude))
      (message "Latitude %s out of bounds! Using %s, i.e. the minimum."
               latitude (- osmMaxLatitude))
      (setq latitude (- osmMaxLatitude))))

  (let* ((equator (lsh 128 z)) ; BEFORE we change the zoom
         (z (or (- zoom 1) (- osm-default-zoom)))
         (r (/ equator pi))
         (north (> latitude 0)) ; Negative latitudes are on the southern hemisphere!
         (lat (degrees-to-radians (abs latitude)))
         ret)
    (setq ret
          (* (/ r 2.0)
             (log
              (/ (+ 1.0 (sin lat))
                 (- 1.0 (sin lat))))))
    (if north
        (floor (- equator ret))
      (floor (+ equator ret)))
    ))


(defun osm-column-for-x (x zoom)
  "Return the column index that contains X (a pixel)."
  (lsh x -8))


(defun osm-row-for-y (y zoom)
  "Return the column index that contains Y (a pixel)."
  (lsh y -8))


(defun osm-draw-track (points &optional file-name zoom)
  "Draw a track.
POINTS is a list of `(longitude latitude elevation)' elements.
The elevation is optional (and not yet implemented).

If FILE-NAME is nil, create a file in `default-directory'.
The basename of that file will be made up from the x and y
values of the background tiles.

If FILE-NAME is a string, use that as file name.  If absolute,
use the provided path, else create the image relative to the
current `default-directory'."
  (let* ((minx 360.0)
         (maxx 0.0)
         (miny 90.0)
         (maxy 0.0)
         (z (or zoom osm-default-zoom))
         (margin (* z osm-margin))
         (mtop 0) (mleft 0)              ; margin differences
         (defdir default-directory)
         target                          ; File to write the imate to
         cmin cmax rmin rmax             ; rows and columns
         x y w h                         ; helpers
         tmp
         )
    (mapc
     (lambda (p)
       (and (< (car p) minx)  (setq minx (car p)))
       (and (> (car p) maxx)  (setq maxx (car p)))
       (and (< (abs (cadr p)) miny) (setq miny (cadr p)))
       (and (> (abs (cadr p)) maxy) (setq maxy (cadr p))))
     points)
    (message "minx maxx miny maxy: %s %s %s %s" minx maxx miny maxy)
    ;; Get the map x and y values for GPS-values:
    (setq minx (osm-longitude-to-x minx z))
    (setq maxx (osm-longitude-to-x maxx z))
    (setq miny (osm-latitude-to-y  miny z))
    (setq maxy (osm-latitude-to-y  maxy z))
    (message "minx maxx miny maxy: %s %s %s %s" minx maxx miny maxy)
    ;; Swap y values (southern hemisphere yield negative latitudes):
    (when (< maxy miny)
      (setq tmp maxy)
      (setq maxy miny)
      (setq miny tmp))
    ;; Add the margin:
    (setq minx (- minx (* osm-margin z)))
    (setq maxx (+ maxx (* osm-margin z)))
    (setq miny (- miny (* osm-margin z)))
    (setq maxy (+ maxy (* osm-margin z)))
    ;; Now get the tiles we need (rows and columns):
    (setq cmin (osm-column-for-x minx z))
    (setq cmax (osm-column-for-x maxx z))
    (setq rmin (osm-row-for-y miny z))
    (setq rmax (osm-row-for-y maxy z))
    (message "cmin cmax rmin rmax: %s %s %s %s" cmin cmax rmin rmax)

    (setq target
          (if (stringp file-name)
              (if (file-name-absolute-p file-name)
                  file-name
                (concat defdir file-name))
            (concat
             (if osm-do-cache
                 (osm-area-file-name cmin rmin cmax rmax z)
               (concat defdir
                       (file-name-nondirectory
                        (osm-area-file-name cmin rmin cmax rmax z))))
             ".svg")))

    (unless (file-directory-p (file-name-directory target))
      (make-directory (file-name-directory target) t))

    ;; Ensure we have the tiles:
    (osm-fetch-area cmin rmin cmax rmax z)
    ;; calculate width and height:
    (setq w (- maxx minx)
          h (- maxy miny))
    ;; Calculate the margines.  The two margins now are positive integers to
    ;; substract from all x and y coords.
    (setq mtop  (- miny (lsh rmin 8))
          mleft (- minx (lsh cmin 8)))
    ;; Create SVG file and draw the track on it:
    (let ((ama auto-mode-alist))
      (unwind-protect
          (progn
            (setq auto-mode-alist nil)
            (with-temp-buffer
              (write-file target t)
              (fundamental-mode)
              (insert
               "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>\n"
               "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
               "<svg\n"
               "	xmlns:svg=\"http://www.w3.org/2000/svg\"\n"
               "	xmlns=\"http://www.w3.org/2000/svg\"\n"
               "	xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n"
               "	version=\"1.1\"\n"
               "	width=\""   (format "%d" w)  "\"\n"
               "	height=\""  (format "%d" h)  "\">\n"
               " <g id=\"layer1\">\n")
              (setq y rmin)
              (while (<= y rmax)
                (setq x cmin)
                (while (<= x cmax)
                  (setq tmp
                        (if osm-do-cache
                            (concat "file://"
                                    (expand-file-name (osm-fetch-tile x y z)))
                          (osm-url-for-tile x y z)))
                  (insert
                   "  <image\n"
                   "		xlink:href=\"" tmp "\"\n"
                   (format "		x=\"%d\"\n" (- (lsh (- x cmin) 8) mleft))
                   (format "		y=\"%d\"\n" (- (lsh (- y rmin) 8) mtop))
                   "		width=\"256\"\n"
                   "		height=\"256\" />\n")
                  (setq x (+ x 1)))
                (setq y (+ y 1)))

              ;; Draw the track
              (insert
               "  <path\n"
               "		d=\"M")
              (setq tmp t)
              (mapc
               (lambda (c)
                 (insert
                  (format " %f,%f"
                          ;; We loose 6 X-Pixels somewhere, that's why we add 6 here:
                          (+ (- (- (osm-longitude-to-x (car c) z)  (lsh cmin 8)) mleft) 6)
                          (- (- (osm-latitude-to-y (cadr  c) z) (lsh rmin 8)) mtop)))
                 (when tmp
                   (setq tmp nil)
                   (insert " L")))
               points)
              (insert "\"\n" ;; end of d value.
                      "		style=\"fill:none;"
                      "stroke:#ff0000;stroke-width:"
                      (format "%d" (if (<= z 10) 1.5 (+ 1.5 (* 0.6 (- z 10)))))
                      ";stroke-linecap:round;stroke-linejoin:round;"
                      "stroke-miterlimit:4;stroke-opacity:0.65;stroke-dasharray:none\" />\n")

              (insert
               " </g>\n"
               "</svg>\n")
              ;; We copy the file, to circumvent the delay produced by image mode, if
              ;; tiles on the server are used.
              (save-buffer 0)
              )                         ; end with-temp-buffer
            )                           ; end progn
        (setq auto-mode-alist ama))     ; end unwind-protect
      )                                 ; end of let
    target
    ))


(defun osm-draw-tracks (track-list &optional zoom)
  "Draw tracks."
  (let ((z (or zoom osm-default-zoom))
        (image-list nil))
    (mapc (lambda (track)
            (push
             (osm-draw-track (car track) (cadr track) z)
             image-list))
          track-list)
    image-list))


(defun osm-check-track (track)
  "Check, if TRACK is valid.
If the TRACK is invalid, throw an error.
Currently only strings and lists are checked.  Lists are
expected to be lists of lists,  each sub-list a list of two
float values.  Strings are expected to be a valid elisp
representation of such a lol.

This function returns the lol as elisp object suitable as
parameter for `osm-draw-track' and similar.

`match-string' 1 will hold the track data as string,
`match-string' 2 will hold the rest of the string with all
whitespace removed.

See `osm-track-regexp' for more information."
  (let ((errstr "The track does not look like valid coords: %s")
        (str (if (stringp track)
                 track
               (format "%s" track))))
    (if (string-match osm-track-regexp str)
        (read (match-string 1 str))
      (error errstr track))))



(provide 'osm-maps)
;;; osm-maps.el ends here
