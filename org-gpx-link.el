;;; org-gpx-link.el --- Org-links to GPX files

;; Copyright (c) 2010 Sebastian Rose, sebastian_rose gmx de
;; Authors:    Sebastian Rose, sebastian_rose gmx de

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Add a special Org link type, that, when clicked, renders a SVG from a GPX
;; file.

;; Example link:
;;   [[gpx:FILE.GPX][DESCRIPTION]]

;; An link's path consists of a path to a GPX file and the description.  As
;; usual, the description part (\"DESCRIPTION\" in this case) is optional.

;; Installation
;; ------------

;; Make sure `osm-maps.el' `org-gpx-link.el' are in your `load-path' and add
;; this to your emacs setup: (require 'org-gpx-link)


;;; Code:

(require 'org)
(require 'osm-maps)



;;; Functions

(defun osm-org-gpx-link-follow (path)
  "Follow the Org mode link when clicked."
  (let* ((tracks (osm-gpx-to-tracks path))
         (image-list (osm-draw-tracks tracks)))
    ;; FIXME: A *.gpx file might contain thousands of tracks.
    ;; Possible strategies are:
    ;; 1. Insert org-osm-links for each track and let org-osm-link take care
    ;;    of export
    ;; 2. Insert Images (SVGs - not a very good idea unless we add SVG export
    ;;    to org-mode)
    ;; 3. Do nothing.  Just generate the SVG files and download the background tiles.
    (funcall osm-org-image-viewer-function
             (file-truename (car image-list)))))


(defun osm-org-link-export (path description format)
  "TODO."
  (error "osm-org-link-export: Not implemented yet.")
  (let* ((trk (osm-check-track path))
         (coords (elt trk 1))
         (file (car trk))
         (target (file-relative-name file))
         (desc (or description target)))
    (unless (file-exists-p target)
      ;; if no file exists, the name will be made
      ;; in osm-draw-track for us:
      (setq target (file-relative-name (osm-draw-track coords file)))
      (setq desc (or description target)))
    (cond
     ((eq format 'html)
      (format osm-org-export-html-format
              (file-relative-name target) desc))
     ((eq format 'latex)
      (org-replace-escapes
       osm-org-export-LaTeX-format
       (list (cons "%f" target)
             (cons "%F" (expand-file-name target))
             (cons "%d" desc))))
     (t
      (file-relative-name target)))))


;;; Finally install the link type

(org-add-link-type
 "gpx"
 'osm-org-gpx-link-follow
 'osm-org-gpx-link-export)


(provide 'org-gpx-link)

;;; org-gpx-link.el ends here
