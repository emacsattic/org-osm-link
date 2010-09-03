;;;
;; org-osm-link.el
;;
;; Author and Copyright (c) 2010 Sebastian Rose, Hannover, Germany, sebastian_rose@gmx.de
;;
;; Contact:
;;
;;    Phone: +49 (0) 173 - 83 93 4 17
;;    Email: sebastian_rose gmx de
;;
;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html
;;;

;; Add a special Org link type, that, when clicked, opens an
;; SVG image of a certain track.

;; Example link:
;;   [[track:((12.0399212 14.919293) (32.12394 15.342345))][Stupid track]]


(require 'org)
(require 'osm-maps)



(defun osm-org-link-follow (path)
  "Follow the Org mode link when clicked."
  (unless (string-match
           "(\\([[:space:]]*([0-9]+\\(.[0-9]*\\)?[[:space:]]+[0-9]+\\(.[0-9]*\\)?[[:space:]]*)\\)+[[:space:]]*)"
           path)
    (error "The link's path does not look like coords: %s" path))
  (let ((target (osm-draw-track
                 (read path))))
    (find-file-other-frame target)))

(defun osm-org-link-export (path desc format)
  "Export a track from Org files.
NOT YET IMPLEMENTED"
  (message "path: %s" path)
  (message "desc: %s" desc)
  (message "format: %s" format)
)


(defun osm-install-org-link-type ()
  "Install osm maps as link type in Org mode."
  (org-add-link-type
   "track"
   'osm-org-link-follow
   'osm-org-link-export))



(provide 'org-osm-link)
