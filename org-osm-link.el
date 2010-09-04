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

;; Installation

;; Add this to your emacs setup:
;;
;;   (require 'org-osm-link)
;;   (osm-install-org-link-type)




(require 'org)
(require 'osm-maps)




(defun osm-org-link-store-link ()
  "Store a link for a certain track."
  (let (link desc)
    (setq link (org-make-link
                "track:"
                (read-from-minibuffer "Coords: ")))
    (setq desc (read-from-minibuffer "Desription: "))
    (org-store-link-props :type "track"
                          :link link
                          :desc desc)
    link))

(defun osm-org-link-follow (path)
  "Follow the Org mode link when clicked."
  (let* ((coords (osm-check-track path))
         (file (match-string 2 path))
         (target (osm-draw-track coords file))
         (file (file-name-nondirectory target)))
    (if (get-buffer file)
        (switch-to-buffer file)
      (find-file target))))

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
