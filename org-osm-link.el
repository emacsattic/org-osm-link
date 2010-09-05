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
         (target file))

    (unless (file-exists-p target)
      ;; if no file exists, the name will be made
      ;; in osm-draw-track for us:
      (setq target (file-relative-name (osm-draw-track coords file)))
      (setq desc (or description target)))

    (setq file (file-name-nondirectory target))

    (if (get-buffer file)
        (switch-to-buffer file)
      (find-file target))))


(defun osm-org-link-export (path description format)
  "Export a track from Org files.
NOT YET IMPLEMENTED"
  (let* ((coords (osm-check-track path))
         (file (match-string 2 path))
         (target file)
         (desc (or description target)))

    (unless (file-exists-p target)
      ;; if no file exists, the name will be made
      ;; in osm-draw-track for us:
      (setq target (file-relative-name (osm-draw-track coords file)))
      (setq desc (or description target)))

    (cond
     ((eq format 'html)
      (format "<a href=\"%s\">%s</a>" (file-relative-name target) desc))
     ((eq format 'latex)
      ;; \includegraphics[width=10em]{nested-set_8c79dcf8fd4004ebfdf4d81910ad308c8b9f2ec8.png}
      desc)
     (t desc))))


(defun osm-install-org-link-type ()
  "Install osm maps as link type in Org mode."
  (org-add-link-type
   "track"
   'osm-org-link-follow
   'osm-org-link-export))


;; Finally install the link type:
(osm-install-org-link-type)



(provide 'org-osm-link)
