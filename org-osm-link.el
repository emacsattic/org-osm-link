;;; org-osm-link.el --- Org-links to OSM maps

;; Copyright (c) 2010 Sebastian Rose, sebastian_rose gmx de
;; Authors:    Sebastian Rose, sebastian_rose gmx de

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Add a special Org link type, that, when clicked, opens an
;; SVG image of a certain track.

;; Example link:
;;   [[track:((12.0399212 14.919293)(32.12394 15.342345))FILE.svg][Track]]

;; An link's path consists of a list of coordinates and the
;; filename.  As usual, the description part (\"Track\" in
;; this case) is optional.
;; See the documentation for `osm-track-regexp' for a
;; description of the coordinates.

;; Installation
;; ------------

;; Make sure `osm-maps.el' and `org-osm-link.el' are in your
;; `load-path' and add this to your emacs setup:
;;     (require 'org-osm-link)


;;; Code:

(require 'org)
(require 'osm-maps)


;;; Custom Variables

(defcustom osm-org-export-html-format "<a href=\"%s\" target=\"_blank\">%s</a>"
  "Format for Org mode's XHTML export of OSM tracks.
The format should contain two place holders.  The first one
for the image's relative filename, the second one for the
link description."
  :group 'osm-maps
  :type  'string)


(defcustom osm-org-export-LaTeX-format
  "\\href{file://%F}{%d}"
  "Format used to export a track: link to LaTeX.
Possible replacements:
 \"%F\"  -  absolute path to the image
 \"%f\"  -  relative path to the image
 \"%d\"  -  link description or the image's relative path"
  :group 'osm-maps
  :type  'string)


(defcustom osm-org-image-viewer-function 'osm-org-show-track
  "Function to call if a track image should be shown.
The function is called with one argument: the absolute path
to the image file.

Here is an example for use with Gnome:
  (setq osm-org-image-viewer-function
        (lambda (file)
          (call-process \"gnome-open\" nil 0 nil file)))

More general:
  (setq osm-org-image-viewer-function
        (lambda (file)
          (call-process \"xdg-open\" nil 0 nil file)))

Open the track in a dedicated frame (emacs 25):
  (setq osm-org-image-viewer-function
        (lambda (file)
          (find-file-other-frame file)
          (set-window-dedicated-p (selected-window) t)))

Insert newline and an Org-mode image link and turn on `iimage-mode':
  (setq osm-org-image-viewer-function
        (lambda (file)
          (end-of-line)
          (newline-and-indent)
          (insert (format \"[[file:%s]]\"))
          (iimage-mode 1)))
"
  :group 'osm-maps
  :type  'function)


(defcustom osm-org-track-file-prefix ""
  "This will be used as prefix for track filenames.
You might or might not set this to an existing directory."
  :group 'osm-maps
  :type  'string)



;;; Functions

(defun osm-org-compose-link (&optional coords target-file-name description)
  "Store a link for a certain track.
The link is not validated currently but the .svg extension
is added as needed."
  (interactive)
  (let* ((crds (or coords (read-from-minibuffer "Coords: ")))
         (file (read-from-minibuffer
                "Filename: "
                (concat osm-org-track-file-prefix target-file-name)))
         (desc (or description (read-from-minibuffer "Desription: " file))))
    (unless (string-match "\\.svg$" file)
      (setq file (concat file ".svg")))
    (insert "[[track:" file crds "][" desc "]]")))


(defun osm-org-gpx-to-links (&optional gpx-file)
  "Read tracks from a GPX file and create osm org links interactively."
  (interactive "fGPX-File: ")
  (let ((tracks (osm-gpx-to-tracks gpx-file)))
    (message "tracks: %s" tracks)
    (mapc (lambda (trk)
            (let ((name (car trk))
                  (coords (cdr trk)))
              (message "coords: %s" coords)
              (osm-org-compose-link (format "'%s" coords) name)
              (insert "\n")))
          tracks)))



(defun osm-org-track-links-to-gpx (&optional gpx-file)
  "Put track found in org track link after point into a GPX
file.
If region is active, put all the tracks found in that region
into the GPX file.  If point is not inside a link, search for
the next org track link."
  (interactive)
  (let ((tracks '())
        reg beg end)
    (save-excursion
      (save-restriction
        (save-match-data
          (unwind-protect
              (progn
                (if (use-region-p)
                    (setq beg (region-beginning)
                          end (region-end)
                          reg t)
                  (if (not (search-forward-regexp
                            org-bracket-link-analytic-regexp))
                      (error "No \"track:\" link found.")
                    (setq beg (match-beginning 0)
                          end (match-end 0))))

                (narrow-to-region beg end)
                (goto-char beg)

                (while (search-forward-regexp
                        org-bracket-link-analytic-regexp end t)
                  (if (string= "track" (match-string 2))
                      (let ((desc (match-string 5))
                            (trk (osm-check-track (match-string 3))))
                        ;; Should we use the description part of the link instead?
                        (unless (member trk tracks)
                          (push trk tracks)))))

                (if (= 1 (length tracks))
                    (message "1 track found.")
                  (message "%s tracks found." (length tracks)))
                (osm-tracks-to-gpx tracks)
                )
            (widen)
            ))))))


(defun osm-org-show-track (target)
  (let ((file (file-name-nondirectory target)))
    (if (get-buffer file)
        (switch-to-buffer file)
      (find-file target))))


(defun osm-org-link-follow (path)
  "Follow the Org mode link when clicked."
  (let* ((trk (osm-check-track path))
         (coords (elt trk 1))
         (file (elt trk 0))
         (target file))
    (unless (file-exists-p target)
      ;; if no file exists, the name will be made
      ;; in osm-draw-track for us:
      (setq target (osm-draw-track coords file)))
    (funcall osm-org-image-viewer-function
             (file-truename target))))


(defun osm-org-link-export (path description format)
  "Export a track from Org files."
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


(defun osm-publish-map-for-html (plist file pubdir)
  "Publish existing maps for use with HTML files.
For this to work, you'll need to set up a special publishing
project in your `org-publish-project-alist'.  Here is an
example:

  (\"org-osm-maps\"
   :base-directory \"~/org/training/\"
   :publishing-directory \"~/public_html/org/tracks/\"
   :recursive t
   :base-extension \"svg\"
   :osm-cache-directory \"~/org/img/OSM/\"
   :publishing-function osm-publish-map-for-html)

:osm-cache-directory denotes the path to link the background tiles to.
The directory is not checked for existence.  The value is turned into
a path relative to the `file' parameter.

Example:
If your SVG track lives in ~/org/tracks/ and your background-tiles in
~/org/img/bg-tiles/, the tiles will be referenced by ../img/bg-tiles/.
Therefor, if you publish your SVG tracks to ~/public_html/org/tracks/,
there should be a directory (or smylink) ~/public_html/img/bg-tiles/."
  (let* ((target (concat
                  (file-name-as-directory pubdir)
                  (file-name-nondirectory file)))
         (old-path (concat
                    "file://"
                    (file-name-as-directory
                     (expand-file-name osm-default-cache-directory))))
         (new-path (concat
                    ""
                    (file-name-as-directory
                     (file-relative-name
                      (plist-get plist :osm-cache-directory)
                      (file-name-directory file)))))
         (ama auto-mode-alist)
         (mma magic-mode-alist))
    (make-directory pubdir t)
    (unwind-protect
        (progn
          (setq auto-mode-alist nil
                magic-mode-alist nil)
          (with-temp-file target
            (insert-file-contents file)
            (beginning-of-buffer)
            (replace-string old-path new-path)))
      ;; clean up forms:
      (setq auto-mode-alist  ama
            magic-mode-alist mma))))


;;; Finally install the link type

(org-add-link-type
 "track"
 'osm-org-link-follow
 'osm-org-link-export)


(provide 'org-osm-link)

;;; org-osm-link.el ends here
