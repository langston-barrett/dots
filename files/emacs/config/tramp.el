;; -*- lexical-binding: t; -*-
;;; TRAMP

(defconst
  my/hosts
  '("big"))

(defconst
  my/host-username-alist
  '(("big" . "langston")))

(defun my/choose-host ()
  (interactive)
  (completing-read "Host:" my/hosts))

;;;; Generic Functions

(my/load "contract.el")

(defun my/get-tramp-prefix (path)
  "Get just the \"TRAMP part\" of a path.

>> (my/get-tramp-prefix \"/var\")
=> \"\"
>> (my/get-tramp-prefix \"/ssh:host:/var\")
=> \"/ssh:host:\"
"
  (string-remove-suffix (tramp-file-local-name path) path))

(contract-advise
 (contract-> contract-string-c contract-string-c)
 'my/get-tramp-prefix)

(defun my/drop-last (str) (reverse (seq-drop (reverse str) 1)))

(defun my/tramp-get-hops (path)
  "Get a list of all the TRAMP hops in this path

>> (my/tramp-get-hops \"/ssh:host1|ssh:user@host2:/path\")
=> (\"ssh:host1\" \"ssh:user@host2\")
"
  (assert (tramp-tramp-file-p path))
  (split-string (my/drop-last (seq-drop (my/get-tramp-prefix path) 1)) "\|"))

(defun my/tramp-get-host (hop)
  "Get just the hostname from a HOP.

>> (my/tramp-get-host \"ssh:host1\")
=> \"host1\"
>> (my/tramp-get-host \"ssh:user@host1\")
=> \"host1\"
"
  (car (last (split-string hop "[:@]"))))

(defun my/tramp-mash-hops (hops)
  "Mash a list of TRAMP hops together.

>> (my/tramp-mash-hops '(\"ssh:host1\" \"ssh:user@host2\"))
=> \"/ssh:host1|ssh:user@host2:/path\"
"
  (concat "/" (mapconcat 'identity hops "|") ":"))

;; https://www.emacswiki.org/emacs/TrampMode#h5o-16
(defun my/tramp-add-sudo (path)
  "Add a sudo to the front of a path, potentially making it multi-hop

>> (my/tramp-add-sudo \"/var\")
=> \"/sudo::/var\"

>> (my/tramp-add-sudo \"/ssh:host:/var\")
=> \"/sudo::/var\"
"
  (if (tramp-tramp-file-p path)
      ;; Hard case: It's multi-hop.
      (let* ((hops (my/tramp-get-hops path))
             (last-hop (car (last hops)))
             (last-hop-host (my/tramp-get-host last-hop)))
        (my/tramp-mash-hops
         (seq-concatenate 'list hops (list (concat "sudo:" last-hop-host)))))

      ;; Easy case: It's not multi-hop, so just add the sudo prefix.
      (concat "/sudo::" path)))

