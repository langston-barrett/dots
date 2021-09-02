;; -*- lexical-binding: t; -*-
;;; TRAMP

(require 'contract)

(defconst
  my/hosts
  '("localhost"
    "big"
    "small"))

;; TODO Rewrite with contract-one-of-c
(defconst my/host-c
  (contract-predicate
   (lambda (host) (seq-find (lambda (item) (equal item host)) my/hosts))
   "Expected a host, but got %s"
   'my/host-c
   nil
   t))

(defconst
  my/users
  '("langston"
    "siddharthist"))

;; TODO Rewrite with contract-one-of-c
(defconst my/user-c
  (contract-predicate
   (lambda (user) (seq-find (lambda (item) (equal item user)) my/users))
   "Expected a user, but got %s"
   'my/user-c
   nil
   t))

(defconst
  my/host-username-alist
  '(("localhost" . "siddharthist")
    ("big" . "langston")
    ("small" . "siddharthist")))

(contract-defun
 my/get-user-for-host
 (host)
 :contract (contract-> my/host-c my/user-c)
 (alist-get host my/host-username-alist nil nil #'equal))

(defun my/choose-host ()
  (interactive)
  (completing-read "Host:" my/hosts))

;;;; Generic Functions

(eval-when-compile
  (require 'cl-lib)
  (cl-defstruct
      (my/method-user-host (:constructor my/make-method-user-host))
    (method
     nil
     :type string
     :readonly)
    (user
     nil
     :type string
     :readonly)
    (host
     nil
     :type string
     :readonly)))

(defconst my/method-user-host-c
  (contract-predicate
   #'my/method-user-host-p
   "Expected a method-user-host struct, but got %s"
   'my/method-user-host-c
   nil
   t)
  "Contract that checks that a value is a method-user-host struct.")

(contract-defun
 my/choose-method-user-host
 ()
 :contract (contract-> my/method-user-host-c)
 (interactive)
 (let ((host (completing-read "Host:" my/hosts)))
   (my/make-method-user-host
    :host host
    :method (if (equal host "localhost") "sudo" "ssh")
    :user (my/get-user-for-host host))))

(contract-defun
 my/squash-method-user-host
 (method-user-host)
 :contract (contract-> my/method-user-host-c contract-string-c)
 (concat
  "/"
  (my/method-user-host-method method-user-host)
  ":"
  (my/method-user-host-user method-user-host)
  "@"
  (my/method-user-host-host method-user-host)))


(defun my/get-tramp-prefix (path)
  "Get just the \"TRAMP part\" of a path.

>> (my/get-tramp-prefix \"/var\")
=> \"\"
>> (my/get-tramp-prefix \"/ssh:host:/var\")
=> \"/ssh:host:\"
"
  (string-remove-suffix (tramp-file-local-name path) path))

(contract-defun
 my/drop-last (str)
 :contract (contract-> contract-string-c contract-string-c)
 (reverse (seq-drop (reverse str) 1)))

(defconst my/tramp-file-c
  (contract-predicate
   #'tramp-tramp-file-p
   "expected a tramp file object, but got %s"
   'my/tramp-file-c
   nil
   t)
  "Contract that checks that a value is a tramp file.")

(contract-defun
 my/tramp-get-hops (path)
 :contract (contract-> my/tramp-file-c contract-sequence-c)
 "Get a list of all the TRAMP hops in this path

>> (my/tramp-get-hops \"/ssh:host1|ssh:user@host2:/path\")
=> (\"ssh:host1\" \"ssh:user@host2\")
"
 (split-string (my/drop-last (seq-drop (my/get-tramp-prefix path) 1)) "\|"))

(contract-defun
 my/tramp-parse-hop (hop)
 :contract (contract-> contract-string-c contract-any-c)
 "Parse HOP into a `method-user-host' struct."
 (pcase (split-string hop "[:@]")
   (`(,method . (,user . ,host))
    (my/make-method-user-host :method method :user user :host host))))

(contract-defun
 my/tramp-get-host (hop)
 :contract
 (contract->d
  (in contract-string-c)
  (contract-substring-c in))
 "Get just the hostname from a HOP.

>> (my/tramp-get-host \"ssh:host1\")
=> \"host1\"
>> (my/tramp-get-host \"ssh:user@host1\")
=> \"host1\"
"
 (car (last (split-string hop "[:@]"))))

(contract-defun
 my/tramp-mash-hops (hops)
 :contract
 (contract-> contract-sequence-c contract-string-c)
 "Mash a list of TRAMP hops together.

>> (my/tramp-mash-hops '(\"ssh:host1\" \"ssh:user@host2\"))
=> \"/ssh:host1|ssh:user@host2:\"
"
 (concat "/" (mapconcat 'identity hops "|") ":"))

;; https://www.emacswiki.org/emacs/TrampMode#h5o-16
(defun my/tramp-add-sudo-or-sg (sudo-or-sg username path)
  (if (tramp-tramp-file-p path)
      ;; Hard case: It's multi-hop.
      (let* ((hops (my/tramp-get-hops path))
             (last-hop (car (last hops)))
             (last-hop-host (my/tramp-get-host last-hop)))
        (concat
         (my/tramp-mash-hops
          (seq-concatenate
           'list
           hops
           (list (concat sudo-or-sg ":" username "@" last-hop-host))))
         (tramp-file-local-name path)))

    ;; Easy case: It's not multi-hop, so just add the sudo prefix.
    (concat "/" sudo-or-sg ":" username "@localhost:" path)))

(contract-defun
 my/tramp-add-sudo (path)
 :contract
 (contract-> contract-string-c contract-string-c)
 "Add a sudo to the front of a path, potentially making it multi-hop

>> (my/tramp-add-sudo \"/var\")
=> \"/sudo:root@localhost:/var\"

>> (my/tramp-add-sudo \"/ssh:host:/var\")
=> \"/ssh:host|sudo:root@host:/var\""
 (my/tramp-add-sudo-or-sg "sudo" "root" path))

;; TODO: Doesn't yet work with MATE - TRAMP bug?
(defun my/tramp-add-sudo-g (path group)
  (my/tramp-add-sudo-or-sg "sg" group path))

(contract-defun
 my/tramp-rm-sudo (path)
 :contract
 (contract-> contract-string-c contract-string-c)
 "Take a PATH with a sudo in it and remove the sudo.

>> (my/tramp-rm-sudo \"/var\")
=> \"/var\"
>> (my/tramp-rm-sudo \"/sudo::/var\")
=> \"/var\"
>> (my/tramp-rm-sudo \"/ssh:host|sudo:root@host:/var\")
=> \"/ssh:host:/var\"
>> (my/tramp-rm-sudo \"/sudo:root@host:/var\")
=> \"/ssh:host:/var\""

 (if (tramp-tramp-file-p path)
     ;; sudo can only appear in the last hop.
     (let* ((hops (my/tramp-get-hops path))
            (last-hop (car (last hops)))
            (new-hops
             (if (s-contains-p "sudo" last-hop)
                 (let* ((parsed-last-hop (my/tramp-parse-hop last-hop))
                        (last-hop-host (car (my/method-user-host-host parsed-last-hop))))
                   (if (and (equal (length hops) 1)
                            (not (equal nil last-hop-host))
                            (not (equal "localhost" last-hop-host)))
                       (list (concat "ssh:" last-hop-host))
                     (butlast hops)))
               hops))
            (local (tramp-file-local-name path)))
       (if (not new-hops)
           local
         (concat (my/tramp-mash-hops new-hops) local)))

   ;; Easy case: It's not a TRAMP path.
   path))

(defun my/reopen-file-no-sudo ()
  (interactive)
  (find-file-existing (my/tramp-rm-sudo (buffer-file-name))))
