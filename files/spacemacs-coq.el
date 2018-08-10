(defun company-coq--interactive-list (lst prompt &optional duplicates max-num)
  "Interactively collect a list or set with up to MAX-NUM elements.
If DUPLICATES is t then the user is allowed to select items from the list multiple times"
  (let ((result     nil)
        (lemma-name "")
        (candidates `(,@lst "[done]")))
    (while (and (> (length candidates) 1) ;; "[done]" is always in there
                (if (null max-num) t (< (length result) max-num)))
      (let* ((key    (if (bound-and-true-p helm-mode) "C-RET" "C-j"))
             (prompt (concat prompt (format " (%s when done) " key)))
             (input  (completing-read prompt candidates nil t)))
        (if (member input '("" "[done]"))
            (setq candidates nil)
          (when (not duplicates) (setq candidates (remove input candidates)))
          (push input result))))
    (reverse result)))

(defun company-coq--insert-hypothesis-tactic (tactic prompt &optional max-num after)
  "Insert a tactic whose arguments are space-separated hypotheses from the
current context. The hypotheses are selected (and inserted) interactively and
without duplication."
  (let ((hyps (company-coq--interactive-list company-coq--hyp-names prompt nil max-num)))
    (insert tactic)
    (dolist (hyp hyps)
      (insert (concat " " hyp)))
    (insert (if (null after) "." after))
    (indent-according-to-mode)))

(defun company-coq--insert-single-hypothesis-tactic (tactic &optional prompt after)
  "Insert a tactic that expects a single tactic"
  (company-coq--insert-hypothesis-tactic tactic (if (null prompt) "Select hypothesis" prompt) 1 after))

(defun company-coq-insert-in ()
  "Insert 'in' followed by a hypothesis."
  (interactive)
  (company-coq--insert-single-hypothesis-tactic "in" nil ""))

(defun company-coq-insert-clear ()
  "Insert the 'clear' tactic followed by some number of hypotheses."
  (interactive)
    (company-coq--insert-hypothesis-tactic "clear" "Select hypotheses to clear"))

(defun company-coq-insert-contradict ()
  "Insert the 'contradict' tactic followed by a hypothesis."
  (interactive)
  (company-coq--insert-single-hypothesis-tactic "contradict"))

(defun company-coq-insert-clearbody ()
  "Insert the 'clearbody' tactic followed by a hypothesis."
  (interactive)
  (company-coq--insert-single-hypothesis-tactic "clearbody"))

(defun company-coq-insert-eexact ()
  "Insert the 'eexact' tactic followed by a hypothesis."
  (interactive)
  (company-coq--insert-single-hypothesis-tactic "eexact"))

(defun company-coq-insert-exact ()
  "Insert the 'exact' tactic followed by a hypothesis."
  (interactive)
  (company-coq--insert-single-hypothesis-tactic "exact"))

(defun company-coq-insert-generalize ()
  "Insert the 'generalize' tactic followed by some number of hypotheses."
  (interactive)
  (company-coq--insert-hypothesis-tactic "generalize" "Select hypotheses to generalize"))

(defun company-coq-insert-inversion ()
  "Insert the 'inversion' tactic followed a hypothesis."
  (interactive)
  (company-coq--insert-single-hypothesis-tactic "inversion" "Select hypothesis to invert"))

(defun company-coq-insert-rename ()
  "Insert the 'rename' tactic followed a hypothesis.
TODO(@siddharthist): Insert a hole?"
  (interactive)
  (company-coq--insert-hypothesis-tactic "rename" "Select hypothesis to rename" 1 " into ."))

(defun company-coq-insert-revert ()
  "Insert the 'revert' tactic followed by a hypothesis."
  (interactive)
  (company-coq--insert-single-hypothesis-tactic "revert" "Select hypothesis to revert"))

(defconst company-coq--lemma-from-goal-prompt2 "Select hypotheses to keep")

(defun company-coq-lemma-from-goal-interact2 ()
  "Interactively collect a lemma name and hypothesis names."
  (let ((lemma-name "")
        (candidates (car (company-coq-run-then-collect-hypotheses-and-goal "Show"))))
      (while (string-equal lemma-name "")
        (setq lemma-name (read-string "Lemma name? ")))
      (list lemma-name
            (company-coq--interactive-list
             company-coq--hyp-names company-coq--lemma-from-goal-prompt2))))
