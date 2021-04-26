;;; contract.el --- Racket-style higher-order contracts for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright © 2021 Langston Barrett <langston.barrett@gmail.com>

;; Author: Langston Barrett <langston.barrett@gmail.com>
;; URL: https://github.com/langston-barrett/contracts.el
;; Keywords: lisp
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (pkg-info "0.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides facilities for programming Emacs Lisp in the "design by
;; contract" style. It's implementation and interface are heavily inspired by
;; Racket's contracts.
;;
;; Example:
;;
;; TODO: Example here - from stdlib?
;;
;; Contracts for Built-in Functions:
;;
;; This library ships with a set of contracts for Emacs standard built-in
;; functions. While these are unlikely to catch bugs in Emacs itself, they might
;; be helpful in debugging and isolating errors in user programs or even
;; installed libraries.
;;
;; Performance:
;;
;; Contracts can be disabled by setting `contract-enable'.
;;
;; "Slow" contracts (those that are self-reported to take more than
;; constant-time) can be disabled by setting `contract-enable-slow'.
;;
;; Contracts are be written in the "late negative blame" by default, which can
;; improve performance if contracts are mostly attached to "exported" bindings
;; while avoided on "internal" bindings (see Racket's documentation for more on
;; this).
;;
;; Status:
;;
;; The following is a "wishlist" of features from the Racket contract system
;; that are not (yet?) implemented here:
;;
;; * Generating random data that satisfy a given contract.
;; * Contracts for vectors and other primitive types.
;; * "Collapsible" contracts.
;;
;; Implementation:
;;
;; In Racket, contracts are implemented as "chaperones" to functions. This
;; package only supports contracts on functions, and uses "advice" to emulate
;; chaperones in Emacs.
;;
;;; Code:

;;;; Imports

(require 'cl-lib)
(require 'subr-x)

;;;; Variables

;; TODO: Unused :-)
(defvar
  contract-enable
  t
  "If nil, then contracts will not be enforced.")

;; TODO: Unused :-)
(defvar
  contract-enable-slow
  t
  "If nil, then slow (non-constant-time) contracts will not be enforced.")

;;;; Utilities

(defun contract--assert (condition message)
  "Assert CONDITION, printing MESSAGE if it fails.

Used for bootstrapping (since we don't yet have contracts!)."
  (unless condition
    (error message)))

(defun contract--precond (condition func message)
  "Assert CONDITION as a precondition for FUNC, printing MESSAGE if it fails.

Used for bootstrapping (since we don't yet have contracts!)."
  (contract--assert condition (concat "Precondition of " func " violated: " message)))

(defun contract--precond-expect-string (func str)
  "Assert that STR is a string as a precondition of FUNC."
  (contract--precond (stringp str) func "Expected a string."))

(defun contract--precond-expect-function (func function)
  "Assert that FUNCTION is a function as a precondition of FUNC."
  (contract--precond (functionp function) func "Expected a function."))

;;;; Blame

(cl-defstruct
    (contract-blame
     (:constructor contract-make-blame))
  "See https://docs.racket-lang.org/reference/Building_New_Contract_Combinators.html#%28tech._blame._object%29."

  (context
   '()
   :type list                           ; of strings
   :documentation "A list of strings used in error messages")
  (format
   "Unexpected or incorrect value: %s"
   :type string
   :documentation "Format to use for error messages")
  (positive-party
   "*unknown*"
   :type string
   :documentation "The positive party/value producer/server")
  (negative-party
   "*unknown*"
   :type string
   :documentation "The negative party/value consumer/client"))

(defun contract--precond-expect-blame (func blame)
  "Assert that BLAME is a blame object as a precondition of FUNC."
  (contract--precond (contract-blame-p blame) func "Expected a blame object."))

(defun contract--blame-swap (blame)
  "Swaps the positive and negative party in this BLAME."
  (contract--precond-expect-blame "contract--blame-swap" blame)
  (setf
   (contract-blame-positive-party blame) (contract-blame-negative-party blame)
   (contract-blame-negative-party blame) (contract-blame-positive-party blame)))

(defun contract--add-negative-party (blame str)
  "Add a negative party STR to this BLAME."
  (contract--precond-expect-string "contract--add-negative-party" str)
  (setf (contract-blame-negative-party blame) str))

(defun contract--format-blame-error (blame value)
  "Format the error message for BLAME with value VALUE."
  (contract--precond-expect-blame "contract--format-blame-error" blame)
  (concat
   (format
    "Contract violation!\nMessage: %s\nBlaming: %s (assuming the contract is correct)"
    (format (contract-blame-format blame) value)
    (contract-blame-positive-party blame))
   (if (> 0 (length (contract-blame-context blame)))
       (format "\nIn: %s"
               (let ((sep "\n  "))
                 (concat sep (string-join (contract-blame-context blame) sep))))
     "")))

(defun contract-raise-blame-error (blame value)
  "Signal a contract violation with BLAME on value VALUE."
  (contract--precond-expect-blame "contract-raise-blame-error" blame)
  (error "%s" (contract--format-blame-error blame value)))

;;;; Contracts

;; TODO: Smart constructor that checks that if is-first-order is nil, so is
;; first-order.
(cl-defstruct
    (contract-contract
     (:constructor contract--make-contract))
  "Contracts."
  (name
   "anonymous-contract"
   :type string
   :documentation "Name of this contract.")
  (is-constant-time
   nil
   :type boolean
   :documentation "If this is t, then this contract is considered constant-time, and is enabled even when `contract-enable-slow' is nil.")
  (is-first-order
   nil
   :type boolean
   :documentation "Indicates whether this is a first-order contract, see also `first-order'.")
  (first-order
   (lambda (_) t)
   :type procedure
   :documentation "If this returns nil, then the overall contract is guaranteed to return nil. If it returns a non-nil value, then no guarantees are made about the overall value, unless `contract-contract-is-first-order' is non-nil, in which case the non-nil value is (must be!) the result of the overall contract.")
  (proj
   nil
   :type procedure
   :documentation "Late-negative projection function. From the Racket guide: \"Specifically, a late neg projection accepts a blame object without the negative blame information and then returns a function that accepts both the value to be contracted and the name of the negative party, in that order. The returned function then in turn returns the value with the contract.\""))

(defun contract--precond-expect-contract (func contract)
  "Assert that CONTRACT is a contract object as a precondition of FUNC."
  (contract--precond
   (contract-contract-p contract)
   func
   "Expected a contract object."))

(defun contract--make-late-neg-projection (proj)
  "Create a late-negative projection out of a simple projection PROJ."
  (contract--precond-expect-function "contract--make-late-neg-projection" proj)
  (lambda (positive-blame)
    (contract--precond-expect-blame
     "contract--make-late-neg-projection"
     positive-blame)
    (lambda (val neg-party)
      (contract--precond-expect-string
       "contract--make-late-neg-projection"
       neg-party)
      (contract--add-negative-party positive-blame neg-party)
      (funcall proj positive-blame val))))

(defmacro contract--coerce (val)
  "Coerce VAL to a contract.

Works in case VAL is:

* A contract.
* t or nil.

>> (contract-contract-p (contract--coerce nil))
=> t
>> (contract-contract-p (contract--coerce t))
=> t
>> (contract-contract-p (contract--coerce contract-nil-c))
=> t
>> (condition-case nil (contract--coerce 0) (error nil))
=> nil"
  (cond
   ((equal val 'nil) 'contract-nil-c)
   ((equal val 't) 'contract-t-c)
   (t `(if (contract-contract-p ,val)
           ,val
         (error "Couldn't coerce value to contract: %s" ,val)))))

(defun contract-apply (contract value blame)
  "Apply CONTRACT to VALUE with blame BLAME.

If this CONTRACT is first-order, this function will check that VALUE satisfies
it, and return VALUE. Otherwise, it will check that VALUE satisfies the
first-order part of CONTRACT (if any), and return a function that has the same
behavior as VALUE, except that it checks the higher-order CONTRACT before and
after applying VALUE.

>> (contract-apply contract-nil-c nil (contract-make-blame))
=> nil
>> (contract-apply contract-t-c t (contract-make-blame))
=> t"

  (contract--precond-expect-contract "contract-apply" contract)
  (contract--precond-expect-blame "contract-apply" blame)
  (funcall
   (funcall (contract-contract-proj contract) blame)
   value
   (contract-blame-negative-party blame)))

;;;; Simple Contracts

(defun contract--make-first-order-contract (func &optional name constant-time)
  "Create a first-order contract with name NAME and predicate FUNC."
  (contract--precond-expect-function "contract--make-first-order-contract" func)
  (contract--make-contract
   :name (if (eq nil name)
             "anonymous-first-order-contract"
           (contract--precond-expect-string
            "contract--make-first-order-contract"
            name))
   :is-constant-time constant-time
   :is-first-order t
   :first-order func
   :proj (contract--make-late-neg-projection
          (lambda (blame val)
            (if (funcall func val)
                val
              (contract-raise-blame-error blame val))))))

(defun contract-make-eq-contract (value)
  "Make a contract that asserts that a value is `eq' to VALUE."
  (contract--make-first-order-contract
   (lambda (val) (eq val value))
   (format "eq-to-%s" value)))

(defun contract-make-equal-contract (value)
  "Make a contract that asserts that a value is `equal' to VALUE."
  (contract--make-first-order-contract
   (lambda (val) (equal val value))
   (format "equal-to-%s" value)))

(defvar contract-any-c
  (contract--make-first-order-contract (lambda (_) t) "contract-any-c" t)
  "Contract that doesn't check anything.")

(defvar contract-nil-c
  (contract-make-eq-contract nil)
  "Contract that checks that a value is `eq' to nil.")

(defvar contract-t-c
  (contract-make-eq-contract t)
  "Contract that checks that a value is `eq' to t.")

(defvar contract-blame-c
  (contract--make-first-order-contract #'contract-blame-p "contract-blame-c" t)
  "Contract that checks that a value is a blame object.")

(defvar contract-contract-c
  (contract--make-first-order-contract
   #'contract-contract-p
   "contract-contract-c"
   t)
  "Contract that checks that a value is a contract.")

(defvar contract-function-c
  (contract--make-first-order-contract #'functionp "contract-function-c" t)
  "Contract that checks that a value is `functionp'.")

(defvar contract-nat-number-c
  (contract--make-first-order-contract #'natnump "contract-nat-number-c" t)
  "Contract that checks that a value is `natnump'.")

(defvar contract-string-c
  (contract--make-first-order-contract #'stringp "contract-string-c" t)
  "Contract that checks that a value is `stringp'.")

;;;; Combinators

(defun contract--are-constant-time (contracts)
  "Are CONTRACTS all constant-time?"
  (seq-reduce
   (lambda (acc c) (and acc (contract-contract-is-constant-time c)))
   contracts
   t))

(defun contract--are-first-order (contracts)
  "Are CONTRACTS all first-order?"
  (seq-reduce
   (lambda (acc c) (and acc (contract-contract-is-first-order c)))
   contracts
   t))

(defun contract-> (&rest contracts)
  "Construct a contract for a function out of CONTRACTS.

The last contract is the contract for the function's return value.

>> (contract-contract-p (contract-> contract-nil-c contract-nil-c))
=> t
>> (functionp (contract-apply (contract-> contract-nil-c contract-nil-c) #'identity (contract-make-blame)))
=> t
"
  (dolist (contract contracts)
    (contract--precond-expect-contract "contract->" contract))
  (let ((arg-contracts (seq-take contracts (- (length contracts) 1)))
        (ret-contract (car (last contracts))))
    (contract--make-contract
     :name "contract->"
     :is-constant-time (contract--are-constant-time contracts)
     :is-first-order nil
     :first-order nil
     :proj
     (lambda (pos-blame)
       ;; TODO: Add context to argument and return blames here.
       (lexical-let ((blame pos-blame))
         (lambda (function-value neg-party)
           (contract--add-negative-party blame neg-party)
           (lambda (&rest args)
             (unless (equal (length arg-contracts) (length args))
               (setf (contract-blame-format blame)
                     (concat
                      (format
                       "Wrong number of arguments to function:\nExpected: %s\nFound: %s"
                       (length arg-contracts)
                       (length args))
                      "\nArguments: %s"))
               (contract-raise-blame-error blame args))
             (contract--blame-swap blame) ; For arguments
             (dolist (n (number-sequence 0 (- (length arg-contracts) 1)))
               ;; TODO: Don't use nth here
               (contract-apply (nth n arg-contracts) (nth n args) blame))
             (contract--blame-swap blame) ; Back to normal for return value
             (let ((result (apply function-value args)))
               (contract-apply ret-contract result blame)))))))))

(defun contract--and-c-apply (blame value contracts)
  "Apply CONTRACTS to VALUE with BLAME."
  (pcase contracts
    (`(head . rest)
     (contract--and-c-apply (contract-apply head value blame) rest))
    (_ value)))

(defun contract-and-c (&rest contracts)
  "Construct a conjunction of the given CONTRACTS.

TODO: Use the first-order bits and link to Racket's documentation about them.

>> (contract-contract-p (contract-and-c contract-nil-c contract-nil-c))
=> t
>> (contract-contract-p (contract-and-c contract-nil-c contract-t-c))
=> t
>> (contract-apply (contract-and-c contract-nil-c contract-nil-c) nil (contract-make-blame))
=> nil"

  (dolist (contract contracts)
    (contract--precond-expect-contract "contract-and-c" contract))
  (contract--make-contract
   :name "contract-and-c"
   :is-constant-time (contract--are-constant-time contracts)
   :is-first-order (contract--are-first-order contracts)
   :first-order nil                     ; TODO
   :proj
   (lexical-let ((cs contracts))
     (lambda (pos-blame)
       ;; TODO: Call all the projections with pos-blame here?
       (lexical-let ((blame pos-blame))
         (lambda (value neg-party)
           (contract--add-negative-party blame neg-party)
           (contract--and-c-apply blame value cs)))))))

;; TODO: Handling &optional args, &rest args in contract->.
;; TODO: contract->i
;; TODO: contract-or-c
;; TODO: contract-not-c

;;;; Attaching Contracts

(defun contract--should-enable (contract)
  "Check if CONTRACT should be enabled.

Looks at `contract-enable' and `contract-enable-slow'."
  (contract--precond-expect-contract "contract--should-enable" contract)
  (not contract-enable)
  (and
   (not contract-enable-slow)
   (contract-contract-is-constant-time contract)))

(defun contract-advise (contract func)
  "Advise FUNC by applying CONTRACT to it.

Cautiously will not advice any function with pre-existing advice."
  ;; NOTE: No preconditions are used because this function is dogfooded just
  ;; below its definition.
  (unless (or (advice--p (advice--symbol-function func))
              (not (contract--should-enable contract)))
    (lexical-let* ((name (symbol-name func))
                   (blame (contract-make-blame
                           :positive-party name
                           :negative-party (concat "caller of " name)))
                   (contracted (contract-apply contract func blame)))
      (advice-add func :override contracted))))

;; Dogfooding:
(defconst
  contract--contract-advise-contract
  (contract->
   contract-contract-c
   contract-function-c
   contract-blame-c
   contract-nil-c))
(defvar contract--contract-advise-advised nil)
(unless contract--contract-advise-advised
  (contract-advise
   contract--contract-advise-contract
   #'contract-advise)
  (setq contract--contract-advise-advised t))

;; TODO: define/contract

;;;; Contracts for Built-Ins

(defun contract-apply-contracts-for-built-ins ()
  "Apply contracts to Emacs built-in functions via advise.

CAUTION: This function may mess up your Emacs! Do not enable it e.g. in your
init file. This library is considered unstable, and this is mostly a mechanism
for testing it."
  (contract-advise
   (contract-> contract-any-c contract-any-c)
   'identity)
  (contract-advise
   (contract-> contract-nat-number-c)
   'buffer-size)
  ;; Also: "This is 1, unless narrowing (a buffer restriction) is in effect."
  (contract-advise
   (contract-> contract-nat-number-c)
   #'point-min)
  ;; Also: "This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
  ;; is in effect, in which case it is less."
  (contract-advise
   (contract-> contract-nat-number-c)
   #'point-max))

(provide 'contract)
;;; contract.el ends here
