;;; fetch-utils.el         -*- lexical-binding: t; -*-

(defun fetch--do-setup ()
  ""
  ;; When this function has already been called, then it is a
  ;; no-op.  Otherwise it sets `url-registered-auth-schemes' among
  ;; other things.  If we didn't ensure that it has been run, then
  ;; `url-retrieve-synchronously' would do it, which would cause
  ;; the value that we let-bind below to be overwritten, and the
  ;; "default" value to be lost outside the let-binding.
  (url-do-setup))

(defun fetch--parse-response-headers (string)
  "Return an alist of headers for response in the current buffer."
  ;; based on ghub--handle-response-headers
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (forward-line 1)
    (let (headers)
      (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                                (point-max) t)
        (push (cons (match-string 1)
                    (match-string 2))
              headers))
      (setq headers (nreverse headers))
      headers)))

(defun twitter--url-encode-params (params)
  (mapconcat (lambda (param)
               (pcase-let ((`(,key . ,val) param))
                 (concat (url-hexify-string (symbol-name key)) "="
                         (if (integerp val)
                             (number-to-string val)
                           (url-hexify-string val)))))
             params "&"))

(cl-defun fetch--parse-json-fn (&key (array-type json-array-type)
                                (object-type json-object-type)
                                (array-type json-array-type)
                                (key-type json-key-type)
                                (false json-false)
                                (null json-null))
  "JSON parsing function factory."
  (lambda (json)
    (let* ((json-array-type array-type)
           (json-object-type object-type)
           (json-array-type array-type)
           (json-key-type key-type)
           (json-false false)
           (json-null null))
      (json-read-from-string json))))

(provide 'fetch-utils)
