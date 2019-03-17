;;; fetch-core.el --- Core logic                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Erik Anderson

;; Author: Erik Anderson <erik@ebpa.link>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'fetch-utils)

(cl-defstruct (fetch-request (:constructor fetch-request-create)
                        :named)
  resource
  (method 'get)
  headers
  body)

(cl-defstruct fetch-response
  status-code
  headers
  body
  request
  raw)

(cl-defmethod fetch ((request fetch-request) &key callback promise)
  "Core function to make HTTP requests."
  (declare (indent 1))
  (fetch--do-setup)
  (cl-assert (not (and callback promise)) nil ":callback and :promise arguments are exclusive (may not be used simultaneously).")
  (let ((handler (-partial #'fetch--response-handler request))
        (url (fetch-request-resource request))
        (url-request-data (fetch-request-body request))
        (url-request-extra-headers (fetch-request-headers request))
        (url-request-method (upcase (symbol-name (fetch-request-method request))))
        (url-show-status nil))
    (cond
     (callback
      (url-retrieve url (-compose callback handler)))
     (promise
      (if (not (require 'promise nil t))
          (message "In order to use the fetch promise API, promise.el must be on the load-path.")
        (promise-chain
            (promise-new (lambda (resolve reject)
                           (condition-case err
                               (url-retrieve url (lambda (status)
                                                   (funcall resolve (list (current-buffer) status))))
                             (t
                              (funcall reject err)))))
          (then (-lambda ((response-buffer status))
                  (with-current-buffer response-buffer
                    (funcall handler status)))))))
     (t
      (with-current-buffer
          (url-retrieve-synchronously url)
        (funcall handler (car url-callback-arguments)))))))

(cl-defun fetch--response-handler (request status)
  "Internal function to build a `fetch-response' object from the response by the url library."
  (set-buffer-multibyte t)
  (make-fetch-response
   :status-code url-http-response-status
   :headers (fetch--parse-response-headers (buffer-substring-no-properties (point-min) url-http-end-of-headers))
   :body (when (not (eq url-http-end-of-headers (point-max)))
           (decode-coding-string (buffer-substring-no-properties url-http-end-of-headers (point-max)) 'utf-8))
   :request request
   :raw (current-buffer)))

(provide 'fetch-core)

;;; fetch-core.el ends here
