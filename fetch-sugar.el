;;; fetch-sugar.el --- Convenience methods for fetch      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Erik Anderson

;; Author: Erik Anderson <erik@ebpa.link>
;; Keywords: tools

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

;; TODO: consider async patterns for `fetch-parse-json' and `fetch-parse-html' methods.

;;; Code:

(require 'fetch-core)

(cl-defmethod fetch (request &rest args)
  "Core function to make HTTP requests."
  (-let* ((request (if (listp request)
                       (apply #'fetch-request-create request)
                     (fetch-request-create :resource request))))
    (apply #'fetch request args)))

(cl-defmethod fetch-parse-json ((response fetch-response))
  "Read the JSON object contained in the body of RESPONSE and return it.

See: `json-read-from-string'."
  (when (fetch-response-body response)
    (json-read-from-string (fetch-response-body response))))

(cl-defmethod fetch-parse-json ((request fetch-request) &rest args)
  "Read the JSON object contained in the body of RESPONSE and return it.

See: `json-read-from-string'."
  (fetch-parse-json (apply #'fetch request args)))

(cl-defmethod fetch-parse-json (resource)
  "Fetch RESOURCE and return the JSON object contained in the body of the response.

See: `json-read-from-string'."
  (fetch-parse-json (fetch (fetch-request-create :resource resource :headers '(("Accept" . "application/json"))))))

(cl-defmethod fetch-parse-json ((response-promise promise-class))
  "Read the JSON object contained in the body of the response value that RESPONSE-PROMISE resolves to and return it.

See: `json-read-from-string'."
  (promise-chain response-promise
    (then (lambda (response)
            (fetch-parse-json response)))))

(cl-defmethod fetch-parse-html ((response fetch-response))
  "Read the HTML object contained in the body of RESPONSE and return it.

See: `libxml-parse-html-region'."
  (when (fetch-response-body response)
    (with-temp-buffer
      (insert (fetch-response-body response))
      (goto-char (point-min))
      (libxml-parse-html-region (point-min) (point-max)))))

(cl-defmethod fetch-parse-html ((request fetch-request))
  "Read the HTML object contained in the body of RESPONSE and return it.

See: `libxml-parse-html-region'."
  (fetch-parse-html (fetch request)))

(cl-defmethod fetch-parse-html (resource)
  "Read the HTML object contained in the body of RESPONSE and return it.

See: `libxml-parse-html-region'."
  (fetch-parse-html (fetch-request-create :resource resource)))

(cl-defmethod fetch-parse-xml ((response fetch-response))
  "Read the XML object contained in the body of RESPONSE and return it.

See: `libxml-parse-xml-region'."
  (when (fetch-response-body response)
    (with-temp-buffer
      (insert (fetch-response-body response))
      (goto-char (point-min))
      (libxml-parse-xml-region (point-min) (point-max)))))

(provide 'fetch-sugar)

;;; fetch-sugar.el ends here
