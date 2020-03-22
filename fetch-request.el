;;; fetch-request.el --- Request datastructure

;;; Commentary:
;; Request object similar to https://developer.mozilla.org/en-US/docs/Web/API/Request.

;;; Code:

(cl-defstruct (fetch-request (:constructor fetch-request-create
                                      (input &key (url (if (fetch-request-p input)
                                                           (fetch-request-url input)
                                                         input))
                                             (credentials "omit")
                                             (method (if (fetch-request-p input)
                                                         (fetch-request-method input)
                                                       'get))
                                             (headers (when (fetch-request-p input)
                                                        (fetch-request-headers input)))
                                             (body (when (fetch-request-p input)
                                                     (fetch-request-body input)))))
                        :named)
  url
  method
  headers
  body
  credentials)

(provide 'fetch-request)
;;; fetch-request.el ends here
