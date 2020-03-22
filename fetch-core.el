;;; fetch-core.el --- Core logic                          -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'promise)
(require 'fetch-request)
(require 'fetch-response)
(require 'fetch-utils)

(cl-defmethod fetch ((request fetch-request) &key callback promise)
  "Core function to make HTTP requests."
  (declare (indent 1))
  (fetch--do-setup)
  (cl-assert (not (and callback promise)) nil ":callback and :promise arguments are exclusive (may not be used simultaneously).")
  (let ((handler (-partial #'fetch--response-handler request))
        (url (fetch-request-url request))
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
          (cl-letf (((symbol-function 'url-http-create-request) `(lambda () (fetch-build-raw-request ,request))))
            (url-retrieve-synchronously url))
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
(upcase (symbol-name (fetch-request-method (fetch-request-create "/"))))

(defun fetch-build-raw-request (request)
  "Create an HTTP request string from REQUEST."
  (cl-assert (fetch-request-p request))
  (-let* ((url-obj (url-generic-parse-url (fetch-request-url request)))
          (body (fetch-request-body request))
          raw-request)
    (setq raw-request
          (concat (upcase (symbol-name (fetch-request-method request))) " "
                  (-let* (((path . query) (url-path-and-query url-obj)))
                    (url-http--encode-string
                     (concat path (when query (concat "?" query)))))
                  " HTTP/" url-http-version "\r\n"
                  ;; (maybe) Try to keep the connection open
                  "Connection: " (if (not url-http-attempt-keepalives)
                                     "close" "keep-alive") "\r\n"
                  ;; TODO: handle port (see url-http-create-request)
                  (format "Host: %s\r\n"
                          (url-http--encode-string (puny-encode-domain (url-host url-obj))))
                  ;; User agent
                  (url-http-user-agent-string)
                  ;; TODO:
                  ;; (when (equal (fetch-request-credentials request)
                  ;;              "include")
                  ;;   (url-http--encode-string
                  ;;    (url-cookie-generate-header-lines
                  ;;     host real-fname
                  ;; Length of data
                  (apply
                   #'concat
                   (mapcar
                    (-lambda ((header . value))
                      (concat header ": " value "\r\n"))
                    (fetch-request-headers request)))
                  (if body
                      (concat
                       "Content-length: " (number-to-string
                                           (length body))
                       "\r\n"))
                  ;; End request
                  "\r\n"
                  ;; Any data
                  ;; CLEANUP: This probably shouldn't be done here?
                  (url-http--encode-string body)))
    ;; CLEANUP: removeme
    (setq my/request raw-request)
    ;; Bug#23750
    (unless (= (string-bytes raw-request)
               (length raw-request))
      (error "Multibyte text in HTTP request: %s" raw-request))
    (url-http-debug "Request is: \n%s" raw-request)
    raw-request))

;; TODO: Replace url-http method (partially done)
;; (defun fetch-http (request callback)
;;   ""
;;   (let* ((url-obj (url-generic-parse-url (fetch-request-url request)))
;;          (raw-request (fetch-build-raw-request request))
;;          (url-current-object url-obj)
;;          (connection (url-http-find-free-connection (url-host url)
;;                                                     (url-port url)
;;                                                     gateway-method))
;; 	 (buffer (or retry-buffer
;; 		     (generate-new-buffer
;;                       (format " *http %s:%d*" (url-host url) (url-port url))))))
;;     (if (not connection)
;; 	;; Failed to open the connection for some reason
;; 	(progn
;; 	  (kill-buffer buffer)
;; 	  (setq buffer nil)
;;           (error "Could not create connection to %s:%d" (url-host url)
;;                  (url-port url)))
;;       (with-current-buffer buffer
;; 	(mm-disable-multibyte)
;; 	(setq url-current-object url-obj
;; 	      mode-line-format "%b [%s]")

;; 	(dolist (var '(url-http-end-of-headers
;; 		       url-http-content-type
;; 		       url-http-content-length
;; 		       url-http-transfer-encoding
;; 		       url-http-after-change-function
;; 		       url-http-response-version
;; 		       url-http-response-status
;; 		       url-http-chunked-length
;; 		       url-http-chunked-counter
;; 		       url-http-chunked-start
;; 		       url-callback-function
;; 		       url-callback-arguments
;; 		       url-show-status
;; 		       url-http-process
;; 		       url-http-method
;; 		       url-http-extra-headers
;; 		       url-http-noninteractive
;; 		       url-http-data
;; 		       url-http-target-url
;; 		       url-http-no-retry
;; 		       url-http-connection-opened
;;                        url-mime-accept-string
;; 		       url-http-proxy
;;                        url-http-referer))
;; 	  (set (make-local-variable var) nil))

;; 	(setq url-http-method (or url-request-method "GET")
;; 	      url-http-extra-headers url-request-extra-headers
;; 	      url-http-noninteractive url-request-noninteractive
;; 	      url-http-data url-request-data
;; 	      url-http-process connection
;; 	      url-http-chunked-length nil
;; 	      url-http-chunked-start nil
;; 	      url-http-chunked-counter 0
;; 	      url-callback-function callback
;; 	      url-callback-arguments cbargs
;; 	      url-http-after-change-function 'url-http-wait-for-headers-change-function
;; 	      url-http-target-url url-current-object
;; 	      url-http-no-retry retry-buffer
;; 	      url-http-connection-opened nil
;;               url-mime-accept-string mime-accept-string
;; 	      url-http-proxy url-using-proxy
;;               url-http-referer referer)

;; 	(set-process-buffer connection buffer)
;; 	(set-process-filter connection 'url-http-generic-filter)
;; 	(pcase (process-status connection)
;;           ('connect
;;            ;; Asynchronous connection
;;            (set-process-sentinel connection 'url-http-async-sentinel))
;;           ('failed
;;            ;; Asynchronous connection failed
;;            (error "Could not create connection to %s:%d" (url-host url)
;;                   (url-port url)))
;;           (_
;;            (if (and url-http-proxy (string= "https"
;;                                             (url-type url-current-object)))
;;                (url-https-proxy-connect connection)
;;              (set-process-sentinel connection
;;                                    'url-http-end-of-document-sentinel)
;;              (process-send-string connection (url-http-create-request)))))))
;;     buffer))

(provide 'fetch-core)

;;; fetch-core.el ends here
