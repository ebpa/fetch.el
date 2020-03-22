(cl-defstruct fetch-response
  status-code
  headers
  body
  request
  raw)

(provide 'fetch-response)
