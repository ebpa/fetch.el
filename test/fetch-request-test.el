(require 'buttercup)

(describe "fetch-request-create"
  (it "accpts a url as the input parameter"
    (let* ((url "/foo"))
      (expect (fetch-request-url (fetch-request-create url)) :to-be url)))

  (it "accpts a fetch-request object as the input parameter"
    (let* ((url "/bar")
           (request (fetch-request-create url)))
      (expect (fetch-request-url (fetch-request-create request)) :to-be url)))

  (it "accepts keyed slot parameters"
    (let* ((url "/baz")
           (method 'post)
           (headers '(("flim" . "flam")))
           (body "Body")
           (request (fetch-request-create url
                                     :method method
                                     :headers headers
                                     :body body)))
      (expect (fetch-request-url request) :to-be url)
      (expect (fetch-request-method request) :to-be method)
      (expect (fetch-request-headers request) :to-be headers)
      (expect (fetch-request-body request) :to-be body)))

  (it "merges supplied headers with supplied request object"))
