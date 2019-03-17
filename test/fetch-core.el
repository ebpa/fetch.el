(require 'buttercup)

(describe "fetch"
  (it "must be able to determine a host")
  (it "can operate synchronously"
    (expect (fetch-response-p (fetch "http://localhost"))))
  (it "can operate using a callback")
  (it "returns a promise by default"))
