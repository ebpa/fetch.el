(require 'buttercup)
(require 'fetch)

(describe "fetch"
  (it "must be able to determine a host")
  (it "can operate synchronously"
    (expect (fetch-response-p (fetch "https://httpstat.us/200"))))
  (it "can operate using a callback")
  (it "can return a promise"))
