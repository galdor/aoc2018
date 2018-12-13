
(defsystem "aoc2018"
    :description "Advent of Code 2018"
    :author "Nicolas Martyanoff <khaelin@gmail.com>"
    :licence "ISC"
    :depends-on ("cl-ppcre")
    :pathname "src"
    :serial t
    :components ((:file "aoc")
                 (:file "01")
                 (:file "02")
                 (:file "03")
                 (:file "04")
                 (:file "05")))
