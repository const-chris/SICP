#lang racket

#|
a) The problem asks for a procedure that takes the personnel file as an argument. Something like:
;|#
(define (get-record employee file)
  (get (file-id file) employee))

#|
An individual division's file would be an installation procedure that looks something like:
|#
(define records <some representation for the list of all records>)

(define (install-record-package)
  (define (record employee) <a procedure that gets the record for an employee>)
  
  (for-each (lambda (record)
              (put <division identifier (what should be returned by the file-id procedure above)>
                   employee (record employee))
              records)))

#|
However, this is not necessary if employees can be uniquely identified across all companies.
(This could be achieved by hashing some combination of the employee's name and the company's name, for instance.)
The get-record procedure can be very simple:
;|#
(define (get-record id) (get 'record id))
; or
(define (get-record employee-name file)
  (get 'record (hash employee-name (company-name file))))
#|
where hash generates the unique id for the employee from the employee's name and the company's name
(found presumably at the top of the file).
;|#

#|
All that is required for this to work is that all divisions provide a uniform interface to their records
(in this case, the employee id). An individual division's file should be an installation procedure that
looks something like:
|#
(define records <some representation for the list of all records>)

(define (install-record-package)
  (define (id record) <a procedure that gets the employee id for a record>)
  
  (for-each (lambda (record)
              (put 'record (id record) record)
              records)))
; or
(define (install-record-package)
  (define (name record) <a procedure that gets the employee id for a record>)
  
  (for-each (lambda (record)
              (put 'record (hash (name record) <company-name>) record)
              records)))


#|
b) get-salary can be implemented similarly:
;|#
(define (get-salary id) (get 'salary id))

#|
An individual division's file should be an installation procedure that looks something like:
;|#
(define (install-salary-package)
  (define (salary record) <a procedure that gets the salary for a record>)
  
  (for-each (lambda (record)
              (put 'salary (id record) (salary record))
              records)))


#|
c) With our system, if we wanted to be able to search by employee name, the simplest way would be to have each division
install an additional package like:
;|#
(define (install-name-package)
  (define (name record) <a procedure that gets the employee name for a record>)
  
  (for-each (lambda (record)
              (put 'record (name record) record)
              records)))

#|
Headquarters could then retrieve employee records by name with a procedure like:
;|#
(define (record-for-name name) (get 'record name))
#|
This works as long as employees have unique ids If not, we'd iterate through the company names searching each one using the
second version of the get-record procedure. If employee names are unique this is unnecessary, and if they aren't
(which is more likely), this will not identify a unique record.
;|#


#|
d) When Insatiable takes over a new company, no changes need be made to the Insatiable codebase. It need only require
the acquired company to provide packages like those above for accessing its records using Insatiable's interfaces.
;|#

